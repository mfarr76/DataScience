#install.packages(c("purrr", "caret", "broom", "ggplot2"))
#install.packages(c("corrplot", "caret"))

rm(list = ls())
library(dplyr)
library(ggplot2)
library(broom)
library(caret)
#library(MASS)
#detach(package:MASS)
library(corrplot)





karnes <- read.csv("Karnes_data.csv", stringsAsFactors = FALSE)
karnes$DateProductionStart <- as.Date(karnes$DateProductionStart, "%m/%d/%Y")

###--------------------------added from kaggle site----------------------------
##https://www.kaggle.com/jiashenliu/updated-xgboost-with-parameter-tuning?scriptVersionId=362252

Num_NA <- sapply( karnes,function( y ) length( which( is.na ( y ) == TRUE )))
(NA_Count<- data.frame( Item = colnames ( karnes ) ,Count = Num_NA ))


##check for NA
a <- is.na(karnes)
## Set to 0 if NA
karnes[a] <-0

##conver cols to numeric if they are interger
karnes <- mutate_if(karnes, is.integer, as.numeric)


correlate <- cor( karnes_numeric, use = "everything" )
corrplot(correlate, method="circle", type="lower",  sig.level = 0.01, insig = "blank")
corrplot(correlate, method = "ellipse")
cor.test(karnes1$CUM12Liquid, karnes1$LBS_FT_CLEAN)
summary(lm(IPLiquid ~ PerfIntervalGross_CLEAN, data = karnes1))



###-----------------------------------------------------------------------------
##filter high sand and change zeros to NAs





high_sand <- karnes %>%
  na.omit() %>%
  filter(LBS_FT_CLEAN > 1500) %>%
  select(OperatorName, PerfIntervalGross_CLEAN, LBS_FT_CLEAN, BBL_FT_CLEAN, 
         Spacing_Avg, GOR_6M, IPLiquid ,CUM6Liquid, CUM12Liquid)

high_sand$Category <- as.factor(ifelse(high_sand$CUM12Liquid >= quantile(high_sand$CUM12Liquid, 
                                                               p = 0.90), "HIGH",
                             ifelse(high_sand$CUM12Liquid < quantile(high_sand$CUM12Liquid, p = 0.90)
                                    & high_sand$CUM12Liquid >= median(high_sand$CUM12Liquid), "MEDIUM", "LOW")))


Zero_NA <- sapply( high_sand, function( y ) length( which( y == 0)))
(Zero_Count<- data.frame( Item=colnames ( high_sand ) ,Count=Zero_NA ))

a0 <- high_sand == 0
high_sand[a0] <- NA 
high_sand[,-1] <- sapply(high_sand[,-1], as.numeric)


summary(high_sand)




#----------------------------------------------------------------------------
# Anova test


ggplot(high_sand, aes(x = OperatorName, fill = Category)) + 
  geom_bar(position = "fill") + 
  ylab("Relative frequencies")


(operator_anova <- aov(CUM12Liquid ~ OperatorName, data = high_sand))
summary(operator_anova)

(category_anova <- aov(CUM12Liquid ~ Category, data = high_sand))
summary(category_anova)


ggplot(high_sand, aes(x = OperatorName, y = CUM12Liquid)) + 
  geom_boxplot()
  #geom_boxplot(aes(colour = OperatorName))


ggplot(high_sand, aes(x = Category, y = CUM12Liquid)) + 
  geom_boxplot()




#----------------------------------------------------------------------------



mod_1 <- lm(CUM12Liquid ~ LBS_FT_CLEAN, data = high_sand)
summary(mod_1)
p_1 <- predict(mod_1, high_sand)
error_1 <- p_1 - high_sand$CUM12Liquid
(RMSE_1 <- sqrt(mean(error_1^2, na.rm = TRUE))) #potiential overfit

##-----------------------------
mod_1
summary(mod_1)
set.seed(1)
LBS_FT.c. <- as.data.frame(scale(high_sand$LBS_FT_CLEAN, center = TRUE, scale = FALSE))
names(LBS_FT.c.)[1] <- "LBS_FT.c."
high_sand <- cbind(high_sand, LBS_FT.c.)
  
mod_sand <- lm(CUM12Liquid ~ LBS_FT.c., data = high_sand)
summary(mod_sand)

plot(mod_1, pch = 16, which = 1)
plot(mod_sand, pch = 16, which = 1)

trans <- boxcox(mod_sand)
trans_df <- as.data.frame(trans)
optimal_lambda <- trans_df[which.max(trans$y),1]

# Create a new calculated variable based on the optimal lambda value and inspect the new dataframe.
high_sand = cbind(high_sand, high_sand$CUM12Liquid^optimal_lambda)
names(high_sand)[11] = "CUM12Liquid_transf"
head(high_sand,5)

mod_sand2 <- lm(CUM12Liquid_transf ~ LBS_FT.c., data = high_sand)
summary(mod_sand2)

ggplot(data = high_sand, aes(y = CUM12Liquid, x = LBS_FT.c.)) +
  geom_point() + geom_smooth(method = "lm")
  

plot(mod_sand2, pch = 16, which = 1)
##------------------------------------------------------------


ggplot(data = high_sand, aes(y = CUM12Liquid, x = LBS_FT_CLEAN)) +
  geom_point() + geom_smooth(method = "lm")


##create test set and train the model on it before predicting
set.seed(42)
rows <- sample(nrow(high_sand))
high_sand1 <- high_sand[rows,]
# Determine row to split on: split
split <- round(nrow(high_sand1) * 0.8)
# Create train
high_sand_train <- high_sand1[1:split,]
# Create test
high_sand_test <- high_sand1[(split + 1):nrow(high_sand1), ]
# Fit lm model on train: model
mod_2 <- lm(CUM12Liquid ~ LBS_FT_CLEAN, high_sand_train)
# Predict on test: p
p_2 <- predict(mod_2, high_sand_test)
# Compute errors: error
error_2 <- (p_2 - high_sand_test$CUM12Liquid)
# Calculate RMSE
(RMSE_2 <- sqrt(mean(error_2^2, na.rm = TRUE)))


##create a train set in caret with cross validation
mod_3 <- train(
  CUM12Liquid ~ LBS_FT_CLEAN, high_sand_train,
  method = "lm",
  trControl = trainControl(
    method = "cv", number = 10,
    verboseIter = TRUE
  )
)

p_3 <- predict(mod_3, high_sand_test)
error_3 <- p_3 - high_sand_test$CUM12Liquid
RMSE_3 <- sqrt(mean(error_3^2, na.rm = TRUE))

mod_4 <- lm(CUM12Liquid ~ PerfIntervalGross_CLEAN + Category, high_sand_train)


