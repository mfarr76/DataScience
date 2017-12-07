
rm(list = ls())
# general visualisation
library('ggplot2') # visualisation
library('RColorBrewer') # visualisation
library('corrplot') # visualisation

# general data manipulation
library('plyr') # data manipulation
library('dplyr') # data manipulation
library('readr') # input/output
library('tibble') # data wrangling
library('tidyr') # data wrangling
library('stringr') # string manipulation
library('forcats') # factor manipulation

# Dates
library('lubridate') # date and time

# Extra vis
library('ggforce') # visualisation
library('ggridges') # visualisation

# Model
library('caret')
library('ranger')
library('randomForest')
library('rpart')
library('rpart.plot')
library('broom')
library('lars')
library('glmnet')

##EagleFord Webb and Dimmit counties 
##Wellhead information in .csv file

#create stx package
stx <- read.csv("stxDataset.csv", header = TRUE)
#rowcount to use in join
#stx <- mutate(stx, id = as.factor(1:n()))

##format datatypes---------------------------
##create column to convert to numeric
factor_col <- c("API","OperatorName","WellName","CountyName","TOE_UP_DWN",
                "Spacing_Category", "DateProductionStart")
##create factor_col into factors
stx[factor_col] <- lapply(stx[factor_col], as.factor)
##create a list of colnames to turn into numeric
numeric_col <- !colnames(stx) %in% factor_col

##remove 0,000 from the number format
stx[numeric_col] <- data.frame(lapply(stx[numeric_col], gsub, pattern = ',', replacement = '', fixed = TRUE))
##convert numeric_col into numeric datatype
stx[numeric_col] <- sapply(stx[numeric_col], function(x) as.numeric(levels(x))[x])
##change first prod date into date datatype
stx$DateProductionStart <- as.Date(stx$DateProductionStart, "%m/%d/%Y")

##create a sm dataset
stx_sm <- stx %>%
  filter(#OperatorName == "SM ENERGY COMPANY",
         Spacing_Category == "Bounded", 
         CountyName == "WEBB", 
         #str_detect(WellName, 'GAL'),
         FluidAmountTotal > 0, 
         ProppantAmountTotal > 0,
         First12MonthGas > 0) %>%
  mutate(Lbs_Ft = ProppantAmountTotal / PerfIntervalGross, 
         Bbl_Ft = (FluidAmountTotal / 42) / PerfIntervalGross, 
         Cum12Gas_MMcf = First12MonthGas / 1000, 
         category = as.factor(ifelse(Cum12Gas_MMcf > quantile(Cum12Gas_MMcf, p = 0.75), "High", "Low"))) %>%
  select(OperatorName, Lat, Lon, TOE_UP_DWN, Lbs_Ft, Bbl_Ft, Lbs_Ft, Bbl_Ft, TotalDepthTVD, 
         SoPhiH_LEF, Spacing_Avg, Vintage, Cum12Gas_MMcf, EffLat = PerfIntervalGross, category)

stx_sm_redu1 <- stx_sm %>%
  select(Cum12Gas_MMcf, EffLat, Lbs_Ft, Bbl_Ft, TotalDepthTVD, 
         SoPhiH_LEF, Spacing_Avg, Lat, Lon, category)

summary(stx_sm_redu2)
ggplot(stx_sm, aes(x = OperatorName, fill = category)) + 
  geom_bar()


ggplot(high_sand, aes(x = OperatorName, fill = Category)) + 
  geom_bar(position = "fill") + 
  ylab("Relative frequencies")
table(stx_sm$category)

stx_sm_redu2 <- stx_sm %>%
  select(Cum12Gas_MMcf, SoPhiH_LEF)

##---------------make plot of key metrics to check normality----------------------------------

p1 <- ggplot(stx_sm_redu1, aes(x = EffLat)) + 
  geom_histogram()

p2 <- ggplot(stx_sm_redu1, aes(x = Proppant)) +
  geom_density(fill = "white")
  
p3 <- ggplot(stx_sm_redu1, aes(x = Fluid)) + 
  geom_density(fill = "yellow")

p4 <- ggplot(stx_sm_redu1, aes(x = SoPhiH_LEF)) + 
  geom_density(fill = "blue")

p5 <- ggplot(stx_sm_redu1, aes(x = Spacing_Avg)) + 
  geom_density(fill = "red")


layout <- matrix(c(1,1,2,2,3,3,4,4,4,5,5,5),2,6,byrow=TRUE)
multiplot(p1, p2, p3, p4, p5, layout=layout)

stx_cen_scal <- data.frame(scale(stx_sm_redu1[,numerics]))


stx_num <- select_if(stx_sm_redu1, is.numeric)
correlate <- cor(stx_num, use = "everything")
corrplot(correlate, method = "circle", type = "lower", sig.level = 0.01, insig = "blank")

##==========================================================================================

##create test / train dataset
set.seed(1234)
trainRow <- createDataPartition(stx_sm_redu1$category, p = 0.8, list = FALSE)
train_sm <- stx_sm_redu1[trainRow, ]
test_sm <- stx_sm_redu1[-trainRow, ]



##multi-variant linear model----------------------------------------------
lm_stx <- lm(Cum12Gas_MMcf ~ ., data = train_sm)
pred_lm_stx <- predict(lm_stx, test_sm)
error_lm_stx <- pred_lm_stx - test_sm$Cum12Gas_MMcf
RMSE_lm_stx <- sqrt(mean(error_lm_stx^2, na.rm = TRUE))
summary(lm_stx)
#evaluate_model(lm_stx)
#tidy(lm_stx)


##randomForest model------------------------------------------------------
set.seed(1234)
rf_stx <- train(Cum12Gas_MMcf ~.,
                tuneLength = 10, 
                data = train_sm, method = "rf",
                preProcess = c("center", "scale"),
                #tuneGrid = myGrid, 
                trControl = trainControl(method = "cv", number = 5, 
                                         verboseIter = TRUE))

plot(rf_stx)
pred_rf_stx <- predict(rf_stx, test_sm)
error_rf_stx <- pred_rf_stx - test_sm$Cum12Gas_MMcf
RMSE_rf_stx <- sqrt(mean(error_rf_stx^2, na.rm = TRUE))
varImp(rf_stx)
##------------------------------------------------------------------------

##randomForest-Ranger model-----------------------------------------------
myGrid <- data.frame(mtry = c(2, 2, 4, 4, 6, 6, 8, 8), 
                     splitrule = c("variance", "extratrees", "variance", "extratrees",
                                   "variance", "extratrees", "variance", "extratrees"))

set.seed(1234)
rf_ranger <- train(Cum12Gas_MMcf ~.,
                      tuneLength = 10, 
                      data = train_sm, method = "ranger",
                      importance = "permutation", 
                      preProcess = c("center", "scale"),
                      #tuneGrid = myGrid, 
                      trControl = trainControl(method = "cv", number = 5, 
                                               verboseIter = TRUE))

plot(rf_ranger)

pred_rf_ranger <- predict(rf_ranger, test_sm)
pr_ranger <- postResample(pred_rf_ranger, test_sm$Cum12Gas_MMcf)
error_rf_ranger <- pred_rf_ranger - test_sm$Cum12Gas_MMcf
RMSE_rf_ranger <- sqrt(mean(error_rf_ranger^2, na.rm = TRUE))
varImp(rf_ranger, scale = FALSE)
##------------------------------------------------------------------------

model_list <- list(item1 = rf_stx, item2 = rf_ranger)
resamps <- resamples(model_list)
summary(resamps)
##glmnet model-----------------------------------------------------------
# Create custom trainControl: myControl
myControl <- trainControl(
  method = "cv", number = 10,
  summaryFunction = twoClassSummary,
  classProbs = TRUE, # IMPORTANT!
  verboseIter = TRUE)

myglmGrid <- expand.grid(alpha = 0:1, 
                         lambda = seq(0.0001, 1, length = 10))

# Fit glmnet model: model
glmnet_stx <- train(category~., data = train_sm,
                    method = "glmnet",
                    tuneGrid = myglmGrid,
                    trControl = myControl)


plot(glmnet_stx)
plot(glmnet_stx$finalModel)
max(glmnet_stx[["results"]][["ROC"]])
##gbm model---------------------------------------------------------------

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)

set.seed(1234)
gbmFit1 <- train(Cum12Gas_MMcf ~ ., data = train_sm, 
                 method = "gbm", 
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)

preds <- predict(gbmFit1, test_sm)
error_gbm <- preds - test_sm$Cum12Gas_MMcf
RMSE_gbm <- sqrt(mean(error_gbm^2, na.rm = TRUE))
pr_gbm <- postResample(preds, test_sm$Cum12Gas_MMcf)

##------------------------------------------------------------------------


##rpart comparision
rp_model <- rpart(Cum12Gas_MMcf ~., data = stx_sm_redu1, cp = 0.02)
prp(rp_model, type = 4)
prp(rp_model, faclen = 0, cex = 0.8, extra = 1)
rpart.plot(rp_model, type = 4)
write.csv(stx_sm_redu2, file = "stx_sm.csv")

RMSE_compare <- data.frame(RMSE_lm_stx, RMSE_rf_stx, RMSE_rf_ranger, RMSE_gbm)




test_sm$model_output <- pred_rf_ranger

ggplot(test_sm, aes(x = model_output, y = Cum12Gas_MMcf)) +
  geom_point() + 
  geom_smooth(method = "lm")

mod <- lm(Cum12Gas_MMcf ~ model_output, data = test_sm)
summary(mod)

dt <- data.frame(pr_ranger, pr_gbm)


plot(rf_ranger)
confusionMatrix(pred_rf_ranger, test_sm$Cum12Gas_MMcf)


ggplot(stx_sm_redu1, aes(x = ProppantAmountTotal, y = Cum12Gas_MMcf)) +
  geom_point() + geom_smooth(method = "lm")

pred <- data.frame(Lbs_Ft = c(1500, 1500, 2000, 2000), Bbl_Ft = c(25, 40, 25, 40))
fit <- predict(lm_stx, pred)
predict(rf_stx, pred)
predict(rf_ranger, pred)
evaluate_model(rf_stx, pred)
plot(rf_ranger)

modelLookup(model = 'ranger')

Independent_variable<- as.matrix(train_sm[2:10])
Dependent_Variable<- as.matrix(train_sm[1])
laa<- lars(Independent_variable,Dependent_Variable,type = 'lasso')
plot(laa)


