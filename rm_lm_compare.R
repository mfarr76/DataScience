install.packages("tidyverse")
#install.packages(c("purrr", "caret", "broom", "ggplot2"))
#install.packages(c("corrplot", "data.table", "rpart.plot"))
install.packages(c("statisticalModeling", "mlbench", "ranger"))

rm(list = ls())
library(dplyr)
library(ggplot2)
library(broom)
library(caret)
#detach(package:MASS)
library(corrplot)
library(randomForest)
library(rpart)
library(rpart.plot)
library(data.table)
library(purrr)
library(stringr)
library(statisticalModeling)
library(mlbench)
library(ranger)



karnes <- read.csv("Karnes_data.csv", stringsAsFactors = FALSE)
karnes$DateProductionStart <- as.Date(karnes$DateProductionStart, "%m/%d/%Y")

##conver cols to numeric if they are interger
karnes <- karnes %>%
  na.omit() %>%
  mutate(EffLat = PerfIntervalGross_CLEAN) %>%
  select(-c(PerfIntervalGross, PerfIntervalGross_CLEAN)) %>%
  mutate_if(is.integer, as.numeric) %>%
  mutate_if(is.character, as.factor) 


karnes_num <- select_if(karnes, is.numeric)

high_sand <- karnes %>%
  na.omit() %>%
  mutate(Cum12Oil_Mbo = CUM12Liquid / 1000,
         Cum6Oil_Mbo = CUM6Liquid / 1000) %>%
  filter(LBS_FT_CLEAN > 1500) %>%
  select(OperatorName, EffLat, LBS_FT_CLEAN, BBL_FT_CLEAN, 
         Spacing_Avg, GOR_6M, IPLiquid ,Cum6Oil_Mbo, Cum12Oil_Mbo)


high_sand_redu <- high_sand %>%
  select(Cum12Oil_Mbo, LBS_FT_CLEAN,
         Spacing_Avg, EffLat, IPLiquid, GOR_6M)

set.seed(1234)
trainRow <- createDataPartition(high_sand_redu$Cum12Oil_Mbo, p = 0.8, list = FALSE)
training_hs <- high_sand_redu[trainRow, ]
testing_hs <- high_sand_redu[-trainRow, ]

#summary(training_hs)
#summary(testing_hs)

lm_1 <- lm(Cum12Oil_Mbo ~ ., data = training_hs)
pred_ls_1 <- predict(lm_1, testing_hs)
error_lm_1 <- pred_ls_1 - testing_hs$Cum12Oil_Mbo
RMSE_lm_1 <- sqrt(mean(error_lm_1^2, na.rm = TRUE))
#summary(lm_1)

set.seed(1234)
#rf_1 <- randomForest(Cum12Oil_Mbo ~ LBS_FT_CLEAN + Spacing_Avg, data = training_hs, ntree = 1000)
rf_1 <- randomForest(Cum12Oil_Mbo ~ ., data = training_hs, ntree = 1000)
pred_rf_1 <- predict(rf_1, testing_hs)
error_rf_1 <- pred_rf_1 - testing_hs$Cum12Oil_Mbo
RMSE_rf_1 <- sqrt(mean(error_rf_1^2, na.rm = TRUE))

########################################

stx <- fread("stxDataset.csv")
#rowcount
stx <- mutate(stx, id = as.factor(1:n()))

col_num <- c("API","OperatorName","WellName","CountyName","TOE_UP_DWN","Spacing_Category")
stx_num <- stx[,!colnames(stx) %in% col_num]
col_chr <- c("API","OperatorName","WellName","CountyName","TOE_UP_DWN","Spacing_Category", "id")
stx_chr <- stx[,colnames(stx) %in% col_chr]
stx_chr[] <- lapply(stx_chr, as.factor)

##remove (,) example 1,000 
stx_num <- data.frame(lapply(stx_num, gsub, pattern = ',', replacement = '', fixed = TRUE))
##convert to date
stx_num$DateProductionStart <- as.Date(stx_num$DateProductionStart, "%m/%d/%Y")
##colnames that are factors
fact_list <- lapply(stx_num, class) == "factor"
##conver factors to numeric
stx_num[fact_list] <- sapply(stx_num[fact_list], function(x) as.numeric(levels(x))[x])

stx_num$id <- as.factor(stx_num$id)

stx <- left_join(stx_chr, stx_num, by = "id")

stx_sm <- stx %>%
  filter(OperatorName == "SM ENERGY COMPANY",
         Spacing_Category == "Bounded", 
         CountyName == "WEBB", 
         str_detect(WellName, 'GAL'),
         !is.na(First12MonthGas)) %>%
  mutate(Lbs_Ft = ProppantAmountTotal / PerfIntervalGross, 
         Bbl_Ft = (FluidAmountTotal / 42) / PerfIntervalGross, 
         Cum12Gas_MMcf = First12MonthGas / 1000) %>%
  select(Lat, Lon, ProppantAmountTotal, FluidAmountTotal, Lbs_Ft, Bbl_Ft, TotalDepthTVD, 
         SoPhiH_LEF, Spacing_Avg, Vintage, Cum12Gas_MMcf, PerfIntervalGross)

stx_sm_redu1 <- stx_sm %>%
  select(Cum12Gas_MMcf, ProppantAmountTotal, FluidAmountTotal, PerfIntervalGross, 
         TotalDepthTVD, SoPhiH_LEF)

stx_sm_redu2 <- stx_sm %>%
  select(Cum12Gas_MMcf, Lbs_Ft)


##rpart comparision
rp_model <- rpart(Cum12Gas_MMcf ~., data = stx_sm_redu1, cp = 0.02)
prp(rp_model, type = 4)
rpart.plot(rp_model)

##create test / train dataset
set.seed(1234)
trainRow <- createDataPartition(stx_sm_redu1$Cum12Gas_MMcf, p = 0.8, list = FALSE)
train_sm <- stx_sm_redu1[trainRow, ]
test_sm <- stx_sm_redu1[-trainRow, ]

lm_stx <- lm(Cum12Gas_MMcf ~ ., data = train_sm)
pred_lm_stx <- predict(lm_stx, test_sm)
error_lm_stx <- pred_lm_stx - test_sm$Cum12Gas_MMcf
RMSE_lm_stx <- sqrt(mean(error_lm_stx^2, na.rm = TRUE))
#summary(stx_lm1)
#evaluate_model(stx_lm1)

set.seed(1234)
rf_stx <- randomForest(Cum12Gas_MMcf ~., data = train_sm, ntree = 1000)
pred_rf_stx <- predict(rf_stx, test_sm)
error_rf_stx <- pred_rf_stx - test_sm$Cum12Gas_MMcf
RMSE_rf_stx <- sqrt(mean(error_rf_stx^2, na.rm = TRUE))
plot(rf_stx)

myControl <- trainControl(
  method = "cv", number = 10,
  summaryFunction = twoClassSummary,
  classProbs = TRUE, verboseIter = TRUE
)

model <- train(Cum12Gas_MMcf ~., train_sm, 
               method = "glmnet", 
               trControl = myControl)


set.seed(1234)
rf_ranger <- train(Cum12Gas_MMcf ~.,
                   tuneLength = 10, 
                   data = train_sm, method = "ranger", 
                   trControl = trainControl(method = "cv", number = 5, 
                                            verboseIter = TRUE))




rf_ranger
#plot(rf_ranger)
rf_ranger <- train(Cum12Gas_MMcf ~., data = train_sm, method = "ranger")
pred_rf_ranger <- predict(rf_ranger, test_sm)
error_rf_ranger <- pred_rf_ranger - test_sm$Cum12Gas_MMcf
RMSE_rf_ranger <- sqrt(mean(error_rf_ranger^2, na.rm = TRUE))

plot(rf_ranger)
confusionMatrix(pred_rf_ranger, test_sm$Cum12Gas_MMcf)


ggplot(stx_sm_redu1, aes(x = ProppantAmountTotal, y = Cum12Gas_MMcf)) +
  geom_point() +
  geom_smooth(method = "lm")
