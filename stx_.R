
rm(list = ls())
library(dplyr)
library(ggplot2)
library(broom)
library(caret)
#library(MASS)
#detach(package:MASS)
library(corrplot)
library(data.table)
library(purrr)
library(stringr)
library(statisticalModeling)

#load("C:/Users/MFARR/Documents/R_files/Spotfire.data/wellheader.RData")

#select.list(chr_names)
#chr_names <- list("API", "OperatorName", "WellName", "CountyName", "TOE_UP_DWN", "Spacing_Categor")

stx <- fread("stxDataset.csv")
#rowcount
stx <- mutate(stx, id = as.factor(1:n()))
#glimpse(stx)
#names(stx)

##count NA
sapply(stx, function(x) sum(is.na(x)))

##count NA function
count_NA <- function(x){
  count_tbl <- sapply( x , function( y ) length ( which ( is.na (y) == TRUE)))
  dt_count <- data.frame( Item = colnames(x), Count = count_tbl)
  return(dt_count)
}

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

##list of chr strings
#i <- grep("API|OperatorName|WellName|CountyName|TOE_UP_DWN|Spacing_Categor", colnames(stx))
##conver to chr
#stx[, i] <- apply(stx[,i], 2, as.character)
stx_num$id <- as.factor(stx_num$id)

stx <- left_join(stx_chr, stx_num, by = "id")

stx_sm <- filter(stx, OperatorName == "SM ENERGY COMPANY", 
                 Spacing_Category == "Bounded", 
                 CountyName == "WEBB", 
                 str_detect(WellName, 'GAL')) 
                  
##select only numeric columns and create new data table
stx_num <- select_if(stx, is.numeric)

correlate <- cor( stx_num, use = "everything" )
corrplot(correlate, method="circle", type="lower",  sig.level = 0.01, insig = "blank")
corrplot(correlate, method = "ellipse")


##check for NA
a <- is.na(stx_num)
## Set to 0 if NA
stx_num[a] <- 0

##prove that changing NA to 0 will give you different results##-----------
stx_num_NA <- stx_num %>%
  select(Lat, Lon, First6MonthGas)

a <- is.na(stx_num_NA)
stx_num_NA[a] <- 0
cor_NA <- cor(stx_num_NA)

stx_num_removeNA <- stx_num %>%
  select(Lat, Lon, First6MonthGas) %>%
  filter(!is.na(First6MonthGas))

cor_na_rm <- cor(stx_num_removeNA)

cor_na_rm - cor_NA
####----------------------------------------------------------------

stx_12 <- stx_sm %>%
  filter(!is.na(First12MonthGas),
         !is.na(SoPhiH_LEF), 
         Spacing_Category == "Bounded", 
         FluidAmountTotal > 0, 
         Max_Infill_Time < 6 & Max_Infill_Time > -6) %>%
  mutate(Cum12Gas_MMcf = First12MonthGas / 1000, 
         Cum12Oil_Mbo = First12MonthLiquid / 1000, 
         Cum12_MBoe = Cum12Oil_Mbo + (Cum12Gas_MMcf / 6)) %>%
  select(OperatorName, TOE_UP_DWN, FluidAmountTotal, ProppantAmountTotal, SoPhiH_LEF, 
         Spacing_Category, Spacing_Avg, First12MonthGas, First12MonthLiquid,
         Cum12Gas_MMcf, Cum12Oil_Mbo, Cum12_MBoe)
#write.csv(stx_12, file = "stx_12.csv")


##select only numeric columns and create new data table
stx_cor <- select_if(stx_12, is.numeric)

correlate <- cor( stx_cor, use = "everything" )
corrplot(correlate, method="circle", type="lower",  sig.level = 0.01, insig = "blank")

summary(stx_12$FluidAmountTotal)

stx_12op <- stx_12 %>%
  group_by(OperatorName) %>%
  summarise_all(mean)



ggplot(stx_12, aes(OperatorName)) + 
  geom_bar(aes(fill = OperatorName)) +
  theme(legend.position = "none") + 
  scale_y_sqrt()

ggplot(stx_12, aes(First12MonthGas)) + 
  geom_density(aes( fill = factor(CountyName)), size = 1) + 
  labs(Title = "Denisty Plot")

ggplot(stx_12, aes(factor(TOE_UP_DWN), First12MonthGas)) + 
  geom_boxplot(aes(fill = TOE_UP_DWN))

##wellcount by operator
stx_12 %>%
  group_by(OperatorName) %>%
  summarise(n())

count(stx_12, OperatorName)
##why is silverbow so high?

stx_12 %>%
  filter(FluidAmountTotal > 100000) %>% 
  ggplot( aes(x = FluidAmountTotal, y = Cum12Gas_MMcf)) +
  geom_point() +
  geom_smooth(method = "lm")

stx_12 %>% 
  filter(FluidAmountTotal > 10000) %>%
  select(FluidAmountTotal) %>%
  ggplot( aes(FluidAmountTotal)) + 
  geom_density()
##-----------------------------
count_NA(stx_12)

trainRow <- createDataPartition(stx_12$Cum12Gas_MMcf, p = 0.8, list = FALSE)
nrow(trainRow)

training_sm <- stx_12[trainRow, ]
testing_sm <- stx_12[-trainRow, ]

##check the distribution
table(training_sm$TOE_UP_DWN)
table(testing_sm$TOE_UP_DWN)


mod <- lm(Cum12Gas_MMcf ~ FluidAmountTotal, data = stx_12)
summary(mod)
out <- evaluate_model(mod, data = stx_12)
with(data = out, mean((First12MonthGas - model_output)^2, na.rm = TRUE))

stx_12$TOE_UP_DWN <- as.factor(stx_12$TOE_UP_DWN)
rf_train <- stx_12[, c("TOE_UP_DWN", "Cum12Gas_MMcf")]
rf_label <- as.factor(stx_12$OperatorName)

set.seed(1234)
rf_1 <- randomForest(x = rf_train, y = rf_label, importance = TRUE, ntree = 1000)
rf_1
varImpPlot(rf_1)







