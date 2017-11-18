
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

#load("C:/Users/MFARR/Documents/R_files/Spotfire.data/wellheader.RData")

#select.list(chr_names)
#chr_names <- list("API", "OperatorName", "WellName", "CountyName", "TOE_UP_DWN", "Spacing_Categor")

stx <- fread("stxDataset.csv", stringsAsFactors = FALSE)
glimpse(stx)
#names(stx)




##remove (,) example 1,000 
stx <- data.frame(lapply(stx, gsub, pattern = ',', replacement = ''))
##convert to date
stx$DateProductionStart <- as.Date(stx$DateProductionStart, "%m/%d/%Y")
##list of chr strings
i <- grep("API|OperatorName|WellName|CountyName|TOE_UP_DWN|Spacing_Categor", colnames(stx))
##conver to chr
stx[, i] <- apply(stx[,i], 2, as.character)
##colnames that are factors
fact_list <- lapply(stx, class) == "factor"
##conver factors to numeric
stx[fact_list] <- sapply(stx[fact_list], function(x) as.numeric(levels(x))[x])


Num_NA <- sapply( stx,function( y ) length( which( is.na ( y ) == TRUE )))
(NA_Count<- data.frame( Item = colnames ( stx ) ,Count = Num_NA ))

##select only numeric columns and create new data table
stx_num <- select_if(stx, is.numeric)

correlate <- cor( stx_num, use = "everything" )
corrplot(correlate, method="circle", type="lower",  sig.level = 0.01, insig = "blank")
corrplot(correlate, method = "ellipse")


##check for NA
a <- is.na(stx_num)
## Set to 0 if NA
stx_num[a] <-0

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

stx_12mo <- stx %>%
  filter(!is.na(First12MonthGas))
write.csv(stx_12mo, file = "stx_12.csv")
summary(stx_12mo)
unique(stx_12mo$CountyName)

stx_12op <- stx_12mo %>%
  group_by(OperatorName) %>%
  summarise_all(mean)
write.csv(stx_12op, file = "stx_12op.csv")

ggplot(stx_12mo, aes(OperatorName)) + 
  geom_bar(aes(fill = OperatorName)) +
  theme(legend.position = "none") + 
  scale_y_sqrt()

##wellcount by operator
stx_12mo %>%
  group_by(OperatorName) %>%
  count()

count(stx_12mo, OperatorName)
##why is silverbow so high?

stx_12mo %>% filter(OperatorName == "SILVERBOW RESOURCES OPER LLC")
