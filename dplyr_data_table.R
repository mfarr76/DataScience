#https://datascienceplus.com/best-packages-for-data-manipulation-in-r/

rm(list = ls())
#install.packages("compare")
library(dplyr)
library(data.table)
library(lubridate)
library(jsonlite)
library(tidyr)
library(compare)
library(ggplot2)


spending=fromJSON("https://data.medicare.gov/api/views/nrth-mfg3/rows.json?accessType=DOWNLOAD")
names(spending)

meta=spending$meta
hospital_spending=data.frame(spending$data)
colnames(hospital_spending)=make.names(meta$view$columns$name)
hospital_spending=select(hospital_spending,-c(sid:meta))


glimpse(hospital_spending)

cols = 6:11; # These are the columns to be changed to numeric.
hospital_spending[,cols] <- lapply(hospital_spending[,cols], as.character)
hospital_spending[,cols] <- lapply(hospital_spending[,cols], as.numeric)

cols = 12:13; # These are the columns to be changed to dates.
hospital_spending[,cols] <- lapply(hospital_spending[,cols], mdy)

sapply(hospital_spending,class)

hospital_spending_DT = data.table(hospital_spending)

class(hospital_spending_DT)

from_dplyr = select(hospital_spending, Hospital_Name)
from_data_table = hospital_spending_DT[,.(Hospital_Name)]

compare(from_dplyr,from_data_table, allowAll=TRUE)

from_dplyr = select(hospital_spending, -Hospital_Name)
from_data_table = hospital_spending_DT[,!c("Hospital_Name"),with=FALSE]

compare(from_dplyr,from_data_table, allowAll=TRUE)
