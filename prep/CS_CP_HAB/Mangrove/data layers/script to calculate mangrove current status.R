#Objective-Calculating mangrove current status

library(dplyr)
csv2014<-read.csv("D:/git/ken/prep/CS_CP_HAB/Mangrove/Extracted_regional_value _csv/KEmangrove_2014.csv")

csv1992<-read.csv("D:/git/ken/prep/CS_CP_HAB/Mangrove/Extracted_regional_value _csv/KEmangrove_1992-baseline_yr.csv")
#change header name
names(csv1992)[3]<-paste("ref year92")

#Columnbind based on a common field
datamerged<-merge(csv2014, csv1992, by="rgn_name")

#Calculating curent status
datamerged$C_status<-(datamerged$totalcover/datamerged$`ref year92`)


cal_status <- datamerged[c(1,2,7)]
cal_status["habitat"]<-"mangrove"

#Export calculated data

write.csv(cal_status,"D:/git/ken/prep/CS_CP_HAB/Mangrove/data layers/mangrove_current_status.csv")
