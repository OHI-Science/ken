#Name of dataset
prawn_trawling_data_<-read.csv(file = "prep/pressures/Softbottom subtidal habitat destruction/prawn trawling data .csv",header = T,stringsAsFactors = F)

#Changing names in dataset##
prawn_trawling_data_$Fishing_Zone[which(prawn_trawling_data_$Fishing_Zone=='malindi bay')]<-'Malindi Bay'

prawn_trawling_data_$Fishing_Zone[which(prawn_trawling_data_$Fishing_Zone=='Malindi bay')]<-'Malindi Bay'

#Changing dates in the dataset##
prawn_trawling_data_$Year[which(prawn_trawling_data_$Year=='2026')]<-'2016'

##Create a datetime object with posixct
prawn_trawling_data_$date<-paste(prawn_trawling_data_$Day,prawn_trawling_data_$Month,prawn_trawling_data_$Year,sep='/')

#minus start and finish time to get hours##
prawn_trawling_data_$fish_time <- (prawn_trawling_data_$`Finish time` - prawn_trawling_data_$`Start time`)/60

#make table of differences# #changing name of fishing zone to Fishing_Zone##

prawn_trawling_data_$fish_time<-as.numeric(prawn_trawling_data_$fish_time)
colnames(prawn_trawling_data_)[2]<-'Fishing_Zone'

prawn_trawling_data_$fish_time<-as.numeric(prawn_trawling_data_$fish_time)
colnames(prawn_trawling_data_)[1]<-'Vessel_Name'

#Getting Total time, total days#

library(plyr)

total_time<-ddply(prawn_trawling_data_,c("Fishing_Zone","Year"),summarise,total_minutes=sum(fish_time,na.rm = T))

total_days<-ddply(prawn_trawling_data_,c("Fishing_Zone","Year"),summarise,total_days=length(unique((date))))

#Getting total time at each trawl vessel (?)#

total_time_trawl<-ddply(prawn_trawling_data_,c("Vessel_Name","Year"),summarise,total_vessel=sum(fish_time,na.rm = T))

#Getting total days each vessel spent in the year#

total_day_trawl<-ddply(prawn_trawling_data_,c("Vessel_Name","Year"),summarise,total_day_trawl=length(unique(date)))
