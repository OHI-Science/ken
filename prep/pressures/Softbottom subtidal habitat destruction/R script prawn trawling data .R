library(plyr)
#GOOD PRACTICE TO ADD A TITLE TO YOUR SCRIPT AND A BRIEF EXPLANATION OF WHAT THE SCRIPT DOES

#Name of dataset
prawn_trawling_data_<-read.csv(file = "prep/pressures/Softbottom subtidal habitat destruction/prawn trawling data .csv",header = T,stringsAsFactors = F)

#column rename - remove the space
colnames(prawn_trawling_data_)[1]<-'Vessel_Name'
colnames(prawn_trawling_data_)[2]<-'Fishing_Zone'
prawn_trawling_data_ <- rename(prawn_trawling_data_, replace = c("Start.time" = "Start_time"))
prawn_trawling_data_ <- rename(prawn_trawling_data_, replace = c("Finish.time" = "Finish_time"))


#Correcting site names in dataset##

#first need to find out what the different fishing zone names are
unique(prawn_trawling_data_$Fishing_Zone)

prawn_trawling_data_$Fishing_Zone[which(prawn_trawling_data_$Fishing_Zone=='malindi bay')]<-'Malindi Bay'

prawn_trawling_data_$Fishing_Zone[which(prawn_trawling_data_$Fishing_Zone=='Malindi bay')]<-'Malindi Bay'

#Correcting dates in the dataset##
prawn_trawling_data_$Year[which(prawn_trawling_data_$Year=='2026')]<-'2016'

unique(prawn_trawling_data_$Year)

##Create a date field - not actually a date category though
prawn_trawling_data_$date<-paste(prawn_trawling_data_$Day,prawn_trawling_data_$Month,prawn_trawling_data_$Year,sep='/')

#minus start and finish time to get hours##
prawn_trawling_data_$fish_time <- (prawn_trawling_data_$End_time_mins - prawn_trawling_data_$Start_time_mins)

#make table of differences# #changing name of fishing zone to Fishing_Zone##

# prawn_trawling_data_$fish_time<-as.numeric(prawn_trawling_data_$fish_time)
#
# prawn_trawling_data_$fish_time<-as.numeric(prawn_trawling_data_$fish_time)

#Getting Total time, total days#

library(plyr)

total_time<-ddply(prawn_trawling_data_,c("Fishing_Zone","Year"),summarise,
                  total_minutes=sum(fish_time,na.rm = T),
                  total_hours=round(total_minutes/60,1),
                  total_fishing_days=length(unique((date))))

total_time$Region<-NA
total_time$Region[which(total_time$Fishing_Zone=='Malindi Bay')]<-'Kilifi'
total_time$Region[which(total_time$Fishing_Zone=='Ungwana Bay')]<-'Tana River'

total_time<-total_time[,c(6,1:5)]

year_effort<-ddply(total_time,c("Year"),summarise,
                   year_time=sum(total_minutes))

total_time$year_minutes<-year_effort$year_time[match(total_time$Year,year_effort$Year)]

total_time$proportion_effort<-round(total_time$total_minutes/total_time$year_minutes,3)

write.csv(total_time,"prep/pressures/Softbottom subtidal habitat destruction/Total_effort_fishing_shallow_water_prawn_trawl.csv",row.names = F)

final_results<-total_time[,c(1,3,8)]

write.csv(final_results,"prep/pressures/Softbottom subtidal habitat destruction/subtidal_hab_destr_final_county_Scores.csv",row.names = F)

#Getting total time at each trawl vessel (?)#

total_time_trawl<-ddply(prawn_trawling_data_,c("Fishing_Zone","Vessel_Name","Year"),summarise,
                        total_minutes=sum(fish_time,na.rm = T),
                        total_hours=round(total_minutes/60,1),
                        total_fishing_days=length(unique(date)))

total_time_trawl<-total_time_trawl[order(total_time_trawl$Fishing_Zone,total_time_trawl$Year),]

write.csv(total_time_trawl,"prep/pressures/Softbottom subtidal habitat destruction/Total_fishing_effort_per_vessel_shallow_water_prawn_trawl.csv",row.names = F)
