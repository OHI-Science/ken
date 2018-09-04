library(plyr)

illegal_gear<-read.csv(file = "prep/pressures/Destructive artisanal fishing practices/illegal gears count and score per county per year.csv",header = T,stringsAsFactors = F)

unique(illegal_gear$Illegal.Gears)

illegal_gear<-illegal_gear[-which(illegal_gear$County=='TOTAL'),] #remove old total for illegal gears

illegal_gear$weight_no<-illegal_gear$No.

illegal_gear$weight_no[which(illegal_gear$Illegal.Gears=='Beach Seine'|illegal_gear$Illegal.Gears=='Reef seine')]<-3*illegal_gear$No.[which(illegal_gear$Illegal.Gears=='Beach Seine'|illegal_gear$Illegal.Gears=='Reef seine')]   #seines have the highest weight - highest damaging effect
illegal_gear$weight_no[which(illegal_gear$Illegal.Gears=="Monofilament ")]<-2*illegal_gear$No.[which(illegal_gear$Illegal.Gears=="Monofilament ")]  #mon filament - weight of 2

#spearguns and harpoons - weight of 1

illegal_gear$diff<-illegal_gear$weight_no-illegal_gear$No.  #to find out how many additional 'gears' have been added due to the weights

#calculate the new weighted total of gears and illegal gears for each county per year
illegal_summ<-ddply(illegal_gear,c('County',"Year"),summarise,
                    total_weighted_gears=sum(diff)+unique(All.gears.count),
                    total_weighted_illegal=sum(weight_no))

#new eighted proportion of illegal gears
illegal_summ$weighted_prop<-round(illegal_summ$total_weighted_illegal/illegal_summ$total_weighted_gears,3)

final_results<-illegal_summ[,c(1,2,5)]

write.csv(final_results,'proportion_of_illegal_gears_per_county.csv',row.names = F)
