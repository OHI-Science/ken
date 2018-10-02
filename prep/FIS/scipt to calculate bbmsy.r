library(here)
setwd(here::here('prep/FIS/'))


#script to calculate b/bmsy for each species and combine into single output layer file

nation<-read.csv(file ='fis_test.csv',header = T,stringsAsFactors = F)

nation$biomass<-NA
nation$harvest_rate<-NA

#step 1 - calculate virgin biomass, which is total catch / 2

#Schaefer model
#calculate B0 and assign it to first year
#step: calculate the biomass for other years other than base year

y<-unique(nation$species)

for (i in 1:length(y)){

  nation2=nation[which(nation$species==y[i]),]

library(dplyr)
nation3<-nation2 %>%
  group_by(species)%>%
  # filter(species)%>%
 # summarise(sum(catch,na.rm=T)/2)%>%
  arrange(species,Year)%>%
  mutate(biomass=ifelse(row_number()==1,sum(catch,na.rm=T)/2,NA))%>%
  mutate(biomass=ifelse(row_number()==2,lag(biomass)+k*lag(biomass)*(1-lag(biomass)/.$biomass[.$Year == min(.$Year)])-ifelse(lag(catch)/lag(biomass)<1,lag(catch)/lag(biomass),0.99)*lag(biomass),biomass))%>%
  # harvest_rate=ifelse(lag(catch)/lag(biomass)<1,lag(catch)/lag(biomass),0.99))%>%
  mutate(biomass=ifelse(row_number()==3,lag(biomass)+k*lag(biomass)*(1-lag(biomass)/.$biomass[.$Year == min(.$Year)])-ifelse(lag(catch)/lag(biomass)<1,lag(catch)/lag(biomass),0.99)*lag(biomass),biomass))%>%
  mutate(biomass=ifelse(row_number()==4,lag(biomass)+k*lag(biomass)*(1-lag(biomass)/.$biomass[.$Year == min(.$Year)])-ifelse(lag(catch)/lag(biomass)<1,lag(catch)/lag(biomass),0.99)*lag(biomass),biomass))%>%
  mutate(biomass=ifelse(row_number()==5,lag(biomass)+k*lag(biomass)*(1-lag(biomass)/.$biomass[.$Year == min(.$Year)])-ifelse(lag(catch)/lag(biomass)<1,lag(catch)/lag(biomass),0.99)*lag(biomass),biomass))%>%
  mutate(biomass=ifelse(row_number()==6,lag(biomass)+k*lag(biomass)*(1-lag(biomass)/.$biomass[.$Year == min(.$Year)])-ifelse(lag(catch)/lag(biomass)<1,lag(catch)/lag(biomass),0.99)*lag(biomass),biomass))%>%
  mutate(biomass=ifelse(row_number()==7,lag(biomass)+k*lag(biomass)*(1-lag(biomass)/.$biomass[.$Year == min(.$Year)])-ifelse(lag(catch)/lag(biomass)<1,lag(catch)/lag(biomass),0.99)*lag(biomass),biomass))%>%
  mutate(biomass=ifelse(row_number()==8,lag(biomass)+k*lag(biomass)*(1-lag(biomass)/.$biomass[.$Year == min(.$Year)])-ifelse(lag(catch)/lag(biomass)<1,lag(catch)/lag(biomass),0.99)*lag(biomass),biomass))%>%
  mutate(biomass=ifelse(row_number()==9,lag(biomass)+k*lag(biomass)*(1-lag(biomass)/.$biomass[.$Year == min(.$Year)])-ifelse(lag(catch)/lag(biomass)<1,lag(catch)/lag(biomass),0.99)*lag(biomass),biomass))%>%
  mutate(biomass=ifelse(row_number()==10,lag(biomass)+k*lag(biomass)*(1-lag(biomass)/.$biomass[.$Year == min(.$Year)])-ifelse(lag(catch)/lag(biomass)<1,lag(catch)/lag(biomass),0.99)*lag(biomass),biomass))%>%
  mutate(biomass=ifelse(row_number()==11,lag(biomass)+k*lag(biomass)*(1-lag(biomass)/.$biomass[.$Year == min(.$Year)])-ifelse(lag(catch)/lag(biomass)<1,lag(catch)/lag(biomass),0.99)*lag(biomass),biomass))%>%
  mutate(biomass=ifelse(row_number()==12,lag(biomass)+k*lag(biomass)*(1-lag(biomass)/.$biomass[.$Year == min(.$Year)])-ifelse(lag(catch)/lag(biomass)<1,lag(catch)/lag(biomass),0.99)*lag(biomass),biomass))%>%
  mutate(biomass=ifelse(row_number()==13,lag(biomass)+k*lag(biomass)*(1-lag(biomass)/.$biomass[.$Year == min(.$Year)])-ifelse(lag(catch)/lag(biomass)<1,lag(catch)/lag(biomass),0.99)*lag(biomass),biomass))%>%
  mutate(biomass=ifelse(row_number()==14,lag(biomass)+k*lag(biomass)*(1-lag(biomass)/.$biomass[.$Year == min(.$Year)])-ifelse(lag(catch)/lag(biomass)<1,lag(catch)/lag(biomass),0.99)*lag(biomass),biomass))%>%
  mutate(biomass=ifelse(row_number()==15,lag(biomass)+k*lag(biomass)*(1-lag(biomass)/.$biomass[.$Year == min(.$Year)])-ifelse(lag(catch)/lag(biomass)<1,lag(catch)/lag(biomass),0.99)*lag(biomass),biomass))%>%
    ungroup()

if (i==1){nation4=nation3}else{
nation4<-rbind(nation4,nation3)
}
}


#harvest rate = catch/biomass
nation4$harvest_rate=nation4$catch/nation4$biomass

nation4$harvest_rate[which(nation4$harvest_rate>1)]<-0.99 #cap mean_catch

nation4$bbmsy<-round(nation4$biomass/nation4$bmsy,2)

#add rgn_id column
#need to replicate nation 4, 5 times one for each region
nation4<-nation4[rep(seq_len(nrow(nation4)), each=5),]

nation4$rgn_id <- rep_len(1:5, length.out=75)

nation4<-nation4[order(nation4$rgn_id),]

final<-nation4[,c(9,1,4,8)]

write.csv(final,'fis_b_bmsy_ken2018.csv',row.names = F)

# calculate mean_catch per species per region ----------------------------------------

catch=read.csv('fis_totalcatch_ken2018.csv',header = T,stringsAsFactors = F)

catch2<-catch %>%
  group_by(rgn_id,stock_id_taxonkey)%>%
 mutate(ave_catch=mean(mean_catch))

catch3<-catch2[,c(1,3,2,5)]

colnames(catch3)[4]<-'mean_catch'

write.csv(catch3,"fis_meancatch_ken2018.csv",row.names = F)
