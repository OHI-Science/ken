library(here)
setwd(here::here('prep/FIS/'))


# calculate national catch per species per year ---------------------------

county_catch<-read.csv(file = "kenya_county_catch_per_year.csv",header = T, stringsAsFactors = F)

county_catch$national_total<-rowSums(county_catch[, c(2:6)])

#need to create a new raw file which can be used to calculate mean_catch
library(tidyr)

county_long <- gather(county_catch, region, catch, Lamu:Kwale)

county_long$rgn_id[which(county_long$region=='Lamu')]<-'5'
county_long$rgn_id[which(county_long$region=='Mombasa')]<-'1'
county_long$rgn_id[which(county_long$region=='Kwale')]<-'2'
county_long$rgn_id[which(county_long$region=='Kilifi')]<-'3'
county_long$rgn_id[which(county_long$region=='Tana.river')]<-'4'

county_long<-county_long[,c(6,1,2,5)]

write.csv(county_long,'fis_catch_per_county_ken2018.csv',row.names = F)  #will be used to calculate mean_catch


#read in this file, so we can update with latest catch and species- using exisiting bmsy and k values for each species
nation_pre<-read.csv(file ='fis_test.csv',header = T,stringsAsFactors = F)


# #add extra rows to be populated with new species data - will use global b_bmsy values for istiophurus
# nation_pre[nrow(nation_pre)+15,] <- NA
# nation_pre$Year[76:90] <- 2002:2016
# nation_pre$species[76:90] <- 'Istiophurus_sp'
# nation_pre$k[76:90] <- 'Istiophurus_sp'
# nation_pre$bmsy[76:90] <- 'Istiophurus_sp'


#need to replace the values in fis_test with the correct values from county_catch using merge/match
#this step only needs to be done once to correct fis_test

nation_pre$total <- county_catch$national_total[match(paste(nation_pre$Year,nation_pre$species), paste(county_catch$Year,county_catch$species))]

#most values match, except for sphyranea 2011, and all rastrelliger


nation_pre<-nation_pre[,c(1,6,3,4,5)]

colnames(nation_pre)[2]<-'catch'

write.csv(nation_pre,"fis_national_catch.csv",row.names = F)


# calculate b_bmsy --------------------------------------------------------
options(scipen = 999)   #makes r not use scientific number form


nation<-read.csv(file ='fis_national_catch.csv',header = T,stringsAsFactors = F)

nation$biomass<-NA
nation$harvest_rate<-NA

#remove 2015,2016 values to match global range of up to 2014

nation<-nation[-which(nation$Year==2015|nation$Year==2016),]


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

nation4$rgn_id <- rep_len(1:5, length.out=nrow(nation4)/length(unique(nation4$species)))

nation4<-nation4[order(nation4$rgn_id),]

write.csv(nation4,"fish_catch_biomass_ken2018.csv",row.names = F)

final<-nation4[,c(9,1,4,8)]

write.csv(final,'fis_b_bmsy_ken2018.csv',row.names = F)

# calculate mean_catch per species per region ----------------------------------------

catch=read.csv('fis_catch_per_county_ken2018.csv',header = T,stringsAsFactors = F)

#remove 2015,2016 values to match global range of up to 2014

catch<-catch[-which(catch$Year==2015|catch$Year==2016),]

catch2<-catch %>%
  group_by(rgn_id,species)%>%
 mutate(mean_catch=mean(catch))

catch3<-catch2[,c(1,3,2,5)]

colnames(catch3)[3]<-'stock_id_taxonkey'

catch3<-catch3[order(catch3$rgn_id),]


write.csv(catch3,"fis_meancatch_ken2018.csv",row.names = F)
