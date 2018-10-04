library(here)
setwd(here::here('prep/FIS/'))

biomass<-read.csv('b_bmsy_ken2018.csv',header = T,stringsAsFactors = F)

unique(biomass$species)

#biomass$species[which(biomass$species==)]<-"Istiophorus_platypterus-51"
biomass$species[which(biomass$species=="Rastrelliger_kanarguta")]<-"Rastrelliger_kanagurta-51_600111"
biomass$species[which(biomass$species== "Siganus_sutor" )]<-"Siganus-51_500240"
biomass$species[which(biomass$species=="Sphyraena_sp")]<-"Sphyraena-51_500372"
biomass$species[which(biomass$species=="Panulirus_sp")]<-"Panulirus-51"
biomass$species[which(biomass$species=="Peneius_indicus")]<-"Peneius_indicus-51"


biomass_global<-read.csv("Filtered_b_bmsy_OHI_global.csv",header = T,stringsAsFactors = F)

#plan is to add the values for bbmsy for each region, species, year in global biomass with local biomass
#all 5 species we have bbmsy values are not included in global biomass file

#match colnames
colnames(biomass)<-colnames(biomass_global)

final_biomass<-rbind(biomass_global,biomass)

#NEED TO REMOVE 2001 from global datasets

final_biomass<-final_biomass[-which(final_biomass$year==2001),]

final_biomass<-final_biomass[order(final_biomass$rgn_id),]

write.csv(final_biomass,"fis_b_bmsy_ken2018.csv",row.names = F)

# mean_catch --------------------------------------------------------------
#plan is to replace the values for bbmsy for each region, species, year in global mean_catch with local mean_catch

catch<-read.csv("meancatch_ken2018.csv",header = T,stringsAsFactors = F)

unique(catch$stock_id_taxonkey)

catch$stock_id_taxonkey[which(catch$stock_id_taxonkey=="Istiophurus_sp")]<-"Istiophorus_platypterus-51"
catch$stock_id_taxonkey[which(catch$stock_id_taxonkey=="Rastrelliger_kanarguta")]<-"Rastrelliger_kanagurta-51_600111"
catch$stock_id_taxonkey[which(catch$stock_id_taxonkey== "Siganus_sutor" )]<-"Siganus-51_500240"
catch$stock_id_taxonkey[which(catch$stock_id_taxonkey=="Sphyraena_sp")]<-"Sphyraena-51_500372"
catch$stock_id_taxonkey[which(catch$stock_id_taxonkey=="Panulirus_sp")]<-"Panulirus-51"
catch$stock_id_taxonkey[which(catch$stock_id_taxonkey=="Peneius_indicus")]<-"Peneius_indicus-51"


catch_global<-read.csv("Filtered_mean_catch_OHI_global.csv",header = T,stringsAsFactors = F)

#plan is to add the values for bbmsy for each region, species, year in global catch with local catch
#all 5 species we have bbmsy values are not included in global catch file

#match colnames
colnames(catch)<-colnames(catch_global)

#for siganus, rastrelliger and sphyraena - catch data exists in global - match
#for the other 3 it does not, so will need to append that to global - rbind

#split the datasets into two - those which we can match and those we need to rbind

# catch_global$mean_catch2<-catch_global$mean_catch

catch_global$mean_catch2<-catch$mean_catch[match(paste(catch_global$rgn_id,catch_global$stock_id_taxonkey,catch_global$year),paste(catch$rgn_id,catch$stock_id_taxonkey,catch$year))]

catch_half<-catch[which(catch$stock_id_taxonkey=='Istiophorus_platypterus-51'|catch$stock_id_taxonkey=='Panulirus-51'|catch$stock_id_taxonkey=='Peneius_indicus-51'),]

library(plyr)
f<-rbind.fill(catch_global,catch_half)

#for all NA - meancatch=meancatch2
f$mean_catch2[which(is.na(f$mean_catch2))]<-f$mean_catch[which(is.na(f$mean_catch2))]

#NEED TO REMOVE 2001 VALUES

f1<-f[-which(f$year==2001),]

f2<-f1[,c(1,2,3,5)]

colnames(f2)[4]<-'mean_catch'

f2<-f2[order(f2$rgn_id),]


write.csv(f2,"fis_meancatch_ken2018.csv",row.names = F)
