#script to clean and assign rgn_id codes to fish datasets

library(here)
#steps:

setwd(here::here('prep/FIS/'))

bmsy<-read.csv("fis_b_bmsy_ken2018_no_id.csv",header = T,stringsAsFactors = F)

colnames(bmsy)

#make sure there are the same years of data for each region and species
ftable(bmsy$rgn_id)
ftable(bmsy$stock_id)
ftable(bmsy$year)

colnames(bmsy)[1]<-'rgn_name'

unique(bmsy$rgn_name)

bmsy$rgn_id<-NA
bmsy$rgn_id[which(bmsy$rgn_name=='Mombasa')]<-1
bmsy$rgn_id[which(bmsy$rgn_name=='Kwale')]<-2
bmsy$rgn_id[which(bmsy$rgn_name=='Kilifi')]<-3
bmsy$rgn_id[which(bmsy$rgn_name=='Tana river')]<-4
bmsy$rgn_id[which(bmsy$rgn_name=='Lamu')]<-5

unique(bmsy$rgn_id)

bmsy1<-bmsy[,c(5,2,3,4)]

bmsy1<-bmsy1[order(bmsy1$rgn_id),]

write.csv(bmsy1,"fis_b_bmsy_ken2018.csv",row.names = F)

#same for mean catch file

mcatch<-read.csv("fis_meancatch_ken2018_no_id.csv",header = T,stringsAsFactors = F)

colnames(mcatch)

#make sure there are the same years of data for each region and species
ftable(mcatch$rgn_id)
ftable(mcatch$stock_id)
ftable(mcatch$year)

colnames(mcatch)[1]<-'rgn_name'
colnames(mcatch)[2]<-'stock_id_taxonkey'

unique(mcatch$rgn_name)

mcatch$rgn_id<-NA
mcatch$rgn_id[which(mcatch$rgn_name=='Mombasa')]<-1
mcatch$rgn_id[which(mcatch$rgn_name=='Kwale')]<-2
mcatch$rgn_id[which(mcatch$rgn_name=='Kilifi')]<-3
mcatch$rgn_id[which(mcatch$rgn_name=='Tana river')]<-4
mcatch$rgn_id[which(mcatch$rgn_name=='Lamu')]<-5

unique(mcatch$rgn_id)

mcatch1<-mcatch[,c(5,2,3,4)]

mcatch1<-mcatch1[order(mcatch1$rgn_id),]

write.csv(mcatch1,"fis_meancatch_ken2018.csv",row.names = F)
