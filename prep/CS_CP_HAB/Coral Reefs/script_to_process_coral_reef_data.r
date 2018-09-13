#script will try to develop county level current status, trend and reference points for hard coral cover
library(here)
#steps:
#assign the correct county (maybe under sector) to each site
setwd(here::here('prep/CS_CP_HAB/Coral Reefs'))

ken<-read.csv("Kenya_2017_GCRMN_benthic_dataset.csv",header = T,stringsAsFactors = F)

unique(ken$Site)
f<-unique(ken[c('Site','Station')])
f<-f[order(f$Site),]

ken$Sector[which(ken$Site=='Diani_Chale')]<-'Kwale'
ken$Sector[which(ken$Site=='Diani-Chale')]<-'Kwale'
ken$Sector[which(ken$Site=='Southcoast')]<-'Kwale'
ken$Sector[which(ken$Site=='Kisite')]<-'Kwale'
ken$Sector[which(ken$Site=='Shimoni')]<-'Kwale'

ken$Sector[which(ken$Site=='Malindi')]<-'Kilifi'
ken$Sector[which(ken$Site=='Watamu')]<-'Kilifi'
ken$Sector[which(ken$Site=='Kilifi')]<-'Kilifi'
ken$Sector[which(ken$Site=='Malindi-Ugwana_bay')]<-'Kilifi'

ken$Sector[which(ken$Site=='Mombasa')]<-'Mombasa'

ken$Sector[which(ken$Site=='Kiunga_MNR')]<-'Lamu'
ken$Sector[which(ken$Site=='Lamu')]<-'Lamu'


# ken$Sector[which(ken$Site=='')]<-'Tana River'

#next step - look at temporal range of data per county (sector)
library(plyr)
tmp<-ddply(ken,c("Sector"),summarise,
           start_yr=min(Year),
           latest_yr=max(Year),
           n_years=length(unique(Year)),
           n_sites=length(unique(Station)))

#also look at number of sites for each year
ste<-ddply(ken,c("Sector","Year"),summarise,
           n_sites=length(unique(Station)))

#calculate yearly mean for each county - aggregate across stations

#sum algae categories to produce fleshy algae value for each station

#create a new column and change all algae column codes to FA

ken$benthic_code<-ken$level1_code
ken$benthic_code[which(ken$benthic_code=='AHAL')]<-'FA'
ken$benthic_code[which(ken$benthic_code=='ATRF')]<-'FA'
ken$benthic_code[which(ken$benthic_code=='AMAC')]<-'FA'
ken$benthic_code[which(ken$benthic_code=='ALG')]<-'FA'

#then for each station we have to sum by benthic_code and then we can aggregate across sectors

#this is to aggregate per station all algae to FA. n_test is to make sure that only FA has multiple values per station
station_sum<-ddply(ken,c("Country","Year","Sector","Site","Station","Period","benthic_code"),summarise,
                   mean_cover=sum(cover,na.rm = T),
                   n_test=length(cover))

unique(station_sum$benthic_code[which(station_sum$n_test>1)]) #only FA - all is okay

#this was done across the period break for 2016 - confirm if this is okay?
county_ave<-ddply(station_sum,c("Sector","Year","benthic_code"),summarise,
                   ave_cover=mean(mean_cover),
                  n_sites=length(unique(Station)))

#only take FA and HC

county_ave<-county_ave[which(county_ave$benthic_code=='HC'|county_ave$benthic_code=='FA'),]
county_ave$ave_cover<-round(county_ave$ave_cover,2)


#county level = count_ave dataframe
#calculate trend - 5 most recent years for each county (contains some gaps in years)
county_trend<-ddply(county_ave,c("Sector","benthic_code"),summarise,
recent_cover=tail(ave_cover,n=5),
years=tail(Year,n=5))


#calculate status/reference year
#this is only using 1 single year for ref point and status - can we use an average of 2 or 3 years
county_status<-ddply(county_ave,c("Sector","benthic_code"),summarise,
                    recent_cover=tail(ave_cover,n=1),
                    recent_Year=tail(Year,n=1),
                    n_site_recent=tail(n_sites,n=1),
                    reference_cover=head(ave_cover,n=1),
                    reference_Year=head(Year,n=1),
                    n_site_ref=head(n_sites,n=1))

#average over 2-3 years
county_status2<-ddply(county_ave,c("Sector","benthic_code"),summarise,
                     recent_cover=tail(ave_cover,n=3),
                     recent_Year=tail(Year,n=3),
                     n_site_recent=tail(n_sites,n=3),
                     reference_cover=head(ave_cover,n=3),
                     reference_Year=head(Year,n=3),
                     n_site_ref=head(n_sites,n=3))

county_status3<-ddply(county_status2,c("Sector","benthic_code"),summarise,
                      recent_cover_ave=round(mean(recent_cover),2),
                      reference_cover_ave=round(mean(reference_cover),2))

county_status$status<-county_status$recent_cover/county_status$reference_cover



#calculate trends
## minimum year here for illustration; it is based on data available
year_min = 2011

r.trend <- ry %>%
  filter(year >= year_min) %>%
  filter(!is.na(statusData)) %>%
  group_by(region_id) %>%
  arrange(year) %>%
  top_n(5, year) %>%
  ungroup()


r.trend <- r.trend %>%
  group_by(region_id) %>%
  do(mdl = lm(statusData ~ year, data=.)) %>%
  summarize( region_id = region_id,
             trend = coef(mdl)['year']*5) %>%
  ungroup()
