#Working with Rasters...Plotting and clipping Rquires- rgdal and raster R libraries for this to work

knitr::opts_chunk$set(message=F,warning=F, fig.width = 8, fig.height = 6,strip.white=TRUE)

options(scipen = 999) #this forces reporting in non scientific notation
##############################################################################################
library(here)   #install.packages('here')
library(rgdal)
library(tidyverse)
library(raster)

#Defining the wgs84 coordinate reference system
wgs_crs <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

#.........Set regions of interest..........#

par(mfrow=c(1,2)) #set plotting window to show 2 plots side by side

# setwd(here::here('Chemical pollution'))   #setwd to a file path we all have - doesn't work, cant change wd

#read in the ROI shapefile and reproject to Mollweide
setwd(here::here('prep/pressures/Nutrient pollution/'))

rgn  = readOGR('Shapefiles for clipping Ken/KENOHI.shp')  #navigates from ken folder

plot(rgn,main = "KENOHI regions \n WGS84 projection")

#define the mollweide projection coordinate reference system (crs)
mollCRS=CRS('+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs')

#reproject the shapefile to mollweide using spTransform
rgn_moll = spTransform(rgn, mollCRS)

#plot
plot(rgn_moll, main = "KENOHI regions \n Mollweide projection")



#########################################################################################
#...Bring in the your RASTER....#

#want to create a loop that repeats all these steps for each year of global data

#NOTE: MUST ALTER FOLDER NAME BASED ON WHERE GLOBAL TIF FILES ARE SAVED - SUBSTITUTE 'global tif' in lines below with folder name
#reason for this is global files are too large to upload onto github so each person will have it located in different folder

f <- list.files("F:/ohi datasets",pattern=".tif")     #lists all files in target folder with .tif in file name

for (i in 1:length(f)){

myraster <- raster(paste('F:/ohi datasets',f[i],sep=""))

# plot(myraster,main="Define your title",axes=F, legend.args=list(text='define your legend', side=4, font=2, line=2.5, cex=0.8))
# plot(rgn_moll,add=T)

########################################################################################
#Crop global data to your region

myraster_crop <- crop(myraster,rgn_moll)

# plot(myraster_crop,axes=F,
#      legend.args=list(text='Define your text', side=4, font=2, line=2.5, cex=0.8))
# plot(rgn_moll,add=T)
##################################################################################################
#Get regional data
#There are various ways of getting the data you might for each of your subregions.
#Here we provide two ways of getting the average number of anomalous weeks per subregion
#using raster::extract() and raster::zonal().extract()

# get all values within each region
vals = extract(myraster_crop,rgn_moll,method='simple')%>%
  setNames(rgn_moll@data$rgn_name)

# plot distribution of data per region

df <- data.frame(unlist(vals))%>%
  rename(value = unlist.vals.)%>%
  mutate(rgn_name = gsub("\\d+", "",row.names(.))) #the gsub here removes all numbers from the rgn_name

#now we have each value assigned to rgn_name.
head(df)


#Add new col to data frame

df['year']<-gsub(".*[_]([^.]+)[.].*", "\\1", f[i])    #keep only the year from the file name


#########export csv
dir.create(file.path('./prep/pressures/Nutrient pollution/', 'data layers'), showWarnings = FALSE) #creates new sub folder

write.csv(df, file = paste('./prep/pressures/Nutrient pollution/Extracted_regional_value _csv//',gsub("\\..*","",f[i]),'.csv',sep=""))   #removes the .tif from file name and replaces with .csv

if (i==1){
  regional_values<-df
}else{
regional_values<-rbind(regional_values,df)
}

}

write.csv(regional_values,"./prep/pressures/Nutrient pollution/Extracted_regional_value _csv/Kenya_all_values_per_region_all_years.csv",row.names = F)

#################################################################################################
#use ddply to get average chemical pollution levels across years for each region
library(plyr)

regional_values<-read.csv("Extracted_regional_value _csv/Kenya_all_values_per_region_all_years.csv",header = T,stringsAsFactors = F)

#first step - yearly average per region
yearly_scores<-ddply(regional_values,c("rgn_name","year"),summarise,
                       year_ave=round(mean(value,na.rm=T),3))

yearly_scores$rgn_id<-NA
yearly_scores$rgn_id[which(yearly_scores$rgn_name=='Mombasa')]<-1
yearly_scores$rgn_id[which(yearly_scores$rgn_name=='Kwale')]<-2
yearly_scores$rgn_id[which(yearly_scores$rgn_name=='Kilifi')]<-3
yearly_scores$rgn_id[which(yearly_scores$rgn_name=='Tana River')]<-4
yearly_scores$rgn_id[which(yearly_scores$rgn_name=='Lamu')]<-5
yearly_scores$rgn_id[which(yearly_scores$rgn_name=='EEZ')]<-6

yearly_scores2<-yearly_scores[,c(4,2,3)]
colnames(yearly_scores2)[3]<-'pressure_score'
yearly_scores2<-yearly_scores2[order(yearly_scores2$rgn_id),]


write.csv(yearly_scores2,"po_nutrients_12nm_ken2018.csv",row.names = F)

#second step - overall average across years per region

regional_scores<-ddply(yearly_scores,c("rgn_name"),summarise,
                       reg_ave=mean(year_ave,na.rm=T))

write.csv(regional_scores,"data layers/Kenya_ave_nutrient_regional_values.csv",row.names = F)
