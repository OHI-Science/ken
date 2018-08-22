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
rgn  = readOGR("D:/git/ken/prep/pressures/Population/coastline _buffer/ken_25_mile_coastal_buffer.shp")  #navigates from ken folder

plot(rgn,main = "25 mile buffer \n WGS84 projection")

#########################################################################################
#...Bring in the your RASTER....#

mypop2015<-raster("D:/git/ken/prep/pressures/Population/gpw-v4-population-count_2015.tif")

 plot(mypop2015,main="Define your title",axes=F, legend.args=list(text='define your legend', side=4, font=2, line=2.5, cex=0.8))
 plot(rgn,add=T)

########################################################################################
#Crop global data to your region

myraster_crop <- crop(mypop2015,rgn)

plot(myraster_crop,axes=F,
     legend.args=list(text='Define your text', side=4, font=2, line=2.5, cex=0.8))
plot(rgn,add=T)
##################################################################################################
#Get regional data
#There are various ways of getting the data you might for each of your subregions.
#Here we provide two ways of getting the average number of anomalous weeks per subregion
#using raster::extract() and raster::zonal().extract()

# get all values within each region
vals = extract(myraster_crop,rgn,method='simple')%>%
  setNames(rgn@data$rgn_name)

# plot distribution of data per region

df <- data.frame(unlist(vals))%>%
  rename(value = unlist.vals.)%>%
  mutate(rgn_name = gsub("\\d+", "",row.names(.))) #the gsub here removes all numbers from the rgn_name

#now we have each value assigned to rgn_name.
head(df)


#Add new col to data frame and populate it with year

df["year"]<-2015

#################################################################################################
#use ddply to get total populatio for each region in 2015

total_df <- df%>%
  group_by(rgn_name)%>%
  summarise(total_pop2015 = sum(value,na.rm=T))

total_df


#create a directory "total population" and export population data to csv
dir.create(file.path('D:/git/ken/prep/pressures/Population/','Extracted_regional_value _csv'), showWarnings = FALSE) #creates new sub folder


write.csv(total_df,"D:/git/ken/prep/pressures/Population/Extracted_regional_value _csv/ohi ken tot human pop 2015.csv",row.names = F)
