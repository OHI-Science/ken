#Working with Rasters...Plotting and clipping Rquires- rgdal and raster R libraries for this to work

knitr::opts_chunk$set(message=F,warning=F, fig.width = 8, fig.height = 6,strip.white=TRUE)

options(scipen = 999) #this forces reporting in non scientific notation
##############################################################################################

library(rgdal)
library(tidyverse)
library(raster)

#Defining the wgs84 coordinate reference system
wgs_crs <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

#.........Set regions of interest..........#

par(mfrow=c(1,2)) #set plotting window to show 2 plots side by side

#read in the ROI shapefile and reproject to Mollweide
rgn  = readOGR('KENOHI.shp')

plot(rgn,main = "KENOHI regions \n WGS84 projection")

#define the mollweide projection coordinate reference system (crs)
mollCRS=crs('+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs')

#reproject the shapefile to mollweide using spTransform
rgn_moll = spTransform(rgn, mollCRS)

#plot
plot(rgn_moll, main = "KENOHI regions \n Mollweide projection")



#########################################################################################
#...Bring in the your RASTER....#

myraster <- raster('chemical_pollution_2009.tif')

plot(myraster,main="Define yor title",axes=F, legend.args=list(text='define your legend', side=4, font=2, line=2.5, cex=0.8))
plot(rgn_moll,add=T)

########################################################################################
#Crop global data to your region

myraster_crop <- crop(myraster,rgn_moll)

plot(myraster_crop,axes=F,
     legend.args=list(text='Define your text', side=4, font=2, line=2.5, cex=0.8))
plot(rgn_moll,add=T)
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

df['year']="2009"
#########export csv
write.csv(df, file = "Chemical pollution 2009.csv")

#################################################################################################
#import multile csv and colum bind them-files needs to be in one folder
temp = list.files(pattern="*.csv")
myfiles = lapply(temp, read.delim)


myMergedData <- 
  do.call(rbind,
          lapply(list.files(path = "C:/Users/James/Desktop/test2"), read.csv))



write.csv(myMergedData, file = "Chemical pollution combinedrow.csv")
