
## nClimGrid Multiple Location Extraction Code

# Example used is extracting nClimGrid data for Karner Blue Butterly sites 
# Adapted from extract-site-format-trends.R script from climate futures repo

# Last update: Feb 24, 2022 by GJK 


# This script extracts historical data from NOAA ClimGrid
# This data needs to be downloaded one time, and may be stored on an external hard drive or locally. 

# Set working directory to location with R script and where you want output 

setwd("~/One-offs/KBB-site-extraction")


library(raster); library(sp); library(tidyverse);
library(here); library(plyr); # Use here::here when package lubridate is used
library(plotrix); library(zoo); library(ggplot2); library(grid); library(cowplot); library(reshape2); library(raster); library(ncdf4); library(reshape2); library(WriteXLS); library(data.table); library(RColorBrewer); library(ggrepel); library(lubridate); library(dplyr); library(forcats); library(openxlsx); library("WaterBalance"); library(sf); library(raster); library(rgdal); library(R.utils); library(tmap); library(tmaptools); library(ggmap); library(ggspatial);
library(gridExtra); library(SPEI); library(tidyr); library(tibble); library(sp); library(skimr); library(cft)

# Load in CSV of point locations or create a dataframe w/ sites. 

kbb_sites <- read.csv("C:\\Users\\gknowlton\\OneDrive - DOI\\Documents\\One-offs\\KBB-site-extraction\\KBB_sites.csv")

# adding site ID to identify in output

kbb_sites_csv <- kbb_sites %>% 
  mutate(siteID = paste0("site_", c(1:23)))

kbb_sites <- kbb_sites %>% 
  mutate(siteID = paste0("site_", c(1:23)))

# define coordinates from dataframe, get CRS, and project. 
coordinates(kbb_sites) <- ~Lat+Lon
crs(kbb_sites)
proj4string(kbb_sites) <- CRS("+init=epsg:4326")

# creates an empty list of the length of your list of sites, and add buffer of 0.06 degrees to each site for extraction. 
e <- list()
for (i in 1:length(kbb_sites)) {
  e[[i]] <- extent(kbb_sites[i,])
  
  e[[i]]@xmax = e[[i]]@xmax + 0.06
  e[[i]]@xmin = e[[i]]@xmin - 0.06
  e[[i]]@ymax = e[[i]]@ymax + 0.06
  e[[i]]@ymin = e[[i]]@ymin - 0.06
}

# assign site IDs to the list 
names(e) <- names(kbb_sites_csv$siteID)

# Set data directory to where your nClimGrid data is stored on external harddrive
data.dir <- "D:/NOAA-nClimGrid"

park_specific = './data/park-specific' # Create folder for park-specific data (i.e. parsed data and output)
if(dir.exists(park_specific) == FALSE){
  dir.create(park_specific)
}

parsed_data = './data/park-specific/parsed-data'
if(dir.exists(parsed_data) == FALSE){
  dir.create(parsed_data)
}

OutDir <- paste0(parsed_data, "/")

# set your desired variables to be extracted 
var.list = c("tmax", "tmin", "tave", "prcp")

for (v in 1:length(var.list)){
  DF <- data.frame()
  var = var.list[v]
  print(paste0("extracting ",var))
  pnt.list<-list.files(path=data.dir, pattern=paste(var, "conus", sep = ".")) #list all files by var
  #tmax, tmin, tave, prcp
  
  # Create list of tables
  
  for(i in 1:length(pnt.list)){
    for(b in 1:length(e)){
      t = read.table(paste(data.dir, pnt.list[i], sep = '/'))
      yrmon = substr(pnt.list[i], 1, 6) 
      colnames(t) = c("Lat","Lon", var)
      tt = subset(t, Lon >= e[[b]]@ymin & Lon <= e[[b]]@ymax &
                    Lat >= e[[b]]@xmin & Lat <= e[[b]]@xmax)
      tt$Date = as.Date(paste0(yrmon,"01"),format="%Y%m%d")
      
      tt.merge = aggregate(.~Date,tt,mean)
      tt.merge$YearMon <- yrmon
      tt.merge$siteID = paste0("site_", b) 
      DF = rbind(DF,tt.merge)
    }
    assign(paste0(var,".data"), DF)
    write.csv(DF,paste0(OutDir,var,".csv"),row.names = F)
  }
}
