library(stars)
library(dplyr)
library(ggplot2) #for plotting
library(units) # for dropping units
library(tidyverse)
library(starsdata)
library(viridis)
library(ncdf4)


rm(list=ls())

file.list = list.files(path = DataDir, pattern = '.nc', full.names = TRUE)

df <- data.frame(lon =Lon, lat = Lat)
coordinates(df) <- c("lon", "lat")
proj4string(df) <- CRS("+init=epsg:4326") 
df_sf <- st_as_sf(df)


GetSeason <- function(DateVec){
  seas <- as.character(rep(NA, length(DateVec)))
  seas[which(format(DateVec,'%B') %in% c("December", "January", "February"))]<- "Winter"
  seas[which(format(DateVec,'%B') %in% c("March", "April", "May"))]<- "Spring"
  seas[which(format(DateVec,'%B') %in% c("June", "July", "August"))]<- "Summer"
  seas[which(format(DateVec,'%B') %in% c("September", "October", "November"))]<- "Fall"
  return(seas)
}


cropped_st <- list() # create list for cropped stars objects
for(i in 1:length(file.list)){
  suppressMessages(
    l <- read_ncdf(file.list[i]) # need to read in as ncdf or coordinate system does not translate (not sure why)
      )
  nc_crop = st_extract(l, df_sf); rm(l)
  cropped = nc_crop; rm(nc_crop)
  cropped_st[[i]] <- st_as_stars(cropped); rm(cropped)
}

gc()

var_stars <- Reduce(c,cropped_st); rm(cropped_st)
var_stars$prcp <- drop_units(var_stars$prcp)
var_stars$tmax <- drop_units(var_stars$tmax)
var_stars$tmin <- drop_units(var_stars$tmin)
stars_df <- as.data.frame(var_stars)

rm(var_stars)

gc()
stars_df$geometry <- NULL
colnames(stars_df)[1] <- "Date"

baseData <- stars_df %>% mutate(PptIn = prcp/25.4,
                                TmaxF = tmax * 9/5 + 32,
                                TminF = tmin * 9/5 + 32, 
                                TavgF = (TmaxF+TminF)/2,
                                YearMon = paste0(year(Date),sprintf("%02d",month(Date))),
                                Season = GetSeason(Date))
rm(stars_df)
write.csv(baseData, (sprintf("%s%s_nClimGrid.csv", OutDir, SiteID)),row.names=FALSE)


# how to extract for all CONUS sites: https://r-spatial.github.io/stars/reference/st_extract.html

