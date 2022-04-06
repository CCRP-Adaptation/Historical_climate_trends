library(stars)
library(dplyr)
library(ggplot2) #for plotting
library(units) # for dropping units
library(tidyverse)
# library(starsdata)
library(viridis)
library(ncdf4)

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

# Using Tidync
for (i in 1:length(file.list)){
src <- tidync(file.list[i]) %>% 
  hyper_filter(lat = lat <= c(Lat+0.05) & lat >= c(Lat-0.05)) %>% #subset to Lat/Lon
  hyper_filter(lon = lon <= c(Lon +0.05) & lon >= c(Lon -0.05)) %>% #aggregate lat and lon
  hyper_tibble() %>% 
  mutate(Date = as.Date(time,origin = "1800-01-01")) %>% 
  group_by(Date) %>% 
  summarise_at(1,mean) 
if (i == 1){df<-src} else {df<-merge(df,src,by="Date")}
}

baseData <- df %>% mutate(PptIn = prcp/25.4,
                                TmaxF = tmax * 9/5 + 32,
                                TminF = tmin * 9/5 + 32, 
                                TavgF = (TmaxF+TminF)/2,
                                YearMon = paste0(year(Date),sprintf("%02d",month(Date))),
                                Season = GetSeason(Date)) %>% 
  filter(Date <= paste0(EndYr,"-12-01"))

rm(df,src)
write.csv(baseData, (sprintf("%s%s_nClimGrid.csv", OutDir, SiteID)),row.names=FALSE)


# how to extract for all CONUS sites: https://r-spatial.github.io/stars/reference/st_extract.html

