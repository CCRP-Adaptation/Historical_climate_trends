library(here); library(plyr); # Use here::here when package lubridate is used
library(plotrix); library(zoo); library(ggplot2); library(grid); library(cowplot); library(reshape2); library(raster); library(ncdf4); library(reshape2); library(WriteXLS); library(data.table); library(RColorBrewer); library(ggrepel); library(lubridate); library(dplyr); library(forcats); library(openxlsx); library(sf); library(raster); library(R.utils); library(tmap); library(tmaptools); library(ggmap); library(ggspatial);
library(gridExtra); library(SPEI); library(tidyr); library(tibble); library(sp); library(skimr);  library(stringr); library(ggpubr); library(lemon);library(rvest);library(tidyverse);library(XML);library(xml2);library(curl);library(tidync); library(viridis); library(robustbase)
library(terra) #https://tmieno2.github.io/R-as-GIS-for-Economists/extracting-values-from-raster-layers-for-vector-data.html
rm(list=ls())

centroids <- tibble(read.csv(here::here("NPS_CONUS.csv")))
SiteID = centroids$UNIT_CODE

centroid_county <- read.csv(here::here("centroid_county.csv"))
BeginYr = 2023
EndYr = 2024

DataDir <- "E:/nClimGrid_nc/"

nps_centroids = st_read("C:/Users/arunyon/OneDrive - DOI/Documents/GIS/nps_boundary_centroids_2023/nps_boundary_centroids.shp")

centroid <- filter(nps_centroids, UNIT_CODE %in% SiteID)
centroid <- centroid[2]
centroid = st_transform(centroid, crs = 4326)

file.list = list.files(path = DataDir, pattern = '.nc', full.names = TRUE)
file.list = file.list[2]
# file.list = file.list[1:2] # just prcp and tavg

  raster.obj = terra::rast(file.list)
  RA = subset(raster.obj, time(raster.obj) > as.Date("2022-12-01"))

  raster.extract = pbapply::pblapply(1:nrow(centroid), function(i) extract(RA, centroid[i, , drop = FALSE]), cl = 6) #extracts in parallel and gives progress bar
  # Took 2:53 minutes to get 2023-2025 data
  
  
  for(i in 1:nrow(centroid)){
    raster.extract[[i]]$ID = centroid$UNIT_CODE[i]
    colnames(raster.extract[[i]])[2:length(raster.extract[[i]])] = as.character(time(RA))
  }
  raster.extract.new = do.call(rbind,raster.extract)
  
  raster.extract <- raster.extract.new |> 
    # pivot_longer(!ID, names_to = "Date", values_to = substr(file.list[i], 72, 75)) #numbers correspond with vector length, if filename changes need to update
    pivot_longer(!ID, names_to = "Date", values_to = "TavgC") %>% 
    mutate(TavgF = TavgC * 9/5 + 32,
           Date = as.Date(Date,format="%Y-%m-%d"),
           YearMon = paste0(year(Date),sprintf("%02d",month(Date))),
           Year = as.integer(format(Date,format="%Y")))
 

write.csv(raster.extract,"C:/Users/arunyon/Downloads/historical.centroids_2023-2025.csv",row.names=F)
