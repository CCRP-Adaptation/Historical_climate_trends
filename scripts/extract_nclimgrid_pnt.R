library(dplyr)

rm(list=ls())

Lat = 35.916778769
Lon = -106.51961454
buffer = 0.05

AoAextent = terra::ext(Lon-buffer, Lon+buffer, Lat-buffer, Lat+buffer)

nclim_dir = "C:/Users/arunyon/3D Objects/Local-files/NOAA-data/nclim_2024/"

tmean = read.table(paste0(nclim_dir,"202503.tave.conus.pnt"))
ppt  = read.table(paste0(nclim_dir,"202503.prcp.conus.pnt"))
data = merge(tmean,ppt,by=c("V1", "V2"))
colnames(data) = c("Lat", "Lon", "TavgC","PptMm")


# Extract row that corresponds with coordinates

# Find nearest grid cell
get_nearest <- function(target, grid) {
  grid[which.min((grid$Lon - target$Lon)^2 + (grid$Lat - target$Lat)^2), ]
}

nearest_cell <- get_nearest(target = data.frame(Lon, Lat), grid = data)

nearest_cell$TavgF = (nearest_cell$TavgC * (9/5)) + 32
nearest_cell$PptIn = nearest_cell$PptMm / 35.4


### Test extracting .nc

src <- tidync(paste0(nclim_dir,"nclimgrid-prcp.nc"))  |> 
  hyper_filter(lat = lat <= c(Lat+buffer) & lat >= c(Lat-buffer)) |>  #subset to Lat/Lon
  hyper_filter(lon = lon <= c(Lon+buffer) & lon >= c(Lon-buffer)) %>% #aggregate lat and lon
  hyper_tibble() %>% 
  mutate(Date = as.Date(time,origin = "1800-01-01")) |> 
  group_by(Date) %>% 
  summarise_at(1,mean) 

if (i == 1){df<-src} else {df<-merge(df,src,by="Date")}


file.list = list.files(path = nclim_dir, pattern = '.nc', full.names = TRUE)
file.list = file.list[1:2] # just prcp and tavg

nc.data <- data.frame()
for (i in 1:length(file.list)){
  raster.obj = terra::rast(file.list[i])
  raster.extract<-terra::extract(raster.obj, vect(centroid))
  raster.extract$ID <- centroid$UNIT_CODE
  colnames(raster.extract)[2:length(raster.extract)] = as.character(time(raster.obj))
  raster.extract <- raster.extract |> 
    pivot_longer(!ID, names_to = "Date", values_to = substr(file.list[i], 72, 75)) #numbers correspond with vector length, if filename changes need to update
  if(i==1){nc.data=raster.extract} else{nc.data=left_join(nc.data,raster.extract,by=c("ID","Date"))}
}

baseData <- nc.data %>% mutate(PptIn = prcp/25.4,
                               # TmaxF = tmax * 9/5 + 32,
                               # TminF = tmin * 9/5 + 32, 
                               TavgF = tavg * 9/5 + 32,
                               Date = as.Date(Date,format="%Y-%m-%d"),
                               YearMon = paste0(year(Date),sprintf("%02d",month(Date))),
                               Year = as.integer(format(Date,format="%Y"))) %>% 
  filter(Date <= paste0(EndYr,"-12-01"))

