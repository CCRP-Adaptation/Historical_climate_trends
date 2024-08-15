
library(here); library(plyr); # Use here::here when package lubridate is used
library(plotrix); library(zoo); library(ggplot2); library(grid); library(cowplot); library(reshape2); library(raster); library(ncdf4); library(reshape2); library(WriteXLS); library(data.table); library(RColorBrewer); library(ggrepel); library(lubridate); library(dplyr); library(forcats); library(openxlsx); library(sf); library(raster); library(rgdal); library(R.utils); library(tmap); library(tmaptools); library(ggmap); library(ggspatial);
library(gridExtra); library(SPEI); library(tidyr); library(tibble); library(sp); library(skimr); library(cft); library(stringr); library(ggpubr); library(lemon);library(rvest);library(tidyverse);library(XML);library(xml2);library(curl);library(tidync); library(viridis); library(robustbase)
library(terra) #https://tmieno2.github.io/R-as-GIS-for-Economists/extracting-values-from-raster-layers-for-vector-data.html
rm(list=ls())


#centroids <- tibble(read.csv(here::here("NPS_CONUS.csv")))

nps_cent <- terra::vect("C:/Users/cmquinn/Documents/NPS stuff/Park_GIS_files/park-centroid/nps_boundary_centroids.gdb")
#for RSS, get Tmin and Tmax as well as Tavg and Prcp
#need data for SHEN, WHIS, and BICY
parks <- terra::subset(nps_cent, nps_cent$UNIT_CODE %in% c('SHEN', 'WHIS', 'BICY'))


#First batch of coastal/island "problem" sites
UNIT_CODE <- c('ASIS', 'CABR', 'RORI', 'POCH', 'FOPO', 'SAFR', 'BOHA', 'EBLA')
lon <- c(-75.2054224, -117.205198, -122.3668053, -122.030318, -122.4692684, -122.4194102,
         -70.8850286, -122.6822046)
lat <- c(38.1534803, 32.6898735, 37.9203354, 38.0558694, 37.7856887, 37.7871140, 42.2479112,
         48.2069119)
#Second batch of coastal/island "problem" sites
UNIT_CODE2 <- c("PARA", "FIIS", "GATE", "ROWI", "SAHI", "APIS", "SLBE", "GOIS", 
               "SACR", "PEVI", "BISC", "DESO", "FORA", "FOSU", "WRBR", "CAHA",
               "CALO", "CANA", "FOMA", "GUIS")
lon2 <- c(-113.6699351, -72.8462552, -74.1024276, -71.411628, -73.5029278, -90.8932316,
         -85.9177553, -74.0167188, -67.1458195, -82.8381793, -80.1894312, -82.6434952,
         -75.6815373, -79.8911594, -75.6586536, -75.5619126, -76.4375738, -80.8035282,
         -81.2323701, -87.0181166)
lat2 <- c(36.3681545, 40.762557, 40.5581628, 41.8361793, 40.8859853, 46.9337149,
         44.9324217, 40.6904497, 45.1222347, 41.5241219, 25.4729368, 27.5224682,
         35.9053793, 32.7395695, 35.9918642, 35.8208354, 34.739787, 28.8090672,
         29.7016012, 30.3541255)
               
#ROWI, SAHI, APIS, SLBE, GOIS, SACR, PEVI, BISC, DESO, FORA, FOSU, WRBR
#CAHA, CALO, CANA, FOMA, GUIS, PARA, PEVI, SACR
centroids <- data.frame(UNIT_CODE=UNIT_CODE, x=as.double(lon), y=as.double(lat))
centroids2 <- data.frame(UNIT_CODE=UNIT_CODE2, x=as.double(lon2), y=as.double(lat2))

centroidssf <- st_as_sf(x=centroids, coords = c("x", "y"), crs="EPSG:4326")
centroids2sf <- st_as_sf(x=centroids2, coords = c("x", "y"), crs="EPSG:4326")
#combine them - these are generally coastal and island parks
centroidssf_coisl <- rbind(centroidssf, centroids2sf)
plot(centroidssf, max.plot=1)
plot(centroidssf_coisl, max.plot=1)
st_write(centroidssf_coisl, dsn="C:/Users/cmquinn/DOI/CCRP COLLABORATE! - Chris/multipark_historical-problemsites/point locations used/CF_summary_CONUS_coast_isl_sites.shp",
         driver="ESRI Shapefile")

#SiteID = centroids$UNIT_CODE

#centroid_county <- read.csv(here::here("centroid_county.csv"))
BeginYr = 1895
EndYr = 2022

endRefYr = 1970
# needed for rolling mean plot below.  
stepYrs	= 10		  # for period plots 
rollLen = 10      # period of calc for rolling average; default 10 = decadal
dpi = 600 

# DataDir <- "E:/nClimGrid_nc/"
DataDir <- "C:/Users/cmquinn/Documents/NOAA-data/nclimgrid_230301/"
LocalDir <- "C:/Users/cmquinn/Documents/repos/Historical_climate_trends/multipark_historical-v2/"

#nps_centroids <- st_read('C:/Users/arunyon/3D Objects/Local-files/Git-repos/CCRP_automated_climate_futures/data/general/spatial-data/nps_boundary_centroids/nps_boundary_centroids.shp')
nps_centroids <- st_read('C:/Users/cmquinn/Documents/repos/CCRP_automated_climate_futures/data/general/spatial-data/nps_boundary_centroids.gdb')
SiteID = c('SHEN', 'WHIS', 'BICY')
centroid <- filter(nps_centroids, UNIT_CODE %in% SiteID)
test <- nps_centroids[2]

# #Use this block for areas that are not in centroids file
# LocalDir <- "C:/Users/arunyon/3D Objects/Local-files/RCF_Testing/FOMR-BLSC/"
# SiteID <- c("FOMR", "BLSC")
# DT <- data.table(
#   UNIT_CODE=SiteID,
#   latitude=c(37.472918, 30.305797),
#   longitude=c(-76.322860, -104.022553))
# c = st_as_sf(DT, coords = c("longitude", "latitude"),
#                  crs = 4269, agr = "constant")
# centroid=c

file.list = list.files(path = DataDir, pattern = '.nc', full.names = TRUE)
#file.list = file.list[1:2] # just prcp and tavg
file.list
nc.data <- data.frame()
for (i in 1:length(file.list)){
  raster.obj = terra::rast(file.list[i])
  raster.extract<-terra::extract(raster.obj, parks)
  raster.extract$ID <- parks$UNIT_CODE
  colnames(raster.extract)[2:length(raster.extract)] = as.character(time(raster.obj))
  raster.extract <- raster.extract |> 
    pivot_longer(!ID, names_to = "Date", values_to = substr(file.list[i], 65, 68)) #numbers correspond with vector length, if filename changes need to update
  if(i==1){nc.data=raster.extract} else{nc.data=left_join(nc.data,raster.extract,by=c("ID","Date"))}
}


#alt approach - selects the cell around the lon/lat using tidync
for (j in 1:length(centroidssf$UNIT_CODE)){
  SiteID = centroidssf$UNIT_CODE[j]
  Lat = centroidssf$Lat[which(centroidssf$UNIT_CODE %in% SiteID)]
  Lon = centroidssf$Lon[which(centroidssf$UNIT_CODE %in% SiteID)]
for (k in 1:length(file.list)){
  nc.data <- tidync(file.list[k]) %>% 
    hyper_filter(lat = (Lat <= c(Lat+0.02) & Lat >= c(Lat-0.02))) %>% #subset to Lat/Lon
    hyper_filter(lon = (Lon <= c(Lon+0.02) & Lon >= c(Lon-0.02))) %>% #aggregate lat and lon
    hyper_tibble() %>% 
    mutate(Date = as.Date(time,origin = "1800-01-01"), UNIT_CODE = SiteID) %>% 
    group_by(Date) %>% 
    summarise_at(1,mean) 
  if (k == 1){df<-nc.data} else {df<-merge(df,nc.data,by="Date")}
  
  if (j == 1){stacked <- df} else {stacked<-rbind(stacked, df)}
}
  #extract the nclim data
  nclim_all <- terra::rast(file.list)
  nclimextract<-terra::extract(nclim_all, vect(parks))
  #nclimextdf <- as.data.frame(nclimextract)
  nclimextdf <- 
    nclimdaterange <- seq(as.Date("1895-01-01"), to=as.Date("2023-01-01"),by = "month")
  nclimvars <- c('prcp', 'tavg', 'tmin', 'tmax')
  
  #reshape from wide to long for each park
  for (i in 1:length(nclimvars)){
      nclimvar = nclimvars[i]
      nclimwide0 <- cbind(as.data.frame(UNIT_CODE), dplyr::select(nclimextdf, 'ID'), 
                          dplyr::select(raster.extract, starts_with(paste(nclimvar))))
      colnames(nclimwide0)[3:1539] = c(nclimdaterange)
      long <- reshape2::melt(nclimwide0, id.vars = c("UNIT_CODE", "ID"), 
                             value.name=paste(nclimvar)) %>%
        mutate(Date = as.Date(as.numeric(paste(variable)), 
                              format="%Y-%m-%d", origin="1970-01-01"),
               UNIT_CODE==SiteID) %>% 
        filter(year(Date)<2023) %>%
        dplyr::select(all_of(c("UNIT_CODE", "ID", "Date", paste(nclimvar)))) %>%
        arrange(UNIT_CODE, ID, Date, nclimvar) 
      if (i == 1){ nclimlong<-long } else {
        nclimlong <- merge(nclimlong, long, by=c("UNIT_CODE", "ID", "Date"))
        rm(nclimwide0, long)
      }
    }
  
  
baseData <- nc.data %>% group_by(ID) %>% mutate(PptIn = prcp/25.4,
                                TmaxF = tmax * 9/5 + 32,
                                TminF = tmin * 9/5 + 32, 
                               TavgF = tavg * 9/5 + 32,
                               Date = as.Date(Date,format="%Y-%m-%d"),
                               YearMon = paste0(year(Date),sprintf("%02d",month(Date))),
                               mon = month(Date),
                               Year = as.integer(format(Date,format="%Y")),
                               UNIT_CODE = ID) %>% 
  filter(Date <= paste0(EndYr,"-12-01"))
table(baseData$ID)
write.csv(baseData,paste0(LocalDir,"RSS_parks_summer_2024_historical.csv"),row.names=F)

baseData$ID = baseData$UNIT_CODE
Annual = baseData |> group_by(ID,Year) |> summarise(PptIn = sum(PptIn),
                                                    TavgF = mean(TavgF),
                                                    TminF = mean(TminF),
                                                    TmaxF = mean(TmaxF))
Annual$PptP2 <- ifelse(Annual$Year>=endRefYr, Annual$PptIn, NA)
Annual$TavgP2  <- ifelse(Annual$Year>=endRefYr, Annual$TavgF, NA)
Annual$TminP2 <- ifelse(Annual$Year>=endRefYr, Annual$TminF, NA)
Annual$TmaxP2  <- ifelse(Annual$Year>=endRefYr, Annual$TmaxF, NA)
write.csv(Annual, paste0(LocalDir,"Annual-Averages.csv"), row.names=FALSE)

# Annual <- read.csv(paste0(LocalDir,"Annual-Averages.csv"))
# Annual$Year <- as.Date(as.character(Annual$Year), format = "%Y")

#### Basic plot
# Take out the fuzzy gray error bar shading for the blue lines.
# Recolor the 1970’s trend line and take out the dashed blue line behind it from 1900-1970
# Consider taking out the red rolling average line 
# Add “2020” to the x-axis labels

PlotTheme = theme_gray() %+replace% 
  theme(plot.title = element_text(size=18, face='bold', hjust=0.5, vjust=0.5),
        axis.text.y = element_text(size = 16, colour="black"),
        axis.title.y = element_text(size = 18, angle = 90, margin=margin(0,5,0,0)),
        axis.text.x = element_text(size = 16, colour="black"),
        axis.title.x = element_text(size = 18, margin=margin(5,0,0,0)),
        legend.position = "none",
        legend.title = element_text(size=16),
        legend.text = element_text(size=16)
  )

theme_set(PlotTheme)
TitleSize = theme_get()$plot.title$size  ##Needed for cowplot layouts

lmMetrics <- function(lmout){
  s <- summary(lmout)
  # equ <- as.character(s$call)
  # eq <- equ[2]
  YrCoeff <- s$coefficients[2,1]
  ses <- coef(s)[,"Std. Error"]   # gets intercept & slope
  seSlope <- ses[2]
  probCoeff <- s$coefficients[2,4]
  probSign <- probStar(probCoeff)
  r2 <- s$r.squared
  data.frame(YrCoeff,seSlope,probCoeff, probSign, r2)
}

##########################
PlotName = "Annual Means Lines Regressions"
iter1k = lmrob.control(k.max = 1000)
for(i in 1:length(SiteID)){
  # SiteID=SiteID[i]
  
  A <- Annual |> filter(ID == SiteID[i])
  #Regressions for trends----
   lmTmax <- lmrob(TmaxF~Year,A, control=iter1k)
   lmTmaxP2 <- lmrob(TmaxP2~Year,A, control=iter1k)
  
   lmTmin <- lmrob(TminF~Year,A, control=iter1k)
   lmTminP2 <- lmrob(TminP2~Year,A, control=iter1k)
  
  lmTmean <- lmrob(TavgF~Year,A,  control=iter1k)
  lmTmeanP2 <- lmrob(TavgP2~Year,A,  control=iter1k)
  
  lmPpt  <- lmrob(PptIn~Year,A, control=iter1k)
  lmPptP2 <- lmrob(PptP2~Year,A, control=iter1k)
  
  # make table of coefficients
  probStar <- function(pVal){
    probStar <- "NS"
    if(pVal < 0.05)probStar <- "*"
    if(pVal < 0.01)probStar <- "**"
    if(pVal < 0.001)probStar <- "***"
    probStar
  }
  
  regsTmax <-  rbind(lmMetrics(lmTmax), lmMetrics(lmTmaxP2))
  regsTmin <-  rbind(lmMetrics(lmTmin), lmMetrics(lmTminP2))
  regsTmean <- rbind(lmMetrics(lmTmean),lmMetrics(lmTmeanP2))
  regsPpt <-   rbind(lmMetrics(lmPpt),lmMetrics(lmPptP2))
  
  perAll <- paste(min(Annual$Year), max(Annual$Year), sep="-")
  per2 <- paste(endRefYr, max(Annual$Year), sep="-")
  Period <- rep(c(perAll, per2), 2)
  
  lmTable <- cbind( Var=rep(c("Tmean", "Precip", "Tmin", "Tmax"),each=2), Period, rbind(regsTmean, regsPpt, regsTmin, regsTmax))
  
  lmTable$YrCoeff <- lmTable$YrCoeff * 100   # convert to degF(in)/100-yrs
  lmTable$seSlope <- lmTable$seSlope * 100
  #add units to YrCoeff field
  colnames(lmTable) <- c("Var", "Period", "YrCoeff(degF(in)/100yrs)", "seSlope", "probCoeff", "probSign", "r2")
  
  print(lmTable, row.names = F)
  
  write.csv(lmTable, paste0(LocalDir, SiteID[i],"-Regression Table.csv"), row.names=FALSE)
  # write.csv(Annual, paste0(LocalDir, SiteID[i],"-Annual-Averages.csv"),row.names=FALSE)  
  
  # ####### Identify anomalies ####
  # 
  # hist.tmean.98th <- quantile(A$TavgF[which(A$Year < 1995)], .98)
  # hist.anomalies.tmean <- A$Year[which(A$TavgF > hist.tmean.98th & A$Year < 1995)] # Anomaly years, above 98th
  # recent.percent.tmean.anomaly <- length(A$Year[which(A$TavgF > hist.tmean.98th & A$Year > 2000)])/
  #   length(A$Year[which(A$Year > 2000)]) * 100 # Percent recent years above hist 98th
  # 
  # hist.above.prcp.98th <- quantile(A$PptIn[which(A$Year < 1995)], .98)
  # hist.anomalies.above.prcp <- A$Year[which(A$PptIn > hist.above.prcp.98th & A$Year < 1995)] # Anomaly years, above 98th
  # recent.percent.above.prcp.anomaly <- length(A$Year[which(A$PptIn > hist.above.prcp.98th & A$Year > 2000)])/
  #   length(A$Year[which(A$Year > 2000)]) * 100 # Percent recent years above hist 98th
  # 
  # hist.below.prcp.98th <- quantile(A$PptIn[which(A$Year < 1995)], .02)
  # hist.anomalies.below.prcp <- A$Year[which(A$PptIn < hist.below.prcp.98th & A$Year < 1995)] # Anomaly years, above 98th
  # recent.percent.below.prcp.anomaly <- length(A$Year[which(A$PptIn < hist.below.prcp.98th & A$Year > 2000)])/
  #   length(A$Year[which(A$Year > 2000)]) * 100 # Percent recent years above hist 98th
  # 
  # anomalies.table <- data.frame(hist.tmean.98th,hist.anomalies.tmean,recent.percent.tmean.anomaly,hist.above.prcp.98th,hist.anomalies.above.prcp, recent.percent.above.prcp.anomaly,
  #                               hist.below.prcp.98th,hist.anomalies.below.prcp,recent.percent.below.prcp.anomaly)
  # write.csv(anomalies.table, paste0(LocalDir,SiteID[i], "-Anomalies-table.csv"),row.names=FALSE)
  # 
  a <- Annual |> filter(ID==SiteID[i]) |> 
    ggplot() + geom_line(aes(Year, TavgF), na.rm=TRUE) + geom_point(aes(Year, TavgF), na.rm=TRUE) +
    ylab(expression(paste("Avg Temperature", ~({}^o*F)))) + xlab("") +
    # geom_text(aes(x=1895, y= 13.5, label = "B")) +
    geom_smooth(aes(Year, TavgF),se=F, method="lm", na.rm=TRUE,linetype=if(summary(lmTmean)$coefficients[2,4]<0.05) {
      1
    } else{2}) +
    # geom_line(aes(Year, rTmean), colour = 'brown', size=1) +
    scale_x_continuous(breaks=c(1900, 1920, 1940, 1960, 1980, 2000, 2020)) +
    geom_smooth(method = lm,se=F, aes(Year, TavgP2), na.rm=TRUE,colour="brown",linetype=if(summary(lmTmeanP2)$coefficients[2,4]<0.05){
      1
    } else{2}) 
  
  b <- Annual |> filter(ID==SiteID[i]) |> 
    ggplot() + geom_line(aes(Year, PptIn), na.rm=TRUE) + geom_point(aes(Year, PptIn), na.rm=TRUE) +
    ylab("Precip (in/yr)") + xlab("") +
    # geom_text(aes(x=1895, y=350, label = "C")) +
    geom_smooth(aes(Year, PptIn),se=F, method="lm", na.rm=TRUE,linetype=if(summary(lmPpt)$coefficients[2,4]<0.05) {
      1
    } else{2}) +
    # geom_line(aes(cYr, rPpt), colour = 'brown', size=1) +
    scale_x_continuous(breaks=c(1900, 1920, 1940, 1960, 1980, 2000, 2020)) +
    geom_smooth(method = lm,se=F,aes(Year, PptP2), na.rm=TRUE,colour="brown",linetype=if(summary(lmPptP2)$coefficients[2,4]<0.05) {
      1
    } else{2}) 
  
  title = ggdraw() + draw_label(paste(SiteID[i], " - Trends for Reference and Recent Historical Periods", sep=""), 
                                fontface="bold", size=TitleSize, vjust=0.5)
  p1 = plot_grid(a, b, nrow=2, align="v")
  p2 = plot_grid(title, p1, ncol=1, rel_heights = c(0.1, 1, 0.05)) 
  # p3 = add_sub(p2, paste("Gray shaded area around regression lines = standard error of predicted y's \nReference period: ", beginRefYr, "-", endRefYr, "; Recent period: ", endRefYr+1, "-", EndYr, "; Overall period: ", BeginYr, "-", EndYr, sep=""),
  #              y=.5, hjust=0.5, vjust=0.5, size=12)
  ggdraw(p2)
  
  OFName <- paste0(LocalDir, SiteID[i], "_Historical_Trends.png")
  ggsave(OFName, width=9, height=6, dpi=dpi,bg="white")
  
  #Tmin and Tmax
  c <- Annual |> filter(ID==SiteID[i]) |> 
    ggplot() + geom_line(aes(Year, TminF), na.rm=TRUE) + geom_point(aes(Year, TminF), na.rm=TRUE) +
    ylab(expression(paste("Min Temperature", ~({}^o*F)))) + xlab("") +
    # geom_text(aes(x=1895, y= 13.5, label = "B")) +
    geom_smooth(aes(Year, TminF),se=F, method="lm", na.rm=TRUE,linetype=if(summary(lmTmean)$coefficients[2,4]<0.05) {
      1
    } else{2}) +
    scale_x_continuous(breaks=c(1900, 1920, 1940, 1960, 1980, 2000, 2020)) +
    geom_smooth(method = lm,se=F, aes(Year, TminP2), na.rm=TRUE,colour="brown",linetype=if(summary(lmTminP2)$coefficients[2,4]<0.05){
      1
    } else{2}) 
  
  d <- Annual |> filter(ID==SiteID[i]) |> 
    ggplot() + geom_line(aes(Year, TmaxF), na.rm=TRUE) + geom_point(aes(Year, TmaxF), na.rm=TRUE) +
    ylab(expression(paste("Max Temperature", ~({}^o*F)))) + xlab("") +
    # geom_text(aes(x=1895, y=350, label = "C")) +
    geom_smooth(aes(Year, TmaxF),se=F, method="lm", na.rm=TRUE,linetype=if(summary(lmTmax)$coefficients[2,4]<0.05) {
      1
    } else{2}) +
    # geom_line(aes(cYr, rPpt), colour = 'brown', size=1) +
    scale_x_continuous(breaks=c(1900, 1920, 1940, 1960, 1980, 2000, 2020)) +
    geom_smooth(method = lm,se=F,aes(Year, TmaxP2), na.rm=TRUE,colour="brown",linetype=if(summary(lmTmaxP2)$coefficients[2,4]<0.05) {
      1
    } else{2}) 
  
  title = ggdraw() + draw_label(paste(SiteID[i], " - Trends for Reference and Recent Historical Periods", sep=""), 
                                fontface="bold", size=TitleSize, vjust=0.5)
  p3 = plot_grid(c, d, nrow=2, align="v")
  p4 = plot_grid(title, p1, ncol=1, rel_heights = c(0.1, 1, 0.05)) 
  # p3 = add_sub(p2, paste("Gray shaded area around regression lines = standard error of predicted y's \nReference period: ", beginRefYr, "-", endRefYr, "; Recent period: ", endRefYr+1, "-", EndYr, "; Overall period: ", BeginYr, "-", EndYr, sep=""),
  #              y=.5, hjust=0.5, vjust=0.5, size=12)
  ggdraw(p3)
  
  OFName <- paste0(LocalDir, SiteID[i], "_Historical_Trends_Tmin_Tmax.png")
  ggsave(OFName, width=9, height=6, dpi=dpi,bg="white")
}

for (j in 1:length(SiteID)){
  park <- filter(baseData, ID==SiteID[j])
  ## Monthly Climate Normals - for climate presentations
  tmaxMon <- tapply(park$TmaxF, park$mon, mean)
  tminMon <- tapply(park$TminF, park$mon, mean)
  tmeanMon <- tapply(park$TavgF, park$mon, mean)
  pptMon  <- tapply(park$PptIn, park$mon, mean)
  pptMonSD <- tapply(park$PptIn, park$mon, sd)
  
  monAvg <- data.frame(cbind(tmaxMon, tminMon, pptMon, pptMonSD))
  monAvg$mon <- seq(1:12)
  monAvg$monNames <- c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")
  tmaxq <- tapply(park$TmaxF, park$mon, quantile)
  tminq <- tapply(park$TminF, park$mon, quantile)
  pptq <- tapply(park$PptIn, park$mon, quantile)
  
  for(i in 1:12){
    q <- tmaxq[[i]]
    monAvg$tmax25[i] <- q[2]  # 2 and 4 are 25th and 75th quantile
    monAvg$tmax75[i] <- q[4]
    
    q <- tminq[[i]]
    monAvg$tmin25[i] <- q[2]
    monAvg$tmin75[i] <- q[4]
    
    q <- pptq[[i]]
    monAvg$ppt25[i] <- q[2]
    monAvg$ppt75[i] <- q[4]
  }
  
  PlotName <- paste0(SiteID[j],"_Avg_Monthly_Tmin_Tmax_Ppt")
  OFName <- paste0(PlotName)	
  plot1 <- paste0(LocalDir, OFName)
  
  png(paste(plot1, ".png", sep = ""), width=6.5*dpi, height=4.5*dpi, res=dpi)
  
  par(mfrow=c(1,1), mgp=c(0,.5,0), mar=c(4,3.75,2,3.75))
  attach(monAvg)
  Ppt = barplot(pptMon, names.arg=monNames,
                ylim=c(0, max(monAvg$ppt75)+.5),
                axes=FALSE,
                border=NA,
                col=rgb(.678, .847, .902, alpha=0.6))
  segments(Ppt, ppt25, Ppt, ppt75, col="dark gray")
  axis(side=4)
  par(new = T)
  #if (State == 'AK'){
  #  plot(tmax75~mon, 
  #       type="l", col="red", lty=2, lwd=2,
  #       xlab=NA,
  #       ylab=NA,
  #       xaxt='n',
  #       xlim=c(.5, 12.5),
  #       ylim=c(-30,80),
  #       ps = 2,
  #       main=paste(SiteID, " - Monthly Climate Means", sep="") 
  #  )} else {
  
      plot(tmax75~mon, 
           type="l", col="red", lty=2, lwd=2,
           xlab=NA,
           ylab=NA,
           xaxt='n',
           xlim=c(.5, 12.5),
           ylim=c(0,110),
           ps = 2,
           main=paste(SiteID[j], " - Monthly Climate Means", sep=""))
      #}
  lines(tmax25~mon, col="red", lty=2, lwd=2)
  lines(tmaxMon~mon, col="red", lwd=3)
  lines(tmin75~mon, col="blue", lty=2, lwd=2) 
  lines(tmin25~mon, col="blue", lty=2, lwd=2)
  lines(tminMon~mon, col="blue", lwd=3)
  
  axis(side=2)
  mtext(side=1, line=1.75, "Month")
  mtext(side=2, line=2, expression(paste(Temperature, ~({}^o*F))))
  mtext(side=4, line=2, "Precip (in)")
  mtext(side=1, line=2.75, paste("Dashed lines/error bars = 25th-75th percentile ranges. Data range = ", BeginYr, "-", EndYr, ".", sep=""), cex=0.75, adj=0.5)
  legend("topleft", legend=c("Tmax", "Tmin"), col=c("red", "blue"), lwd=c(2,2), cex=0.75, bty="n")
  #if (State == 'AK'){
  #  legend(.4, 73, legend=c("Precip"), fill=c("light blue"), border=c(NA), cex=0.75, bty="n")
  #} else {
    legend(.4, 103, legend=c("Precip"), fill=c("light blue"), border=c(NA), cex=0.75, bty="n")
  #}
  detach(monAvg)
  dev.off()	
}
  #
  # ggplot(data=A) + geom_line(aes(Year, TavgF), na.rm=TRUE) + geom_point(aes(Year, TavgF), na.rm=TRUE) +
  #   ylab(expression(paste("Avg Temperature", ~({}^o*F)))) + xlab("") +
  #   # geom_text(aes(x=1895, y= 13.5, label = "B")) +
  #   geom_smooth(aes(Year, TavgF),se=F, method="lm", na.rm=TRUE,linetype=if(summary(lmTmean)$coefficients[2,4]<0.05) {
  #     1
  #   } else{2}) +
  #   # geom_line(aes(Year, rTmean), colour = 'brown', size=1) +
  #   scale_x_continuous(breaks=c(1900, 1920, 1940, 1960, 1980, 2000, 2020)) +
  #   geom_smooth(method = lm,se=F, aes(Year, TavgP2), na.rm=TRUE,colour="brown",linetype=if(summary(lmTmeanP2)$coefficients[2,4]<0.05){
  #     1
  #   } else{2}) +
  #   if(recent.percent.tmean.anomaly > 33) {
  #     geom_point(data = subset(A,Year %in% hist.anomalies.tmean),aes(x=Year, y=TavgF), shape=21, size=10, stroke=3, colour="brown") 
  #   }
  # 
  # title = ggdraw() + draw_label(paste(SiteID[i], " - Trends for Reference and Recent Historical Periods", sep=""), 
  #                               fontface="bold", size=TitleSize, vjust=0.5)
  # p1 = plot_grid(c, b, nrow=2, align="v")
  # p2 = plot_grid(title, p1, ncol=1, rel_heights = c(0.1, 1, 0.05)) 
  # # p3 = add_sub(p2, paste("Gray shaded area around regression lines = standard error of predicted y's \nReference period: ", beginRefYr, "-", endRefYr, "; Recent period: ", endRefYr+1, "-", EndYr, "; Overall period: ", BeginYr, "-", EndYr, sep=""),
  # #              y=.5, hjust=0.5, vjust=0.5, size=12)
  # ggdraw(p2)
  # 
  # OFName <- paste0(LocalDir, SiteID[i], "_Historical_Trends-Anomalies.png")
  # ggsave(OFName, width=9, height=6, dpi=dpi,bg="white")
}


# file.list = list.files(path = LocalDir, pattern = '.png', full.names = TRUE)
# successful<- substr(sub('.*\\/', '', file.list), 1, 4)
# not.successful <- subset(SiteID %in% successful)

# (new <- centroids$UNIT_CODE[which(!centroids$UNIT_CODE %in% successful)])
# new <-relist(sort(unlist(new)), new)


#### Update identification of anomalies from 2000
####### Identify anomalies ####

# Create one table for all parks
df <- data.frame()
for(i in 1:length(SiteID)){
  # SiteID=successful[i]
  A <- Annual |> filter(ID == SiteID[i])
  if(anyNA(A$PptIn)) next
  else{
    hist.tmean.98th <- quantile(A$TavgF[which(A$Year < 2000)], .98)
    hist.anomalies.tmean <- A$Year[which(A$TavgF > hist.tmean.98th & A$Year < 2000)] # Anomaly years, above 98th
    recent.percent.tmean.anomaly <- length(A$Year[which(A$TavgF > hist.tmean.98th & A$Year > 2000)])/
      length(A$Year[which(A$Year > 2000)]) * 100 # Percent recent years above hist 98th
    
    hist.above.prcp.98th <- quantile(A$PptIn[which(A$Year < 2000)], .98)
    hist.anomalies.above.prcp <- A$Year[which(A$PptIn > hist.above.prcp.98th & A$Year < 2000)] # Anomaly years, above 98th
    recent.percent.above.prcp.anomaly <- length(A$Year[which(A$PptIn > hist.above.prcp.98th & A$Year > 2000)])/
      length(A$Year[which(A$Year > 2000)]) * 100 # Percent recent years above hist 98th
    
    hist.below.prcp.98th <- quantile(A$PptIn[which(A$Year < 2000)], .02)
    hist.anomalies.below.prcp <- A$Year[which(A$PptIn < hist.below.prcp.98th & A$Year < 2000)] # Anomaly years, above 98th
    recent.percent.below.prcp.anomaly <- length(A$Year[which(A$PptIn < hist.below.prcp.98th & A$Year > 2000)])/
      length(A$Year[which(A$Year > 2000)]) * 100 # Percent recent years above hist 98th
    
    #Add in SIteID as column
    anomalies.table <- data.frame(hist.tmean.98th,hist.anomalies.tmean,recent.percent.tmean.anomaly,hist.above.prcp.98th,hist.anomalies.above.prcp, recent.percent.above.prcp.anomaly,
                                  hist.below.prcp.98th,hist.anomalies.below.prcp,recent.percent.below.prcp.anomaly,SiteID=SiteID[i])
    df<-rbind(df, anomalies.table)
  } 
}
write.csv(df, paste0(LocalDir, "ALL-Anomalies-table.csv"),row.names=FALSE)