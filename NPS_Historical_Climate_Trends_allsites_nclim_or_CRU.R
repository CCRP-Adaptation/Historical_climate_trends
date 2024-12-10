#Title

library(here); library(plyr); library(dplyr); # Use here::here when package lubridate is used
library(plotrix); library(zoo); library(ggplot2); library(grid); library(cowplot); library(reshape2); library(raster); 
library(ncdf4); library(reshape2); library(WriteXLS); library(data.table); library(RColorBrewer); library(ggrepel); 
library(lubridate); library(forcats); library(openxlsx); library(sf); library(raster); library(rgdal); 
library(R.utils); library(tmap); library(tmaptools); library(ggmap); library(ggspatial);
library(gridExtra); library(SPEI); library(tibble); library(sp); library(skimr); library(cft); library(stringr); 
library(ggpubr); library(lemon);library(rvest);library(tidyverse);library(XML);library(xml2);library(curl); 
library(tidync); library(viridis); library(robustbase); library(quantreg); library(zip)
library(terra)
rm(list = ls())
wgs84 <-"+init=epsg:4326"
`%!in%` <- Negate(`%in%`)


#Load the most recent NPS centroid data and prepare the park list
#AK, HI, and Pacific and Virgin Island sites need converted to WGS84 to match the CRU-TS rasters
nps_cent <- terra::vect("C:/Users/cmquinn/Documents/NPS stuff/Park_GIS_files/park-centroid/nps_boundary_centroids.gdb")
conus_cent1 <- subset(nps_cent, nps_cent$STATE %!in% c('AK', 'HI', 'AS', 'GU', 'MP', 'VI', 'PR'))
nonconus_cent0 <- subset(nps_cent, nps_cent$STATE %in% c('AK', 'HI', 'AS', 'GU', 'MP', 'VI', 'PR'))
nonconus_cent1 <- terra::project(nonconus_cent0, "EPSG:4326") 
#Add Long, Lat to file and convert to data frames
conus_cent2 <- cbind(as.data.frame(conus_cent1), terra::geom(conus_cent1))
conus_cent2$conus <- 1
nonconus_cent2 <- cbind(as.data.frame(nonconus_cent1), terra::geom(nonconus_cent1))
nonconus_cent2$conus <- 0

#if using park boundaries (polygons):
nps_boundary_gdb <- "C:/Users/cmquinn/Documents/NPS stuff/Park_GIS_files/park-poly-albers/Administrative_Boundaries_of_National Park_System_Units.gdb/"
nps_boundarywgs <- terra::vect(nps_boundary_gdb)
PI <- subset(nps_boundarywgs, nps_boundarywgs$STATE %in% c('AS', 'MP', 'GU'))

#alternative approach using st instead of terra
#nps_centroids <- st_read(dsn='C:/Users/cmquinn/Documents/NPS stuff/Park_GIS_files/park-centroid/nps_boundary_centroids.gdb')
#conus_cent1 <- subset(nps_centroids, nps_cent$STATE %!in% c('AK', 'HI', 'AS', 'GU', 'MP', 'VI', 'PR'))
#nonconus_cent0 <- subset(nps_centroids, nps_cent$STATE %in% c('AK', 'HI', 'AS', 'GU', 'MP', 'VI', 'PR'))
#nonconus_cent1 <- st_transform(nonconus_cent0, st_crs("EPSG:4326"))
#nonconus_cent2st <- cbind(as.data.frame(nonconus_cent1), sf::st_coordinates(nonconus_cent1))
#conus_cent2 <- cbind(as.data.frame(conus_cent1), sf::st_coordinates(conus_cent1))

#Per AR, park and preserve units are separate even if jointly managed; exclude the preserve area. 
#if running historical trends for a combination of CONUS and non-CONUS Parks:
parklist <- rbind(conus_cent2, nonconus_cent2) %>% filter(!(UNIT_CODE %in% c("ANIA", "DENA", "GAAR", "GLBA", "GRSA", "KATM", "LACL", "WRST") 
                                                            & grepl("Preserve", UNIT_TYPE))) %>%
  dplyr::select(UNIT_CODE, UNIT_TYPE, STATE, REGION, conus, x, y)
#
write.csv(parklist, file="nps_cent_parklist_2024.csv")

#CONUS: if running historical trends for one or more CONUS sites only: 
parklist <- filter(conus_cent2, !(UNIT_CODE=="GRSA" & grepl("Preserve", UNIT_TYPE))) %>%
  dplyr::select(UNIT_CODE, UNIT_TYPE, STATE, x, y)

#confirm that the park list has only unique unit codes
check_uniq <- parklist %>% dplyr::select(UNIT_CODE) %>%
  group_by(UNIT_CODE) %>%
  summarise(count = n())
check_uniq[check_uniq$count==2, ]  




# -------------- Input site and analysis information -------------------------------------- #
#BeginYr = 1895
#EndYr = 2022
county_climate <- "N"
point_climate <- "Y"
poly_climate <- "N"

doP1 <- "YES"  # Should a separate regression be calculated for the reference period (default 1900-1970)? 
doP2 <- "YES"  # Should a separate regression be calculate for the period after the reference period (default 1971-present)? 
#beginRefYr = 1900
endRefYr = 1970
   # needed for rolling mean plot below.  
#dataEndYr = EndYr - 1 
dte = Sys.Date()
stepYrs	= 10		  # for period plots 
rollLen = 10      # period of calc for rolling average; default 10 = decadal
dpi = 600 
taus <- c(.01,.05,.25,.75,.95,.99)  # quantiles to plot (median is specified separately)

nClimDataDir <- "C:/Users/cmquinn/Documents/NOAA-data/nclimgrid_240105/"
CRUDataDir <- "C:/Users/cmquinn/Documents/CRU_TS/"
LocalDir <- "C:/Users/cmquinn/Documents/repos/Historical_climate_trends/"

# nClimGrid NetCDF-4 files
nclim.file.list = list.files(path = nClimDataDir, pattern = '.nc', full.names = TRUE)

# CRU-TS NetCDF-4 files
#cru.file.list = list.files(path = CRUDataDir, pattern = '.nc', full.names = TRUE)
cru_pre <- terra::rast(paste0(CRUDataDir,"cru_ts4.07.1901.2022.pre.dat.nc"))
cru_tmn <- terra::rast(paste0(CRUDataDir,"cru_ts4.07.1901.2022.tmn.dat.nc"))
cru_tmp <- terra::rast(paste0(CRUDataDir,"cru_ts4.07.1901.2022.tmp.dat.nc"))
cru_tmx <- terra::rast(paste0(CRUDataDir,"cru_ts4.07.1901.2022.tmx.dat.nc"))
cru_all <- terra::rast(list(cru_pre, cru_tmn, cru_tmp, cru_tmx))
crudaterange <- seq(as.Date("1901-01-16"), to=as.Date("2022-12-16"),by = "month")
cruvars <- c("pre", "tmn", "tmp", "tmx")

GetSeason <- function(DateVec){
  seas <- as.character(rep(NA, length(DateVec)))
  seas[which(format(DateVec,'%B') %in% c("December", "January", "February"))]<- "Winter"
  seas[which(format(DateVec,'%B') %in% c("March", "April", "May"))]<- "Spring"
  seas[which(format(DateVec,'%B') %in% c("June", "July", "August"))]<- "Summer"
  seas[which(format(DateVec,'%B') %in% c("September", "October", "November"))]<- "Fall"
  return(seas)
}

##ggplot theme for all plots
#Theme for all plots
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
#################   End of Initials  ##########################  

#parklist <- parklist %>% filter(STATE %in% c('AK', 'HI', 'PR' , 'VI'))
.0for(i in 1:(length(parklist$UNIT_CODE))){
  
SiteID = parklist$UNIT_CODE[i]
State = parklist$STATE[which(parklist$UNIT_CODE==SiteID)]
Lon = parklist$x[which(parklist$UNIT_CODE==SiteID)]
Lat = parklist$y[which(parklist$UNIT_CODE==SiteID)]

if(county_climate == "Y") {
  OutDir = paste0(LocalDir,SiteID,"-Historical-County","/") # for .csv's
if(dir.exists(OutDir) == FALSE){
  dir.create(OutDir)
}
  source(here::here("scripts", "webscrape-county-nclim.R"), local = knitr::knit_global())
} else {
  
  OutDir = paste0(LocalDir,SiteID,"-Historical","/") # for .csv's
if(dir.exists(OutDir) == FALSE){
  dir.create(OutDir)
}}

#nClimGrid for CONUS and CRU for all other sites
# Using Tidync (CONUS) or terra::extract (non-CONUS) and pulling out the grid cell with the park centroid point. 
if (State %in% c('AK', 'HI', 'AS', 'GU', 'MP', 'VI', 'PR')) {
  BeginYr = 1901
  beginRefYr = 1901
  EndYr = 2022
  point = subset(parklist, parklist$UNIT_CODE==SiteID) %>% dplyr::select(x, y)
  cruextract <- terra::extract(cru_all, point, method='simple', xy=TRUE, ID=FALSE,  
                                 exact=FALSE, raw=TRUE)
  for (k in 1:length(cruvars)){
      cruvar = cruvars[k]
      cruwide0 <- as.data.frame(cruextract) 
      cruwide <- cruwide0[grepl(paste(cruvar), colnames(cruwide0))]
      colnames(cruwide) <- c(crudaterange)
      long <- reshape2::melt(cruwide, value.name=paste(cruvar)) %>%
               #mutate(Date1 = as.Date(as.numeric(paste(variable))), format="%Y-%m-%d", origin="1901-01-16") %>%
               #mutate(Date = as.Date(c(crudaterange), format="%Y-%m-%d", origin="1901-01-16")) %>%
               dplyr::select(all_of(c('variable', paste(cruvar)))) %>%
               arrange(variable, cruvar)
    if (k == 1){crulong<-long} else {crulong<-merge(crulong,long,by="variable")}
  }
  crulong$Date <- c(crudaterange) %>% as.Date(crulong$Date, format="%Y-%m-%d", origin="1900-01-01")
  baseData <- crulong[,2:6] %>% mutate(PptIn = pre/25.4,
                              TmaxF = tmx * 9/5 + 32,
                              TminF = tmn * 9/5 + 32, 
                              TavgF = (TmaxF+TminF)/2,
                              YearMon = paste0(year(Date),sprintf("%02d",month(Date))),
                              Season = GetSeason(Date)) %>% 
                filter(Date <= paste0(EndYr,"-12-16")) %>%
                relocate(Date)
                
  #save
  write.csv(baseData, (sprintf("%s%s_CRU_TS.csv", OutDir, SiteID)),row.names=FALSE)
  # Create output directories
  #baseData <- if(county_climate == "Y") {
  #  read.csv(paste0(OutDir, SiteID,"_CRU_TS_county.csv"),header=TRUE)
  #} else {
  #  read.csv(paste0(OutDir, SiteID,"_CRU_TS.csv"),header=TRUE) #replace with nclim_nc_extraction script
  #}
} else {
  BeginYr = 1895 
  EndYr = 2023
  beginRefYr = 1900
  for (j in 1:length(nclim.file.list)){
   src <- tidync(nclim.file.list[j]) %>% 
     hyper_filter(lat = (lat <= c(Lat+0.06) & lat >= c(Lat-0.05))) %>% #subset to Lat/Lon
     hyper_filter(lon = (lon <= c(Lon +0.05) & lon >= c(Lon -0.05))) %>% #aggregate lat and lon
     hyper_tibble() %>% 
     mutate(Date = as.Date(time,origin = "1800-01-01")) %>% 
     group_by(Date) %>% 
     summarise_at(1,mean) 
   if (j == 1){df<-src} else {df<-merge(df,src,by="Date")}
 }
  baseData <- df %>% mutate(PptIn = prcp/25.4,
                            TmaxF = tmax * 9/5 + 32,
                            TminF = tmin * 9/5 + 32, 
                            TavgF = (TmaxF+TminF)/2,
                            YearMon = paste0(year(Date),sprintf("%02d",month(Date))),
                            Season = GetSeason(Date)) %>% 
  filter(Date <= paste0(EndYr,"-12-01"))
  write.csv(baseData, (sprintf("%s%s_nClimGrid.csv", OutDir, SiteID)),row.names=FALSE)
}
#baseData <- if(county_climate == "Y") {
#  read.csv(paste0(OutDir, SiteID,"_nClimGrid_county.csv"),header=TRUE)
#} else {
#  read.csv(paste0(OutDir, SiteID,"_nClimGrid.csv"),header=TRUE) #replace with nclim_nc_extraction script
#}


##  RSS Historical-trends plots vxxx.R
#################################################

baseData$yr<-year(baseData$Date)
baseData$mon<-month(baseData$Date)

refData <- baseData[baseData$yr >= beginRefYr & baseData$yr <= endRefYr,]

	# maybe should use Historical-trends year avgs instead i.e. month 14
pptAvg = with(baseData, tapply(PptIn, yr, mean))  * 12  # xx/mo ->: xx/yr
tminAvg = with(baseData, tapply(TminF, yr, mean))
tmaxAvg = with(baseData, tapply(TmaxF, yr, mean))
tmeanAvg = with(baseData, tapply(TavgF, yr, mean))

cYr <- BeginYr:EndYr
yrAvgs <- data.frame(cYr, pptAvg, tminAvg, tmaxAvg, tmeanAvg)
yrAvgs$tAvg <- (yrAvgs$tminAvg+yrAvgs$tmaxAvg)/2


  ## interesting to compare Historical-trends vs calcuated Tmean
pptRef <- data.frame(yrAvgs[yrAvgs$cYr >= beginRefYr & yrAvgs$cYr <= endRefYr, 2])
names(pptRef) <- "ppt"


######################  Periods of Analysis  ######################

p1_start  = beginRefYr
p1_end    = endRefYr
p2_start  = endRefYr
p2_end    = EndYr
	
yrAvgs$tmaxP1 <- yrAvgs$tmaxAvg
yrAvgs$tmaxP1[which(yrAvgs$cYr < p1_start | yrAvgs$cYr > p1_end)] = NA

yrAvgs$tminP1 <- yrAvgs$tminAvg
yrAvgs$tminP1[which(yrAvgs$cYr < p1_start | yrAvgs$cYr > p1_end)] = NA

yrAvgs$tmeanP1 <- yrAvgs$tmeanAvg
yrAvgs$tmeanP1[which(yrAvgs$cYr < p1_start | yrAvgs$cYr > p1_end)] = NA
                              
yrAvgs$pptP1 <- yrAvgs$pptAvg
yrAvgs$pptP1[which(yrAvgs$cYr < p1_start | yrAvgs$cYr > p1_end)] = NA

yrAvgs$tmaxP2 <- yrAvgs$tmaxAvg
yrAvgs$tmaxP2[which(yrAvgs$cYr < p2_start | yrAvgs$cYr > p2_end)] = NA

yrAvgs$tminP2 <- yrAvgs$tminAvg
yrAvgs$tminP2[which(yrAvgs$cYr < p2_start | yrAvgs$cYr > p2_end)] = NA

yrAvgs$tmeanP2 <- yrAvgs$tmeanAvg
yrAvgs$tmeanP2[which(yrAvgs$cYr < p2_start | yrAvgs$cYr > p2_end)] = NA

yrAvgs$pptP2 <- yrAvgs$pptAvg
yrAvgs$pptP2[which(yrAvgs$cYr < p2_start | yrAvgs$cYr > p2_end)] = NA

	########################################
#Data check - plot min and max by month----
tmaxMon <- tapply(baseData$TmaxF, baseData$mon, mean)
tminMon <- tapply(baseData$TminF, baseData$mon, mean)
tmeanMon <- tapply(baseData$TavgF, baseData$mon, mean)
pptMon  <- tapply(baseData$PptIn, baseData$mon, mean)
pptMonSD <- tapply(baseData$PptIn, baseData$mon, sd)

monAvg <- data.frame(cbind(tmaxMon, tminMon, pptMon, pptMonSD))
monAvg$mon <- seq(1:12)
monAvg$monNames <- c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")
tmaxq <- tapply(baseData$TmaxF, baseData$mon, quantile)
tminq <- tapply(baseData$TminF, baseData$mon, quantile)
pptq <- tapply(baseData$PptIn, baseData$mon, quantile)

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

PlotName <- "Avg_Monthly_Tmin_Tmax_Ppt"
OFName <- paste0(PlotName)	
plot1 <- paste0(OutDir, OFName)

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
if (State == 'AK'){
  plot(tmax75~mon, 
     type="l", col="red", lty=2, lwd=2,
     xlab=NA,
     ylab=NA,
     xaxt='n',
     xlim=c(.5, 12.5),
     ylim=c(-30,80),
     ps = 2,
     main=paste(SiteID, " - Monthly Climate Means", sep="") 
)} else {
  plot(tmax75~mon, 
     type="l", col="red", lty=2, lwd=2,
     xlab=NA,
     ylab=NA,
     xaxt='n',
     xlim=c(.5, 12.5),
     ylim=c(0,110),
     ps = 2,
     main=paste(SiteID, " - Monthly Climate Means", sep="") 
)}
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
if (State == 'AK'){
legend(.4, 73, legend=c("Precip"), fill=c("light blue"), border=c(NA), cex=0.75, bty="n")
} else {
legend(.4, 103, legend=c("Precip"), fill=c("light blue"), border=c(NA), cex=0.75, bty="n")
}
detach(monAvg)
dev.off()	



#-------------------------------------------------#
#Running average plots----
#-------------------------------------------------#

rTmin <- rollmean(tminAvg, rollLen)
rTmax <- rollmean(tmaxAvg, rollLen)
rTmean <- rollmean(tmeanAvg, rollLen)
rPpt  <- rollmean(pptAvg, rollLen)

rYr = seq((EndYr-1) - length(rTmin)+1, EndYr - 1)

rDat <- data.frame(cbind(rYr, rTmin, rTmax, rTmean, rPpt))
names(rDat)[1] <- "cYr"
rDat$yr <- rDat$cYr
yrAvgs <- merge(rDat, yrAvgs, all=TRUE)

##ggplot
PlotName <- "10yr_Running_Means"

#Colors for running means
RMColors = scale_color_manual(name="", values=c("brown", "black", "#3366FF"))

a <- ggplot(aes(x=cYr), data=yrAvgs) + 
  geom_line(aes(y=tmaxAvg, group=1, col="Annual means"), na.rm=TRUE) + 
  geom_point(aes(y=tmaxAvg, col="Annual means"), na.rm=TRUE) +
  ylab(expression(paste(Tmax, ~({}^o*F)))) + xlab("") +
  # geom_text(aes(x=1895, y=29, label="A")) +
  geom_smooth(method="lm", aes(y=tmaxAvg, group=2, col="Regression trend"), na.rm=TRUE)+ 
  geom_line(aes(y=rTmax, group=3, col=paste(rollLen, "-yr running mean", sep="")), size=1.5, na.rm=TRUE) +
  RMColors +
  scale_x_continuous(breaks=c(1900, 1920, 1940, 1960, 1980, 2000, 2020))

b <- ggplot(aes(cYr, tminAvg), data=yrAvgs) + geom_line(na.rm=TRUE) + geom_point(na.rm=TRUE) +
  ylab(expression(paste(Tmin, ~({}^o*F)))) + xlab("") +
  # geom_text(aes(x=1895, y= 13.5, label = "B")) +
  geom_smooth(method="lm", na.rm=TRUE)+
  geom_line(aes(cYr, rTmin), size=1.5, colour="brown", na.rm=TRUE) + 
  scale_x_continuous(breaks=c(1900, 1920, 1940, 1960, 1980, 2000, 2020))

c <- ggplot(aes(cYr, tmeanAvg), data=yrAvgs) + geom_line(na.rm=TRUE) + geom_point(na.rm=TRUE) +
  ylab(expression(paste(Tmean, ~({}^o*F)))) + xlab("") +
  # geom_text(aes(x=1895, y= 13.5, label = "B")) +
  geom_smooth(method="lm", na.rm=TRUE)+
  geom_line(aes(cYr, rTmean), size=1.5, colour="brown", na.rm=TRUE) +
  scale_x_continuous(breaks=c(1900, 1920, 1940, 1960, 1980, 2000, 2020))

d <- ggplot(aes(cYr, pptAvg), data=yrAvgs) + geom_line(na.rm=TRUE) + geom_point(na.rm=TRUE) +
  ylab("Precip (in/yr)") + xlab("") +
  # geom_text(aes(x=1895, y=350, label = "C")) +
  geom_smooth(method="lm", na.rm=TRUE)+
  geom_line(aes(cYr, rPpt), size=1.5, colour="brown", na.rm=TRUE) + 
  scale_x_continuous(breaks=c(1900, 1920, 1940, 1960, 1980, 2000, 2020))

p1 = plot_grid(a, b, c, d, nrow=4, align="v")
title = ggdraw() + draw_label(paste(SiteID, " - Annual Means and Trends", sep=""), 
                              fontface="bold", size=TitleSize, vjust=0.5)
legend = get_legend(a + theme(legend.position = "bottom"))
p2 = plot_grid(title, p1, legend, ncol=1, rel_heights=c(0.05, 1, .05))
p3 = add_sub(p2, paste("Gray shaded area around regression lines = standard error of predicted y's \nData range = ", BeginYr, "-", EndYr, sep=""), 
             y=0.5, hjust=0.5, vjust=0.5, size=12)
ggdraw(p3)

OFName = paste0(OutDir, PlotName,".png")
ggsave(OFName, width=6.5, height=8.5, dpi=dpi,bg="white")

##########################
#Regressions for trends----
iter1k = lmrob.control(k.max = 1000)
lmTmax <- lmrob(yrAvgs$tmaxAvg~cYr, control=iter1k)
lmTmaxP1 <- lmrob(yrAvgs$tmaxP1~cYr, control=iter1k)
lmTmaxP2 <- lmrob(yrAvgs$tmaxP2~cYr, control=iter1k)

lmTmin <- lmrob(yrAvgs$tminAvg~cYr, control=iter1k)
lmTminP1 <- lmrob(yrAvgs$tminP1~cYr, control=iter1k)
lmTminP2 <- lmrob(yrAvgs$tminP2~cYr, control=iter1k)

lmTmean <- lmrob(yrAvgs$tmeanAvg~cYr, control=iter1k)
lmTmeanP1 <- lmrob(yrAvgs$tmeanP1~cYr, control=iter1k)
lmTmeanP2 <- lmrob(yrAvgs$tmeanP2~cYr, control=iter1k)

lmPpt  <- lmrob(yrAvgs$pptAvg~cYr, control=iter1k)		
lmPptP1 <- lmrob(yrAvgs$pptP1~cYr, control=iter1k)		
lmPptP2 <- lmrob(yrAvgs$pptP2~cYr, control=iter1k)		

# make table of coefficients
probStar <- function(pVal){
  probStar <- "NS"
  if(pVal < 0.05)probStar <- "*"
  if(pVal < 0.01)probStar <- "**"
  if(pVal < 0.001)probStar <- "***"
  probStar
}

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

regsTmax <-  rbind(lmMetrics(lmTmax), lmMetrics(lmTmaxP1), lmMetrics(lmTmaxP2))
regsTmin <-  rbind(lmMetrics(lmTmin), lmMetrics(lmTminP1), lmMetrics(lmTminP2))
regsTmean <- rbind(lmMetrics(lmTmean),lmMetrics(lmTmeanP1),lmMetrics(lmTmeanP2))
regsPpt <-   rbind(lmMetrics(lmPpt),  lmMetrics(lmPptP1),  lmMetrics(lmPptP2))

perAll <- paste(min(yrAvgs$cYr), max(yrAvgs$cYr), sep="-")
per1 <- paste(p1_start, p1_end, sep="-")
per2 <- paste(p2_start, p2_end, sep="-")
Period <- rep(c(perAll, per1, per2), 4)

lmTable <- cbind( Var=rep(c("Tmax", "Tmin", "Tmean", "Precip"),each=3), Period, rbind(regsTmax, regsTmin, regsTmean, regsPpt))

lmTable$YrCoeff <- lmTable$YrCoeff * 100   # convert to degF(in)/100-yrs
lmTable$seSlope <- lmTable$seSlope * 100
#add units to YrCoeff field
colnames(lmTable) <- c("Var", "Period", "YrCoeff(degF(in)/100yrs)", "seSlope", "probCoeff", "probSign", "r2")

print(lmTable, row.names = F)

write.csv(lmTable, paste0(OutDir, "Regression Table.csv"), row.names=FALSE)
write.csv(yrAvgs, paste0(OutDir, "Annual-Averages.csv"),row.names=FALSE)


#-----------------------------------------------------------#
#ANNUAL AVERAGE LINES WITH REGRESSION----
#-----------------------------------------------------------#
# Amend to incl. running means and dashed lines for significance
#    ggplot graphics    #
# annaul points, linear regression, and 95% CI
# need to manually set position of a, b, c labels

# gray zone is 95% confidence interval

PlotName = "Annual_Means_Lines_Regressions"

a <- ggplot(yrAvgs) +	geom_smooth(method = lm, aes(cYr, tmaxAvg), na.rm=TRUE,linetype=if(summary(lmTmax)$coefficients[2,4]<0.05) {
  1
} else{2}) +
  geom_line(aes(cYr, tmaxAvg), na.rm=TRUE) + geom_point(aes(cYr, tmaxAvg), na.rm=TRUE) +
  ylab(expression(paste(Tmax, ~({}^o*F)))) + xlab("") +
  scale_x_continuous(breaks=c(1900, 1920, 1940, 1960, 1980, 2000, 2020)) +
  geom_line(aes(cYr, rTmax), colour = 'brown', size=1)  # rolling mean
if(doP1 == "YES")a <- a + geom_smooth(method = lm, aes(cYr, tmaxP1), na.rm=TRUE,linetype=if(summary(lmTmaxP1)$coefficients[2,4]<0.05) {
  1
} else{2})
if(doP2 == "YES")a <- a + geom_smooth(method = lm, aes(cYr, tmaxP2), na.rm=TRUE,linetype=if(summary(lmTmaxP2)$coefficients[2,4]<0.05) {
  1
} else{2})
a

b <- ggplot(data=yrAvgs) + geom_line(aes(cYr, tminAvg), na.rm=TRUE) + geom_point(aes(cYr, tminAvg), na.rm=TRUE) +
  ylab(expression(paste(Tmin, ~({}^o*F)))) + xlab("") +
  # geom_text(aes(x=1895, y= 13.5, label = "B")) +
  geom_smooth(aes(cYr, tminAvg), method="lm", na.rm=TRUE,linetype=if(summary(lmTmin)$coefficients[2,4]<0.05) {
    1
  } else{2}) +
  scale_x_continuous(breaks=c(1900, 1920, 1940, 1960, 1980, 2000, 2020))+
  geom_line(aes(cYr, rTmin), colour = 'brown', size=1)

if(doP1 == "YES")b <- b +	geom_smooth(method = lm, aes(cYr, tminP1), na.rm=TRUE,linetype=if(summary(lmTminP1)$coefficients[2,4]<0.05) {
  1
} else{2})
if(doP2 == "YES")b <- b +	geom_smooth(method = lm, aes(cYr, tminP2), na.rm=TRUE,linetype=if(summary(lmTminP2)$coefficients[2,4]<0.05) {
  1
} else{2})

c <- ggplot(data=yrAvgs) + geom_line(aes(cYr, tmeanAvg), na.rm=TRUE) + geom_point(aes(cYr, tmeanAvg), na.rm=TRUE) +
  ylab(expression(paste(Tmean, ~({}^o*F)))) + xlab("") +
  # geom_text(aes(x=1895, y= 13.5, label = "B")) +
  geom_smooth(aes(cYr, tmeanAvg), method="lm", na.rm=TRUE,linetype=if(summary(lmTmean)$coefficients[2,4]<0.05) {
    1
  } else{2}) +
  geom_line(aes(cYr, rTmean), colour = 'brown', size=1) +
  scale_x_continuous(breaks=c(1900, 1920, 1940, 1960, 1980, 2000, 2020))

if(doP1 == "YES")c <- c + geom_smooth(method = lm, aes(cYr, tmeanP1), na.rm=TRUE,linetype=if(summary(lmTmeanP1)$coefficients[2,4]<0.05) {
  1
} else{2})
if(doP2 == "YES") c <- c + geom_smooth(method = lm, aes(cYr, tmeanP2), na.rm=TRUE,linetype=if(summary(lmTmeanP2)$coefficients[2,4]<0.05) {
  1
} else{2}) 

d <- ggplot(data=yrAvgs) + geom_line(aes(cYr, pptAvg), na.rm=TRUE) + geom_point(aes(cYr, pptAvg), na.rm=TRUE) +
  ylab("Precip (in/yr)") + xlab("") +
  # geom_text(aes(x=1895, y=350, label = "C")) +
  geom_smooth(aes(cYr, pptAvg), method="lm", na.rm=TRUE,linetype=if(summary(lmPpt)$coefficients[2,4]<0.05) {
    1
  } else{2}) +
  geom_line(aes(cYr, rPpt), colour = 'brown', size=1) +
  scale_x_continuous(breaks=c(1900, 1920, 1940, 1960, 1980, 2000, 2020))

if(doP1 == "YES")d <- d + geom_smooth(method = lm, aes(cYr, pptP1), na.rm=TRUE,linetype=if(summary(lmPptP1)$coefficients[2,4]<0.05) {
  1
} else{2})
if(doP2 == "YES")d <- d + geom_smooth(method = lm, aes(cYr, pptP2), na.rm=TRUE,linetype=if(summary(lmPptP2)$coefficients[2,4]<0.05) {
  1
} else{2}) 


#4-panel plot		
p1 = plot_grid(a, b, c, d, nrow=4, align="v")
title = ggdraw() + draw_label(paste(SiteID, " - Trends for Reference and Recent \nHistorical Periods", sep=""), 
                              fontface="bold", size=TitleSize, vjust=0.5)
p2 = plot_grid(title, p1, ncol=1, rel_heights = c(0.07, 1)) 
p3 = add_sub(p2, paste("Gray shaded area around regression lines = standard error of predicted y's \nReference period: ", beginRefYr, "-", endRefYr, "; Recent period: ", endRefYr+1, "-", EndYr, "; Overall period: ", BeginYr, "-", EndYr, sep=""),
             y=.5, hjust=0.5, vjust=0.5, size=12)
ggdraw(p3)

OFName <- paste0(OutDir, PlotName, " 4-panel.png")
ggsave(OFName, width=6.5, height=8.5, dpi=dpi,bg="white")

#2-panel Tmax/Tmin plot
p1 = plot_grid(a, b, nrow=2, align="v")
p2 = plot_grid(title, p1, ncol=1, rel_heights = c(0.1, 1, 0.05)) 
p3 = add_sub(p2, paste("Gray shaded area around regression lines = standard error of predicted y's \nReference period: ", beginRefYr, "-", endRefYr, "; Recent period: ", endRefYr+1, "-", EndYr, "; Overall period: ", BeginYr, "-", EndYr, sep=""),
             y=.5, hjust=0.5, vjust=0.5, size=12)
ggdraw(p3)

OFName <- paste0(OutDir, PlotName, "_Tmin_Tmax.png")
ggsave(OFName, width=6.5, height=6.5, dpi=dpi,bg="white")

#2-panel Tmean/Precip plot
p1 = plot_grid(c, d, nrow=2, align="v")
p2 = plot_grid(title, p1, ncol=1, rel_heights = c(0.1, 1, 0.05)) 
p3 = add_sub(p2, paste("Gray shaded area around regression lines = standard error of predicted y's \nReference period: ", beginRefYr, "-", endRefYr, "; Recent period: ", endRefYr+1, "-", EndYr, "; Overall period: ", BeginYr, "-", EndYr, sep=""),
             y=.5, hjust=0.5, vjust=0.5, size=12)
ggdraw(p3)

OFName <- paste0(OutDir, PlotName, "_Tmean_Precip.png")
ggsave(OFName, width=6.5, height=6.5, dpi=dpi,bg="white")


####### Identify anomalies ####

hist.tmean.98th <- quantile(yrAvgs$tmeanAvg[which(yrAvgs$cYr < 1995)], .98)
hist.anomalies.tmean <- yrAvgs$cYr[which(yrAvgs$tmeanAvg > hist.tmean.98th & yrAvgs$cYr < 1995)] # Anomaly years, above 98th
recent.percent.tmean.anomaly <- length(yrAvgs$cYr[which(yrAvgs$tmeanAvg > hist.tmean.98th & yrAvgs$cYr > 2000)])/
  length(yrAvgs$cYr[which(yrAvgs$cYr > 2000)]) * 100 # Percent recent years above hist 98th

hist.above.prcp.98th <- quantile(yrAvgs$pptAvg[which(yrAvgs$cYr < 1995)], .98)
hist.anomalies.above.prcp <- yrAvgs$cYr[which(yrAvgs$pptAvg > hist.above.prcp.98th & yrAvgs$cYr < 1995)] # Anomaly years, above 98th
recent.percent.above.prcp.anomaly <- length(yrAvgs$cYr[which(yrAvgs$pptAvg > hist.above.prcp.98th & yrAvgs$cYr > 2000)])/
  length(yrAvgs$cYr[which(yrAvgs$cYr > 2000)]) * 100 # Percent recent years above hist 98th

hist.below.prcp.98th <- quantile(yrAvgs$pptAvg[which(yrAvgs$cYr < 1995)], .02)
hist.anomalies.below.prcp <- yrAvgs$cYr[which(yrAvgs$pptAvg < hist.below.prcp.98th & yrAvgs$cYr < 1995)] # Anomaly years, above 98th
recent.percent.below.prcp.anomaly <- length(yrAvgs$cYr[which(yrAvgs$pptAvg < hist.below.prcp.98th & yrAvgs$cYr > 2000)])/
  length(yrAvgs$cYr[which(yrAvgs$cYr > 2000)]) * 100 # Percent recent years above hist 98th

anomalies.table <- data.frame(hist.tmean.98th,hist.anomalies.tmean,recent.percent.tmean.anomaly,hist.above.prcp.98th,hist.anomalies.above.prcp, recent.percent.above.prcp.anomaly,
           hist.below.prcp.98th,hist.anomalies.below.prcp,recent.percent.below.prcp.anomaly)
write.csv(anomalies.table, paste0(OutDir, "Anomalies-table.csv"),row.names=FALSE)

# Plot tmean and precip w/ anomalies
c <- ggplot(data=yrAvgs) + geom_line(aes(cYr, tmeanAvg), na.rm=TRUE) + geom_point(aes(cYr, tmeanAvg), na.rm=TRUE) +
  ylab(expression(paste(Tmean, ~({}^o*F)))) + xlab("") +
  # geom_text(aes(x=1895, y= 13.5, label = "B")) +
  geom_smooth(aes(cYr, tmeanAvg), method="lm", na.rm=TRUE,linetype=if(summary(lmTmean)$coefficients[2,4]<0.05) {
    1
  } else{2}) +
  scale_x_continuous(breaks=c(1900, 1920, 1940, 1960, 1980, 2000)) +
  geom_smooth(method = lm, aes(cYr, tmeanP1), na.rm=TRUE,linetype=if(summary(lmTmeanP1)$coefficients[2,4]<0.05) {
  1
} else{2}) +
  geom_smooth(method = lm, aes(cYr, tmeanP2), na.rm=TRUE,linetype=if(summary(lmTmeanP2)$coefficients[2,4]<0.05) {
  1
} else{2}) +
  if(recent.percent.tmean.anomaly > 33) {
    geom_point(data = subset(yrAvgs, cYr %in% hist.anomalies.tmean),aes(x=cYr, y=tmeanAvg), shape=21, size=10, stroke=3, colour="brown") 
  }
  # if(recent.percent.tmean.anomaly > 33)

d <- ggplot(data=yrAvgs) + geom_line(aes(cYr, pptAvg), na.rm=TRUE) + geom_point(aes(cYr, pptAvg), na.rm=TRUE) +
  ylab("Precip (in/yr)") + xlab("") +
  # geom_text(aes(x=1895, y=350, label = "C")) +
  geom_smooth(aes(cYr, pptAvg), method="lm", na.rm=TRUE,linetype=if(summary(lmPpt)$coefficients[2,4]<0.05) {
    1
  } else{2}) +
  scale_x_continuous(breaks=c(1900, 1920, 1940, 1960, 1980, 2000, 2020)) +
  geom_smooth(method = lm, aes(cYr, pptP1), na.rm=TRUE,linetype=if(summary(lmPptP1)$coefficients[2,4]<0.05) {
  1
} else{2}) +
  geom_smooth(method = lm, aes(cYr, pptP2), na.rm=TRUE,linetype=if(summary(lmPptP2)$coefficients[2,4]<0.05) {
  1
} else{2}) 

g <- grid.arrange(c,d,nrow=2)
figure <- ggarrange(c + rremove("xlab") + rremove("x.text"), d, # remove axis labels from plots
                    labels = NULL,
                    nrow = 2)

annotate_figure(figure, top = textGrob(paste0("Historical trends for ",SiteID), vjust = 1, gp = gpar(cex = 2)))

ggsave(paste0(OutDir, "Tmean_Prcp_AnnualMeans_and_Anomlies.png"), height=6.5, width=6.5, dpi=dpi,bg="white")


#Decadal PLOTS by "step" and season----


nSteps = floor((EndYr - BeginYr)/stepYrs)	# year steps 
sStepAvgs = matrix(-99, nrow = nSteps * 4, ncol = 5)
seasStepAvgs = data.frame(sStepAvgs);  rm(sStepAvgs)
seasName <- c("Winter", "Spring", "Summer", "Fall")
seasStepAvgs$seas <- rep(seasName, nSteps)

names(seasStepAvgs) <- c("startYr", "tmin", "tmax", "tmean", "ppt", "seas")
	#columns are: 1=startYr 2=tmin 3=tmax 4=mean(tempStepData$TminF[tempStepData$season == seasName[seas]])ppt 5=seas 
i=0

for(i in 0:(nSteps-1)){
	curBeginYr = BeginYr + i*stepYrs
	tempStepData <- baseData[baseData$yr >= curBeginYr & baseData$yr < (BeginYr + (i+1)*stepYrs), ]
	for(seas in 1:4) {
		stepYr = i * 4 + seas
		seasStepAvgs$startYr[stepYr] = curBeginYr
		seasStepAvgs$tmin[stepYr] = 
		seasStepAvgs$tmax[stepYr] = mean(tempStepData$TmaxF[tempStepData$Season == seasName[seas]])
		seasStepAvgs$tmean[stepYr] = mean(tempStepData$TavgF[tempStepData$Season == seasName[seas]])
		seasStepAvgs$ppt[stepYr] = mean(tempStepData$PptIn[tempStepData$Season == seasName[seas]])
	}		# next seas
}			# next nSteps

rm(tempStepData)
seasStepAvgs$ppt <- seasStepAvgs$ppt * 12  # xx/mo to xx/yr

#-------------------------------------------------#
#ANOMALY LINE PLOTS - By Decade and Season----
#-------------------------------------------------#

#  1 per season, points at "step" yr intervals (decade if step == 10)
#  Labels (A, B, C) need to be placed manually
#  Adjust vertical scale using ylim(y min, y max)

########################################

pdat <- seasStepAvgs   # period data

# these are departures from entire period of record
seas
pdat$tmin[which(pdat$seas=="Winter")] <- pdat$tmin[which(pdat$seas=="Winter")] - mean(pdat$tmin[which(pdat$seas=="Winter")])
pdat$tmin[which(pdat$seas=="Spring")] <- pdat$tmin[which(pdat$seas=="Spring")] - mean(pdat$tmin[which(pdat$seas=="Spring")])
pdat$tmin[which(pdat$seas=="Summer")] <- pdat$tmin[which(pdat$seas=="Summer")] - mean(pdat$tmin[which(pdat$seas=="Summer")])
pdat$tmin[which(pdat$seas=="Fall")] <- pdat$tmin[which(pdat$seas=="Fall")] - mean(pdat$tmin[which(pdat$seas=="Fall")])

pdat$tmax[which(pdat$seas=="Winter")] <- pdat$tmax[which(pdat$seas=="Winter")] - mean(pdat$tmax[which(pdat$seas=="Winter")])
pdat$tmax[which(pdat$seas=="Spring")] <- pdat$tmax[which(pdat$seas=="Spring")] - mean(pdat$tmax[which(pdat$seas=="Spring")])
pdat$tmax[which(pdat$seas=="Summer")] <- pdat$tmax[which(pdat$seas=="Summer")] - mean(pdat$tmax[which(pdat$seas=="Summer")])
pdat$tmax[which(pdat$seas=="Fall")] <- pdat$tmax[which(pdat$seas=="Fall")] - mean(pdat$tmax[which(pdat$seas=="Fall")])

pdat$tmean[which(pdat$seas=="Winter")] <- pdat$tmean[which(pdat$seas=="Winter")] - mean(pdat$tmean[which(pdat$seas=="Winter")])
pdat$tmean[which(pdat$seas=="Spring")] <- pdat$tmean[which(pdat$seas=="Spring")] - mean(pdat$tmean[which(pdat$seas=="Spring")])
pdat$tmean[which(pdat$seas=="Summer")] <- pdat$tmean[which(pdat$seas=="Summer")] - mean(pdat$tmean[which(pdat$seas=="Summer")])
pdat$tmean[which(pdat$seas=="Fall")] <- pdat$tmean[which(pdat$seas=="Fall")] - mean(pdat$tmean[which(pdat$seas=="Fall")])

pdat$ppt[which(pdat$seas=="Winter")] <- pdat$ppt[which(pdat$seas=="Winter")] - mean(pdat$ppt[which(pdat$seas=="Winter")])
pdat$ppt[which(pdat$seas=="Spring")] <- pdat$ppt[which(pdat$seas=="Spring")] - mean(pdat$ppt[which(pdat$seas=="Spring")])
pdat$ppt[which(pdat$seas=="Summer")] <- pdat$ppt[which(pdat$seas=="Summer")] - mean(pdat$ppt[which(pdat$seas=="Summer")])
pdat$ppt[which(pdat$seas=="Fall")] <- pdat$ppt[which(pdat$seas=="Fall")] - mean(pdat$ppt[which(pdat$seas=="Fall")])


# need roundDown function to define axis ranges

SeasLineColors = scale_color_manual(name="Season: ", breaks=c("Fall", "Spring", "Summer", "Winter"),
                                values = c("brown", "darkgreen", "red", "blue"))

PlotName <- "Season_Decadal_Anomaly_Lines" 
a <- ggplot(pdat, aes(x=startYr+5, y=tmax, shape=seas, colour=seas)) +
		#ylim(-1.75, 1.75) +
		geom_line(size=1) +
		SeasLineColors +
    scale_shape(name="Season: ") +
		geom_point(size=3.2) +
		ylab(expression(paste(Tmax, ~({}^o*F)))) + xlab("") +
    scale_x_continuous(breaks=c(1900, 1920, 1940, 1960, 1980, 2000))
		
b <- ggplot(pdat, aes(startYr+5, tmin, shape=seas, colour=seas)) +
		#ylim(-1.75, 1.75) +
		geom_line(size=1) +
		SeasLineColors +
    scale_shape(name="Season: ") +
		geom_point(size=3.2) +
		ylab(expression(paste(Tmin, ~({}^o*F)))) + xlab("") +
  scale_x_continuous(breaks=c(1900, 1920, 1940, 1960, 1980, 2000))
		
c <- ggplot(pdat, aes(startYr+5, tmean, shape=seas, colour=seas)) +
		#ylim(-1.75, 1.75) +
		geom_line(size=1) +
		SeasLineColors + 
    scale_shape(name="Season: ") +
    geom_point(size=3.2) +
		ylab(expression(paste(Tmean, ~({}^o*F)))) + xlab("") +
  scale_x_continuous(breaks=c(1900, 1920, 1940, 1960, 1980, 2000))
		
d <- ggplot(pdat, aes(startYr+5, ppt, shape=seas, colour=seas)) +
		#ylim(-325, 325) +
		geom_line(size=1) +
		SeasLineColors + 
    scale_shape(name="Season: ") +
    geom_point(size=3.2) +
		ylab("Precip (in/yr)") + xlab("") +
    scale_x_continuous(breaks=c(1900, 1920, 1940, 1960, 1980, 2000))
				
p1 = plot_grid(a, b, c, d, nrow=4, align="v")
title = ggdraw() + draw_label(paste(SiteID, " - Decadal Anomalies by Season"), 
                              fontface="bold", size=TitleSize, vjust=0.5)
legend = get_legend(a + theme(legend.position = "bottom"))
p2 = plot_grid(title, p1, legend, ncol=1, rel_heights = c(0.05, 1, 0.05)) 
p3 = add_sub(p2, paste("Anomaly = (Mean decadal value) - (mean of all decades) \n Decadal ranges = ", stepYrs, "-year steps from ", BeginYr, " to ", max(seasStepAvgs$startYr)+9, sep=""), y=0.5, hjust=0.5, size=12)
ggdraw(p3)

OFName <- paste0(OutDir, PlotName,".png")
ggsave(OFName, width=6.5, height=8.5, dpi=dpi,bg="white")


##Boxplots of anomolies  - from entire period of record----

	
	#  calc seasonal avg for each year then subtract ref mean	
tminSeas <- data.frame(tapply(baseData$TminF, list(baseData$yr,baseData$Season), FUN=mean))
tmaxSeas <- data.frame(tapply(baseData$TmaxF, list(baseData$yr,baseData$Season), FUN=mean))
tmeanSeas <- data.frame(tapply(baseData$TavgF, list(baseData$yr,baseData$Season), FUN=mean))
pptSeas  <- data.frame(tapply(baseData$PptIn, list(baseData$yr,baseData$Season),   FUN=mean))

  # need to swap this and next section so use tminSeas absolute value, then calc anomoly and plot			
	# departure from entire period of record
for(i in 1:4)
	{ 	tminSeas[,i] <- tminSeas[,i] - mean(tminSeas[,i])
	  	tmaxSeas[,i] <- tmaxSeas[,i] - mean(tmaxSeas[,i])
	  	tmeanSeas[,i] <- tmeanSeas[,i] - mean(tmeanSeas[,i])
		  pptSeas[,i] <- pptSeas[,i] - mean(pptSeas[,i])}

yrcat <- (round(unique(baseData$yr)/10)*10)   # year category (e.g., 1900,1900, 1910)
yrcat <- yrcat[1:(floor(length(yrcat)/10)*10)]  #remove incomplete decades at end of record
yLabPs <- length(unique(yrcat))

#Remove extra years from data
tmaxSeas <- tmaxSeas[1:(yLabPs*10),]
tminSeas <- tminSeas[1:(yLabPs*10),]
tmeanSeas <- tmeanSeas[1:(yLabPs*10),]
pptSeas <- pptSeas[1:(yLabPs*10),]

  #Melt data frame
GetDecadeSeasons = function(df){
  df$Year = as.numeric(row.names(df))
  df.m = reshape2::melt(df, id="Year")
  colnames(df.m) = c("Year", "Season", "Var")
  df.m$Decade = (floor((df.m$Year+5)/10))*10
  df.m$Season = factor(df.m$Season, levels=c("Winter", "Spring" ,"Summer", "Fall"))
  return(df.m)
}

tmaxDecadeSeasons = GetDecadeSeasons(tmaxSeas)
tminDecadeSeasons = GetDecadeSeasons(tminSeas)
tmeanDecadeSeasons = GetDecadeSeasons(tmeanSeas)
pptDecadeSeasons = GetDecadeSeasons(pptSeas)

 SeasBoxColors = scale_fill_manual(name="Season: ", values=c("lightblue", "lightgreen", "lightpink", "linen"))
 #Only run decadal plots for CONUS using nClimGrid b/c CRU data starts at a different time
 if (State %!in% c('AK', 'HI', 'AS', 'GU', 'MP', 'VI', 'PR')){
 
a = ggplot(tmaxDecadeSeasons, aes(x=factor(Decade), y=Var, fill=Season)) + geom_boxplot() +
  SeasBoxColors + labs(x="", y=expression(paste("Tmax (", degree*F,")", sep=""))) 
b = ggplot(tminDecadeSeasons, aes(x=factor(Decade), y=Var, fill=Season)) + geom_boxplot() +
  SeasBoxColors + labs(x="", y=expression(paste("Tmin (", degree*F,")", sep="")))
c = ggplot(tmeanDecadeSeasons, aes(x=factor(Decade), y=Var, fill=Season)) + geom_boxplot() +
  SeasBoxColors + labs(x="", y=expression(paste("Tmean (", degree*F,")", sep="")))
d = ggplot(pptDecadeSeasons, aes(x=factor(Decade), y=Var, fill=Season)) + geom_boxplot() +
  SeasBoxColors + labs(x="", y="Ppt (in/year)")

PlotName <- "Seas_Decadal_Tmax_Tmin_Anomaly_Box"
p1 = plot_grid(a, b, nrow=2, align="v")
title = ggdraw() + draw_label(paste(SiteID, "- Annual Anomalies by Season and Decade"), 
                              fontface="bold", size=TitleSize, vjust=0.5)
legend = get_legend(a + theme(legend.position = "bottom"))
p2 = plot_grid(title, p1, legend, ncol=1, rel_heights = c(0.1, 1, 0.05)) 
p3 = add_sub(p2, paste("Anomaly = (Mean annual value) - (mean of all years) \n Decadal ranges = ", stepYrs, "-year steps from ", BeginYr, " to ", max(tmaxDecadeSeasons$Decade+4), sep=""), y=0.5, hjust=0.5, size=12)
ggdraw(p3)

OFName <- paste0(OutDir, PlotName,".png")
ggsave(OFName, width=8, height=6, dpi=dpi,bg="white")

PlotName <- "Seas_Decadal_Tmean_Ppt_Anomaly Box"
p1 = plot_grid(c, d, nrow=2, align="v")
title = ggdraw() + draw_label(paste(SiteID, "- Annual Anomalies by Season and Decade"), 
                              fontface="bold", size=TitleSize, vjust=0.5)
legend = get_legend(a + theme(legend.position = "bottom"))
p2 = plot_grid(title, p1, legend, ncol=1, rel_heights = c(0.1, 1, 0.05)) 
p3 = add_sub(p2, paste("Anomaly = (Mean annual value) - (Mean of all years) \n Decadal ranges = ", stepYrs, "-year steps from ", BeginYr, " to ", max(tmaxDecadeSeasons$Decade+4), sep=""), y=0.5, hjust=0.5, size=12)
ggdraw(p3)

OFName <- paste0(OutDir, PlotName,".png")
ggsave(OFName, width=8, height=6, dpi=dpi,bg="white")

	
##Box Plots - Decadal Avgs Annual & Seasonal----
nSteps = floor((EndYr - BeginYr)/stepYrs)	# year steps 
stepVal <- rep(0,nSteps)
midVal <- floor(stepYrs/2)

for(i in 0:(nSteps-1))
  stepVal[(1+i*stepYrs):((i+1)*stepYrs)] <- rep(midVal + i*stepYrs + BeginYr, stepYrs)

stepData <- yrAvgs[yrAvgs$cYr >= BeginYr,]
stepData <- stepData[1:length(stepVal),]
stepData$stepVal <- stepVal

PlotName <- "Decadal_Avg_Tmin_Tmax_Tmean_Ppt_Box"

a = ggplot(stepData, aes(x=factor(stepVal), y=tmaxAvg)) + geom_boxplot(fill="red") +
  labs(x="", y=expression(paste("Tmax (", degree*F,")", sep=""))) +
  scale_x_discrete(breaks=c("1910", "1930", "1950", "1970", "1990", "2010"))
b = ggplot(stepData, aes(x=factor(stepVal), y=tminAvg)) + geom_boxplot(fill="blue") +
  labs(x="", y=expression(paste("Tmin (", degree*F,")", sep=""))) +
  scale_x_discrete(breaks=c("1910", "1930", "1950", "1970", "1990", "2010"))
c = ggplot(stepData, aes(x=factor(stepVal), y=tminAvg)) + geom_boxplot(fill="tan") +
  labs(x="", y=expression(paste("Tmean (", degree*F,")", sep=""))) +
  scale_x_discrete(breaks=c("1910", "1930", "1950", "1970", "1990", "2010"))
d = ggplot(stepData, aes(x=factor(stepVal), y=pptAvg)) + geom_boxplot(fill="light blue") + 
  labs(x="", y="Precip (in/yr)") +
  scale_x_discrete(breaks=c("1910", "1930", "1950", "1970", "1990", "2010"))

p1 = plot_grid(a,b,c,d, nrow=2, ncol=2, align="v")
title = ggdraw() + draw_label(paste(SiteID, "- Annual Climate Means By Decade"), 
                              fontface="bold", size=TitleSize, vjust=0.5)
p2 = plot_grid(title, p1, ncol=1, rel_heights = c(0.05, 1)) 
#if (State %in% c('AK', 'HI', 'AS', 'GU', 'MP', 'PR', 'VI')){
p3 = add_sub(p2, paste("Decadal ranges = ", stepYrs, "-year steps from ", BeginYr, " to ", max(tmaxDecadeSeasons$Decade+4), sep=""), y=0.5, hjust=0.5, size=12)
#} else{}
p3 = add_sub(p2, paste("Decadal ranges = ", stepYrs, "-year steps from ", BeginYr, " to ", EndYr, sep=""), y=0.5, hjust=0.5, size=12)
#}
ggdraw(p3)
OFName = paste0(OutDir, PlotName,".png")
ggsave(OFName, width=8.5, height=8.5,bg="white")


		####  Period (e.g. decadal) averages for all seasons  #####

##  ========= Decadal values by season
PlotName = "Decadal_Seasonal_Tmax_Box"
p1 = ggplot(tmaxDecadeSeasons, aes(x=as.character(Decade), y=Var, fill=Season)) + geom_boxplot() +
  facet_wrap(~ Season, nrow=2, ncol=2) +
  SeasBoxColors + 
  labs(x="", y=expression(paste("Maximum Temperature (", degree*F,")", sep="")), title=paste(SiteID, "- Annual Tmax Anomalies by Decade and Season")) +
  theme(strip.text = element_text(size=16)) +
  scale_x_discrete(breaks=c("1910", "1930", "1950", "1970", "1990", "2010")) 
p2 = add_sub(p1, paste("Anomaly = (Mean annual value) - (Mean of all years) \n Decadal ranges = ", stepYrs, "-year steps from ", BeginYr, " to ", max(tmaxDecadeSeasons$Decade+4), sep=""), y=0.5, hjust=0.5, size=12)
ggdraw(p2)
OFName = paste0(OutDir, PlotName,  ".png")
ggsave(OFName, width=8.5, height=8.5,bg="white")

PlotName = "Decadal_Seasonal_Tmin_Box"
p1 = ggplot(tminDecadeSeasons, aes(x=as.character(Decade), y=Var, fill=Season)) + geom_boxplot() +
  facet_wrap(~ Season, nrow=2, ncol=2) +
  SeasBoxColors + 
  labs(x="", y=expression(paste("Minimum Temperature (", degree*F,")", sep="")), title=paste(SiteID, "- Annual Tmin Anomalies by Decade and Season")) + 
  theme(strip.text = element_text(size=16)) + 
  scale_x_discrete(breaks=c("1910", "1930", "1950", "1970", "1990", "2010"))
p2 = add_sub(p1, paste("Anomaly = (Mean annual value) - (Mean of all years) \n Decadal ranges = ", stepYrs, "-year steps from ", BeginYr, " to ", max(tmaxDecadeSeasons$Decade+4), sep=""), y=0.5, hjust=0.5, size=12)
ggdraw(p2)
OFName = paste0(OutDir, PlotName,".png")
ggsave(OFName, width=8.5, height=8.5,bg="white")

PlotName = "Decadal_Seasonal_Tmean_Box"
p1 = ggplot(tmeanDecadeSeasons, aes(x=as.character(Decade), y=Var, fill=Season)) + geom_boxplot() +
  facet_wrap(~ Season, nrow=2, ncol=2) +
  SeasBoxColors + 
  labs(x="", y=expression(paste("Mean Temperature (", degree*F,")", sep="")), title=paste(SiteID, "- Annual Tmean Anomalies by Decade and Season")) + 
  theme(strip.text = element_text(size=16)) + 
  scale_x_discrete(breaks=c("1910", "1930", "1950", "1970", "1990", "2010"))
p2 = add_sub(p1, paste("Anomaly = (Mean annual value) - (Mean of all years) \n Decadal ranges = ", stepYrs, "-year steps from ", BeginYr, " to ", max(tmaxDecadeSeasons$Decade+4), sep=""), y=0.5, hjust=0.5, size=12)
ggdraw(p2)
OFName = paste0(OutDir, PlotName,".png")
ggsave(OFName, width=8.5, height=8.5,bg="white")

PlotName = "Decadal_Seasonal_Ppt_Box"
p1 = ggplot(pptDecadeSeasons, aes(x=as.character(Decade), y=Var, fill=Season)) + geom_boxplot() +
  facet_wrap(~ Season, nrow=2, ncol=2) +
  SeasBoxColors + 
  labs(x="", y=expression("Precipitation (in/mon)"), title=paste(SiteID, "- Annual Precip Anomalies by Decade and Season")) + 
  theme(strip.text = element_text(size=16)) + 
  scale_x_discrete(breaks=c("1910", "1930", "1950", "1970", "1990", "2010"))
p2 = add_sub(p1, paste("Anomaly = (Mean annual value) - (Mean of all years) \n Decadal ranges = ", stepYrs, "-year steps from ", BeginYr, " to ", max(tmaxDecadeSeasons$Decade+4), sep=""), y=0.5, hjust=0.5, size=12)
ggdraw(p2)
OFName = paste0(OutDir, PlotName,".png")
ggsave(OFName, width=8.5, height=8.5,bg="white")

} else{}

#--------------------------------------------------------------------------#
#RED BLUE anomaly plots---- 
#--------------------------------------------------------------------------#

mtempMin = mean(yrAvgs$tminAvg)
mtempMax = mean(yrAvgs$tmaxAvg)
mtempMean = mean(yrAvgs$tmeanAvg)
mppt = mean(yrAvgs$pptAvg)

mTmin = mean(refData$TminF)
mTmax = mean(refData$TmaxF)
mTmean = mean(refData$TavgF)
mPrcp = mean(refData$PptIn)

Annual.Anomaly = data.frame(cYr = yrAvgs$cYr,
                            aTmax = yrAvgs$tmaxAvg - mtempMax,
                            aTmin = yrAvgs$tminAvg - mtempMin,
                            aTmean = yrAvgs$tmeanAvg - mtempMean,
                            aPpt = yrAvgs$pptAvg - mppt)
Annual.Anomaly$aTmax.col = ifelse(Annual.Anomaly$aTmax > 0, "red", "blue")
Annual.Anomaly$aTmin.col = ifelse(Annual.Anomaly$aTmin > 0, "red", "blue")
Annual.Anomaly$aTmean.col = ifelse(Annual.Anomaly$aTmean > 0, "red", "blue")
Annual.Anomaly$aPpt.col = ifelse(Annual.Anomaly$aPpt > 0, "green", "brown")

PlotName <- "Red_Blue_Anomaly_Bar_plot"

  # Tmax
a = ggplot(Annual.Anomaly, aes(x=cYr, y=aTmax, fill=aTmax.col)) + 
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("blue", "red")) +
  labs(x="", y=expression(paste("Tmax (", degree*F,")", sep=""))) + 
  scale_x_continuous(breaks=c(1900, 1920, 1940, 1960, 1980, 2000, 2020))

  #Tmin
b = ggplot(Annual.Anomaly, aes(x=cYr, y=aTmin, fill=aTmin.col)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("blue", "red")) +
  labs(x="", y=expression(paste("Tmin (", degree*F,")", sep=""))) +  
  scale_x_continuous(breaks=c(1900, 1920, 1940, 1960, 1980, 2000, 2020))

  #Tmean 
c = ggplot(Annual.Anomaly, aes(x=cYr, y=aTmean, fill=aTmean.col)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("blue", "red")) +
  labs(x="", y=expression(paste("Tmean (", degree*F,")", sep=""))) +
  scale_x_continuous(breaks=c(1900, 1920, 1940, 1960, 1980, 2000, 2020))

  #Precip
d = ggplot(Annual.Anomaly, aes(x=cYr, y=aPpt, fill=aPpt.col)) + 
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("brown", "green")) +
  labs(x="", y="Precip (in/yr)") +
  scale_x_continuous(breaks=c(1900, 1920, 1940, 1960, 1980, 2000, 2020))

p1 = plot_grid(a,b,c,d, nrow=4, align="v")
title = ggdraw() + draw_label(paste(SiteID, "- Annual Climate Anomalies"), 
                              fontface="bold", size=TitleSize, vjust=0.5)
p2 = plot_grid(title, p1, ncol=1, rel_heights = c(0.05, 1)) 
p3 = add_sub(p2, paste("Anomaly = (Annual value) - (Mean of all years) \nData range = ", BeginYr, "-", EndYr, sep=""), y=0.5, hjust=0.5, size=12)
ggdraw(p3)
OFName <- paste0(OutDir, PlotName,".png")
ggsave(OFName, width=6.5, height=8.5,bg="white")

#-------------------------------------------------#
#Decadal barplots----
#-------------------------------------------------#
head(baseData)
baseData$decade <- baseData$yr - baseData$yr %% 10
DecDat<-aggregate(cbind(TminF,TmaxF,TavgF)~decade,baseData,mean,na.rm=TRUE)
DecDat1<-aggregate(PptIn~yr,baseData,sum,na.rm=TRUE)
DecDat1$decade<-DecDat1$yr - DecDat1$yr %% 10
DecDat2<-aggregate(PptIn~decade,DecDat1,mean,na.rm=TRUE)
DecDat<-merge(DecDat,DecDat2,by="decade"); rm(DecDat1,DecDat2)
DecDat$decade <- DecDat$decade + 5    # centers bars at YYY5 (the mid-point of data) instead of YYY0 (the beginning year of data)

DecDat$Atmin<-DecDat$TminF-mean(DecDat$TminF)
DecDat$Atmax<-DecDat$TmaxF-mean(DecDat$TmaxF)
DecDat$Atmean<-DecDat$TavgF-mean(DecDat$TavgF)
DecDat$Appt<-DecDat$PptIn-mean(DecDat$PptIn)
# DecDat$Appt[1]<-(DecDat$ppt[1]*2)-mean(DecDat$ppt)

DecDat$Atmax.col = ifelse(DecDat$Atmax > 0, "red", "blue")
DecDat$Atmin.col = ifelse(DecDat$Atmin > 0, "red", "blue")
DecDat$Atmean.col = ifelse(DecDat$Atmean > 0, "red", "blue")
DecDat$Appt.col = ifelse(DecDat$Appt > 0, "green", "brown")

PLOT<-"Historical_trends_Decadal_barplot"
# Tmax
ggplot(DecDat, aes(x=decade, y=Atmax, fill=Atmax.col)) + 
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("blue", "red")) +
  labs(title="Average Decadal Max Temperature Climate Anomaly",x="", y=expression(paste("Tmax (", degree*F,")", sep=""))) + 
  scale_x_continuous(breaks=c(1900, 1920, 1940, 1960, 1980, 2000, 2020)) 

ggsave(paste(PLOT,"_Tmax.png",sep=""), path = OutDir, width = 15, height = 9,bg="white")

#Tmin
ggplot(DecDat, aes(x=decade, y=Atmin, fill=Atmin.col)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("blue", "red")) +
  labs(title="Average Decadal Min Temperature Climate Anomaly",x="", y=expression(paste("Tmin (", degree*F,")", sep=""))) +  
  scale_x_continuous(breaks=c(1900, 1920, 1940, 1960, 1980, 2000, 2020))
ggsave(paste(PLOT,"_Tmin.png",sep=""), path = OutDir, width = 15, height = 9,bg="white")

#Tmean 
ggplot(DecDat, aes(x=decade, y=Atmean, fill=Atmean.col)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("blue", "red")) +
  labs(title="Average Decadal Mean Temperature Climate Anomaly",x="", y=expression(paste("Tmean (", degree*F,")", sep=""))) +
  scale_x_continuous(breaks=c(1900, 1920, 1940, 1960, 1980, 2000, 2020))
ggsave(paste(PLOT,"_Tmean.png",sep=""), path = OutDir, width = 15, height = 9,bg="white")

#Precip
ggplot(DecDat, aes(x=decade, y=Appt, fill=Appt.col)) + 
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("brown", "dark green")) +
  labs(title="Average Decadal Annual Precipitation Climate Anomaly",x="", y="Precip (in/yr)") +
  scale_x_continuous(breaks=c(1900, 1920, 1940, 1960, 1980, 2000, 2020))
ggsave(paste(PLOT,"_Ppt.png",sep=""), path = OutDir, width = 15, height = 9,bg="white")


#SPEI calculations----

head(baseData)
baseData$Tmean_C<-(baseData$TavgF - 32) * 5/9
baseData$Precip<- baseData$PptIn *25.4
baseData$PET<-thornthwaite(baseData$Tmean_C, Lat)
SPEI<-spei(baseData$Precip - baseData$PET, 6)
baseData$SPEI<-SPEI$fitted

spei<-subset(baseData,select=c(yr,mon,SPEI))
spei$date<-as.Date(paste(spei$yr,spei$mon,"01",sep="-"),format="%Y-%m-%d")

spei$col[spei$SPEI>=0]<-"wet"
spei$col[spei$SPEI<0]<-"dry"
spei$col<-factor(spei$col, levels=c("wet","dry"))

ggplot(data = spei, aes(x = date, y = SPEI,fill = col)) + 
  geom_bar(stat="identity",aes(fill=col)) +
  scale_fill_manual(name="",values =c("blue","red")) +
  theme(axis.text=element_text(size=20),axis.title.x=element_text(size=20,vjust=-0.2),
        axis.title.y=element_text(size=18,vjust=0.8),
        plot.title=element_text(size=18,face="bold",vjust=2,hjust=0.5),
        legend.text=element_text(size=20), legend.title=element_text(size=20)) +
  labs(title = "SPEI values for Historical Period", 
       x = "Date", y = "SPEI") +
  guides(color=guide_legend(override.aes = list(size=7))) + 
  scale_y_continuous(limits=c(min(spei$SPEI)-.5, max(spei$SPEI)+1)) #warning message that 11 rows are removed will occur due to the rolling mean, ignore warning
ggsave("Historical_SPEI.png", path = OutDir, width = 15, height = 9,bg="white")

 spei$decade <- spei$yr - spei$yr %% 10
 table(spei$decade)
 #spei.decade <- spei %>% group_by(spei$decade) %>% summarise(SPEI=mean(spei$SPEI, na.rm=TRUE))
 spei.decade <- ddply(spei, c("decade"), summarise,
                       SPEI = mean(SPEI, na.rm=TRUE))

spei.decade$col[spei.decade$SPEI>=0]<-"wet"
spei.decade$col[spei.decade$SPEI<0]<-"dry"
spei.decade$col<-factor(spei.decade$col, levels=c("wet","dry"))

ggplot(data = spei.decade, aes(x = decade, y = SPEI,fill = col)) + 
  geom_bar(stat="identity",aes(fill=col)) +
  scale_fill_manual(name="",values =c("blue","red")) +
  theme(axis.text=element_text(size=20),axis.title.x=element_text(size=20,vjust=-0.2),
        axis.title.y=element_text(size=18,vjust=0.8),
        plot.title=element_text(size=18,face="bold",vjust=2,hjust=0.5),
        legend.text=element_text(size=20), legend.title=element_text(size=20)) +
  labs(title = "Average SPEI values by decade", 
       x = "Date", y = "SPEI") +
  guides(color=guide_legend(override.aes = list(size=7))) + 
  scale_y_continuous(limits=c(min(spei$SPEI)-.5, max(spei$SPEI)+1)) #warning message that 11 rows are removed will occur due to the rolling mean, ignore warning
ggsave("Decadal_SPEI.png", path = OutDir, width = 15, height = 9,bg="white")


#---------------------------------------------------------------------------------------------------#
#  Quantile regression plots for annual data, seasonal, or monthly periods
# This section taken from quantiles.R (author: J. Gross): "Source code for NCDC Daily Summary Report"
#---------------------------------------------------------------------------------------------------#

Temps <- baseData %>% dplyr::select(Date, yr, TmaxF, TminF, mon) %>%
  mutate(month = mon, jDay = yday(Date),
         monD = month(Date) + day(Date) * .032,
         seas = case_when( month %in% c(12,1,2) ~ "a.Winter",
                           month %in% c(3,4,5)  ~ "b.Spring",
                           month %in% c(6,7,8) ~  "c.Summer",
                           month %in% c(9,10,11) ~ "d.Fall"))
Temps$month = factor(Temps$month, levels=c(1,2,3,4,5,6,7,8,9,10,11,12),
                        labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                        exclude=NA, ordered = TRUE)

#  Annual - TminF and TmaxF
ggplot(Temps, aes(yr, TminF)) + geom_jitter(color='light blue', size=.35) + 
  # ggtitle(paste(month.name[MonthToPlot], "Tmax quantiles ", paste(taus, collapse=", "))) +
  geom_quantile(quantiles = taus, color="blue") + geom_quantile(quantiles = .5, color='black') +
  ggtitle(paste0(SiteID," - Annual TminF, Quantile Regression")) +
  xlab("Year") +
  ylab("Min. Temperature (F)") +
  labs(caption = "Fitted quantile series are, from bottom, 1st, 5th, 25th, median (black line), 75th, 95th, and 99th.")
ggsave(paste(OutDir, "Tmin_Annual_Quantiles_plot.png", sep=""), width = 10, height = 7, units = "in")

ggplot(Temps, aes(yr, TmaxF)) + geom_jitter(color='light blue', size=.35) + 
  # ggtitle(paste(month.name[MonthToPlot], "Tmax quantiles ", paste(taus, collapse=", "))) +
  geom_quantile(quantiles = taus, color="blue") + geom_quantile(quantiles = .5, color='black') +
  ggtitle(paste0(SiteID," - Annual TmaxF, Quantile Regression")) + 
  xlab("Year") +
  ylab("Max. Temperature (F)") +
  labs(caption = "Fitted quantile series are, from bottom, 1st, 5th, 25th, median (black line), 75th, 95th, and 99th.")
ggsave(paste(OutDir, "Tmax_Annual_Quantiles_plot.png", sep=""), width = 10, height = 7, units = "in")

#  by Month
ggplot(Temps, aes(yr, TminF)) + geom_jitter(color='light blue', size=.35) + 
  #ggtitle(paste(month.name[MonthToPlot], "Tmax quantiles ", paste(taus, collapse=", "))) +
  geom_quantile(quantiles = taus, color="blue") + geom_quantile(quantiles = .5, color='black') +
  facet_wrap(~month) +
  ggtitle(paste0(SiteID," - TminF by Month, Quantile Regression")) +
  xlab("Year") +
  ylab("Min. Temperature (F)") + 
  scale_x_continuous(n.breaks=4) +
  labs(caption = "Fitted quantile series are, from bottom, 1st, 5th, 25th, median (black line), 75th, 95th, and 99th.")
ggsave(paste(OutDir, "Tmin_by_Month_Quantile_Facet_plots.png", sep=""), width = 10, height = 7, units = "in")
                         
quantRegResults_tmin <- rq(Temps$TminF ~ Temps$yr, taus)   # quantile stats if desired
quantRegResults_tmin$coefficients
quantRegResults_tmax <- rq(Temps$TmaxF ~ Temps$yr, taus)   # quantile stats if desired
quantRegResults_tmax$coefficients

ggplot(Temps, aes(yr, TmaxF)) + geom_jitter(color='light blue', size=.15) + 
  #ggtitle(paste(month.name[MonthToPlot], "Tmax quantiles ", paste(taus, collapse=", "))) +
  geom_quantile(quantiles = taus, color="blue") + geom_quantile(quantiles = .5, color='black') +
  facet_wrap(~month) +
  ggtitle(paste0(SiteID," - TmaxF by Month, Quantile Regression")) +
  xlab("Year") +
  ylab("Max. Temperature (F)") +
  scale_x_continuous(n.breaks=4) +
  labs(caption = "Fitted quantile series are, from bottom, 1st, 5th, 25th, median (black line), 75th, 95th, and 99th.")
ggsave(paste(OutDir, "Tmax_by_Month_Quantile_Facet_plots.png", sep=""), width = 10, height = 7, units = "in")

# by Season
ggplot(Temps, aes(yr, TminF)) + geom_jitter(color='light blue', size=.35) + 
  # ggtitle(paste(month.name[MonthToPlot], "Tmax quantiles ", paste(taus, collapse=", "))) +
  geom_quantile(quantiles = taus, color="blue") + geom_quantile(quantiles = .5, color='black') +
  facet_wrap(~seas) +
  xlab("Year") +
  ylab("Min. Temperature (F)") +
  scale_x_continuous(n.breaks=5) +
  labs(caption = "Fitted quantile series are, from bottom, 1st, 5th, 25th, median (black line), 75th, 95th, and 99th.") +
  ggtitle(paste0(SiteID," - TminF by Season, Quantile Regression"))
ggsave(paste(OutDir, "Tmin_by_season_Quantile_Facet_plots.png", sep=""), width = 10, height = 7, units = "in")

ggplot(Temps, aes(yr, TmaxF)) + geom_jitter(color='light blue', size=.15) + 
  # ggtitle(paste(month.name[MonthToPlot], "Tmax quantiles ", paste(taus, collapse=", "))) +
  geom_quantile(quantiles = taus, color="blue") + geom_quantile(quantiles = .5, color='black') +
  facet_wrap(~seas) +
  xlab("Year") +
  ylab("Max. Temperature (F)") +
  scale_x_continuous(n.breaks=5) +
  ggtitle(paste0(SiteID," - TmaxF by Season, Quantile Regression")) + 
  labs(caption = "Fitted quantile series are, from bottom, 1st, 5th, 25th, median (black line), 75th, 95th, and 99th.")
ggsave(paste(OutDir, "Tmax_by_season_Quantile_Facet_plots.png", sep=""), width = 10, height = 7, units = "in")


#zip up the folder of output
zip::zip(paste0(SiteID,"_Historical.zip"), paste0(SiteID,"-Historical"))

rm(list=ls()[! ls() %in% c("parklist", "testlist", "nps_boundary", "nps_boundary2", "nps_cent", "conus_cent2", 
                           "nonconus_cent2", "wgs84", "EndYr", "county_climate", "point_climate", "poly_climate", 
                           "doP1", "doP2", "endRefYr", "stepYrs", "rollLen", "dpi", "dataEndYr", "dte", "SeasBoxColors",
                           "CRUDataDir", "nClimDataDir", "LocalDir", "nclim.file.list", "cru.file.list", "cru_all", 
                           "crudaterange", "cruvars", "GetSeason", "PlotTheme", "TitleSize", "taus", "%!in%")])

  }


### EOF ###
###########