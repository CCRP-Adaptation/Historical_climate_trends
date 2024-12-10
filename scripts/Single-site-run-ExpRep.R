
library(here); library(plyr); # Use here::here when package lubridate is used
library(plotrix); library(zoo); library(ggplot2); library(grid); library(cowplot); library(reshape2); library(raster); library(ncdf4); library(reshape2); library(WriteXLS); library(data.table); library(RColorBrewer); library(ggrepel); library(lubridate); library(dplyr); library(forcats); library(openxlsx); library(sf); library(raster); library(rgdal); library(R.utils); library(tmap); library(tmaptools); library(ggmap); library(ggspatial);
library(gridExtra); library(SPEI); library(tidyr); library(tibble); library(sp); library(skimr);  library(stringr); library(ggpubr); library(lemon);library(rvest);library(tidyverse);library(XML);library(xml2);library(curl);library(tidync); library(viridis); library(robustbase)

rm(list=ls())

SiteID <- "BLRV" #From Koppen #27 site
Lat = 41.939028
Lon = -71.431884

DataDir <- "C:/Users/arunyon/3D Objects/Local-files/NOAA-data/nclim_2405/"
# OutDir <- "C:/Users/arunyon/3D Objects/Local-files/RCF_Testing/OLYM-Quinault-Historical/"
OutDir <- paste0("C:/Users/arunyon/OneDrive - DOI/CFs_For_Kaylin/Additional-sites/",SiteID,"-Historical/")
if(dir.exists(OutDir) == FALSE){
  dir.create(OutDir)
}

BeginYr = 1895
EndYr = 2023
beginRefYr = 1900
endRefYr = 1970
# needed for rolling mean plot below.  
stepYrs	= 10		  # for period plots 
rollLen = 10      # period of calc for rolling average; default 10 = decadal
dpi = 600 

file.list = list.files(path = DataDir, pattern = '.nc', full.names = TRUE)

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

write.csv(baseData, (sprintf("%s%s_nClimGrid.csv", OutDir, SiteID)),row.names=FALSE)


baseData$Date<- as.Date(baseData$Date,format="%Y-%m-%d")
baseData$Year = as.integer(format(baseData$Date,format="%Y"))

Annual = baseData |> group_by(Year) |> summarise(PptIn = sum(PptIn),
                                                    TavgF = mean(TavgF))
Annual$PptP2 <- ifelse(Annual$Year>=endRefYr, Annual$PptIn, NA)
Annual$TavgP2  <- ifelse(Annual$Year>=endRefYr, Annual$TavgF, NA)
write.csv(Annual, paste0(OutDir,SiteID,"-Annual-Averages.csv"), row.names=FALSE)
# Annual$Year <- Date(Annual$Year,format="%Y")

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
  A <- Annual
  #Regressions for trends----
  # lmTmax <- lmrob(yrAvgs$tmaxAvg~cYr)
  # lmTmaxP1 <- lmrob(yrAvgs$tmaxP1~cYr)
  # lmTmaxP2 <- lmrob(yrAvgs$tmaxP2~cYr)
  #
  # lmTmin <- lmrob(yrAvgs$tminAvg~cYr)
  # lmTminP1 <- lmrob(yrAvgs$tminP1~cYr)
  # lmTminP2 <- lmrob(yrAvgs$tminP2~cYr)
  
  lmTmean <- lmrob(TavgF~Year,A)
  lmTmeanP2 <- lmrob(TavgP2~Year,A)
  
  lmPpt  <- lmrob(PptIn~Year,A)
  lmPptP2 <- lmrob(PptP2~Year,A)
  
  # make table of coefficients
  probStar <- function(pVal){
    probStar <- "NS"
    if(pVal < 0.05)probStar <- "*"
    if(pVal < 0.01)probStar <- "**"
    if(pVal < 0.001)probStar <- "***"
    probStar
  }
  
  # regsTmax <-  rbind(lmMetrics(lmTmax), lmMetrics(lmTmaxP1), lmMetrics(lmTmaxP2))
  # regsTmin <-  rbind(lmMetrics(lmTmin), lmMetrics(lmTminP1), lmMetrics(lmTminP2))
  regsTmean <- rbind(lmMetrics(lmTmean),lmMetrics(lmTmeanP2))
  regsPpt <-   rbind(lmMetrics(lmPpt),lmMetrics(lmPptP2))
  
  perAll <- paste(min(Annual$Year), max(Annual$Year), sep="-")
  per2 <- paste(endRefYr, max(Annual$Year), sep="-")
  Period <- rep(c(perAll, per2), 2)
  
  lmTable <- cbind( Var=rep(c("Tmean", "Precip"),each=2), Period, rbind(regsTmean, regsPpt))
  
  lmTable$YrCoeff <- lmTable$YrCoeff * 100   # convert to degF(in)/100-yrs
  lmTable$seSlope <- lmTable$seSlope * 100
  #add units to YrCoeff field
  colnames(lmTable) <- c("Var", "Period", "YrCoeff(degF(in)/100yrs)", "seSlope", "probCoeff", "probSign", "r2")
  
  print(lmTable, row.names = F)
  
  write.csv(lmTable, paste0(OutDir, SiteID,"-Regression Table-ExposureReport.csv"), row.names=FALSE)
  # write.csv(Annual, paste0(LocalDir, SiteID[i],"-Annual-Averages.csv"),row.names=FALSE)  
  
  ####### Identify anomalies ####

  hist.tmean.98th <- quantile(A$TavgF[which(A$Year < 1995)], .98)
  hist.anomalies.tmean <- A$Year[which(A$TavgF > hist.tmean.98th & A$Year < 1995)] # Anomaly years, above 98th
  recent.percent.tmean.anomaly <- length(A$Year[which(A$TavgF > hist.tmean.98th & A$Year > 2000)])/
    length(A$Year[which(A$Year > 2000)]) * 100 # Percent recent years above hist 98th

  hist.above.prcp.98th <- quantile(A$PptIn[which(A$Year < 1995)], .98)
  hist.anomalies.above.prcp <- A$Year[which(A$PptIn > hist.above.prcp.98th & A$Year < 1995)] # Anomaly years, above 98th
  recent.percent.above.prcp.anomaly <- length(A$Year[which(A$PptIn > hist.above.prcp.98th & A$Year > 2000)])/
    length(A$Year[which(A$Year > 2000)]) * 100 # Percent recent years above hist 98th

  hist.below.prcp.98th <- quantile(A$PptIn[which(A$Year < 1995)], .02)
  hist.anomalies.below.prcp <- A$Year[which(A$PptIn < hist.below.prcp.98th & A$Year < 1995)] # Anomaly years, above 98th
  recent.percent.below.prcp.anomaly <- length(A$Year[which(A$PptIn < hist.below.prcp.98th & A$Year > 2000)])/
    length(A$Year[which(A$Year > 2000)]) * 100 # Percent recent years above hist 98th

  anomalies.table <- data.frame(hist.tmean.98th,hist.anomalies.tmean,recent.percent.tmean.anomaly,hist.above.prcp.98th,hist.anomalies.above.prcp, recent.percent.above.prcp.anomaly,
                                hist.below.prcp.98th,hist.anomalies.below.prcp,recent.percent.below.prcp.anomaly,row.names = NULL)
  write.csv(anomalies.table, paste0(OutDir,SiteID, "-Anomalies-table.csv"),row.names=FALSE)

  a <- Annual  |> 
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
  
  b <- Annual  |> 
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
  
  title = ggdraw() + draw_label(paste(SiteID, " - Trends for Reference and Recent Historical Periods", sep=""), 
                                fontface="bold", size=TitleSize, vjust=0.5)
  p1 = plot_grid(a, b, nrow=2, align="v")
  p2 = plot_grid(title, p1, ncol=1, rel_heights = c(0.1, 1, 0.05)) 
  # p3 = add_sub(p2, paste("Gray shaded area around regression lines = standard error of predicted y's \nReference period: ", beginRefYr, "-", endRefYr, "; Recent period: ", endRefYr+1, "-", EndYr, "; Overall period: ", BeginYr, "-", EndYr, sep=""),
  #              y=.5, hjust=0.5, vjust=0.5, size=12)
  ggdraw(p2)
  
  OFName <- paste0(OutDir, SiteID, "_Historical_Trends.png")
  ggsave(OFName, width=9, height=6, dpi=dpi,bg="white")
  
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
  # OFName <- paste0(OutDir, SiteID[i], "_Historical_Trends-Anomalies.png")
  # ggsave(OFName, width=9, height=6, dpi=dpi,bg="white")
