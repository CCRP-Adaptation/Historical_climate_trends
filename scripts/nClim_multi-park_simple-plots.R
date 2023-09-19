SiteID = c("CAVE","GUMO") 

centroid_county <- read.csv(here::here("centroid_county.csv"))
BeginYr = 1895
EndYr = 2022

county_climate <- "N"
point_climate <- "Y"

doP1 <- "YES"  # Should a separate regression be calculated for the reference period (default 1900-1970)? 
doP2 <- "YES"  # Should a separate regression be calculate for the period after the reference period (default 1971-present)? 
beginRefYr = 1900
endRefYr = 1970
# needed for rolling mean plot below.  
stepYrs	= 10		  # for period plots 
rollLen = 10      # period of calc for rolling average; default 10 = decadal
dpi = 600 
Lat = centroid_county$Lat[which(centroid_county$UNIT_CODE %in% SiteID)]
Lon = centroid_county$Lon[which(centroid_county$UNIT_CODE %in% SiteID)]

# DataDir <- "E:/nClimGrid_nc/"
DataDir <- "C:/Users/arunyon/3D Objects/Local-files/NOAA-data/nclim_2302/"
LocalDir <- "C:/Users/arunyon/3D Objects/Local-files/RCF_Testing/"

nps_centroids <- st_read('C:/Users/arunyon/3D Objects/Local-files/Git-repos/CCRP_automated_climate_futures/data/general/spatial-data/nps_boundary_centroids/nps_boundary_centroids.shp')
centroid <- filter(nps_centroids, UNIT_CODE %in% SiteID)
centroid <- centroid[1]


file.list = list.files(path = DataDir, pattern = '.nc', full.names = TRUE)
file.list = file.list[1]

library(terra) #https://tmieno2.github.io/R-as-GIS-for-Economists/extracting-values-from-raster-layers-for-vector-data.html
t1 = terra::rast(file.list)
dim(t1)
plot(t1[[1]])
t2 <- terra::extract(t1, centroid, xy = T, method = "simple")
t3<-terra::extract(t1, vect(centroid))
t3$ID <- centroid$UNIT_CODE
t<-t3
colnames(t)[2:length(t)] = as.character(time(t1))
t<- t |> pivot_longer(!ID, names_to = "Date", values_to = substr(file.list, 72, 75))
substr(file.list, 72, 75)

baseData <- df %>% mutate(PptIn = prcp/25.4,
                          TmaxF = tmax * 9/5 + 32,
                          TminF = tmin * 9/5 + 32, 
                          TavgF = (TmaxF+TminF)/2,
                          YearMon = paste0(year(Date),sprintf("%02d",month(Date))),
                          Season = GetSeason(Date)) %>% 
  filter(Date <= paste0(EndYr,"-12-01"))

baseData <- t |> mutate(PptIn = prcp/25.4,
                        Date = as.Date(Date,format="%Y-%m-%d"),
                        Year = format(Date,format="%Y"),
                        Month = format(Date,format="%m"))

Annual = aggregate(PptIn~ID+Year,baseData,sum)
Annual$Year <- as.Date(Annual$Year,format="%Y")


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


##########################
#Regressions for trends----
# lmTmax <- lmrob(yrAvgs$tmaxAvg~cYr)
# lmTmaxP1 <- lmrob(yrAvgs$tmaxP1~cYr)
# lmTmaxP2 <- lmrob(yrAvgs$tmaxP2~cYr)
# 
# lmTmin <- lmrob(yrAvgs$tminAvg~cYr)
# lmTminP1 <- lmrob(yrAvgs$tminP1~cYr)
# lmTminP2 <- lmrob(yrAvgs$tminP2~cYr)
# 
# lmTmean <- lmrob(yrAvgs$tmeanAvg~cYr)
# lmTmeanP1 <- lmrob(yrAvgs$tmeanP1~cYr)
# lmTmeanP2 <- lmrob(yrAvgs$tmeanP2~cYr)

lmPpt  <- lm(PptIn~Year,Annual)		
lmPptP1 <- lmrob(yrAvgs$pptP1~cYr)		
lmPptP2 <- lmrob(yrAvgs$pptP2~cYr)		

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



