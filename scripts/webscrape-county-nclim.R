########### Web scraping ################
# Tutorial: # https://medium.com/geekculture/reading-xml-files-in-r-3122c3a2a8d9

#Deal with dates and seasons
GetSeason <- function(DateVec){
  seas <- as.character(rep(NA, length(DateVec)))
  seas[which(format(DateVec,'%B') %in% c("December", "January", "February"))]<- "Winter"
  seas[which(format(DateVec,'%B') %in% c("March", "April", "May"))]<- "Spring"
  seas[which(format(DateVec,'%B') %in% c("June", "July", "August"))]<- "Summer"
  seas[which(format(DateVec,'%B') %in% c("September", "October", "November"))]<- "Fall"
  return(seas)
}

park<- centroid_county %>% mutate(COUNTY.3d = sprintf("%03d",centroid_county$COUNTYFP))%>% 
  filter(UNIT_CODE == SiteID)

vars <- c("tmax","tmin","pcp")

DF <- data.frame(date=paste0(rep(BeginYr:EndYr,each=12),sprintf("%02d",rep(1:12,times=(EndYr-BeginYr)))))

for (i in 1:length(vars)){
  url <- paste0("https://www.ncdc.noaa.gov/cag/county/time-series/",park$STATE,"-",
                park$COUNTY.3d,"/",vars[i],"/all/12/",BeginYr,"-",EndYr,".xml")
    xml <- read_xml(url)
  xml_parse <- xmlParse(xml)
  xml_df <- xmlToDataFrame(nodes=getNodeSet(xml_parse, "//data"))
  colnames(xml_df)[2] <- vars[i]
  xml_df[,2]<-as.numeric(xml_df[,2])
  DF <- merge(DF,xml_df,by="date")
}

colnames(DF) <- c("YearMon","TmaxF","TminF","PptIn")

baseData <- DF %>% mutate(TavgF = (TmaxF+TminF)/2,
                          Date = as.Date(paste(substr(YearMon, 1, 4),substr(YearMon, 5, 6),"15"),format="%Y%m%d"),
                          Season = GetSeason(Date))
rm(DF)
write.csv(baseData, (sprintf("%s%s_nClimGrid_county.csv", OutDir, SiteID)),row.names=FALSE)

# save.image(sprintf("%s%s_nClimGrid_county.RData", OutDir, SiteID ))


