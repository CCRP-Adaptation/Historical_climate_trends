baseData <- if(county_climate == "Y") {
  read.csv(paste0(OutDir, SiteID,"_nClimGrid_county.csv"),header=TRUE)
} else {
  read.csv(paste0(OutDir, SiteID,"_nClimGrid.csv"),header=TRUE) #replace with nclim_nc_extraction script
}

OutDir <- paste0(OutDir,"centroid/")
baseData <- read.csv(paste0(OutDir, SiteID,"_nClimGrid-centroid.csv"),header=TRUE)

dataEndYr = EndYr - 1 


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
dte = Sys.Date()
# clean up trashy namespace

baseData$yr<-year(baseData$Date)
baseData$mon<-month(baseData$Date)

refData<-baseData[baseData$yr >= beginRefYr & baseData$yr <= endRefYr,]

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


##########################
#Regressions for trends----
lmTmax <- lm(yrAvgs$tmaxAvg~cYr)
lmTmaxP1 <- lm(yrAvgs$tmaxP1~cYr)
lmTmaxP2 <- lm(yrAvgs$tmaxP2~cYr)

lmTmin <- lm(yrAvgs$tminAvg~cYr)
lmTminP1 <- lm(yrAvgs$tminP1~cYr)
lmTminP2 <- lm(yrAvgs$tminP2~cYr)

lmTmean <- lm(yrAvgs$tmeanAvg~cYr)
lmTmeanP1 <- lm(yrAvgs$tmeanP1~cYr)
lmTmeanP2 <- lm(yrAvgs$tmeanP2~cYr)

lmPpt  <- lm(yrAvgs$pptAvg~cYr)		
lmPptP1 <- lm(yrAvgs$pptP1~cYr)		
lmPptP2 <- lm(yrAvgs$pptP2~cYr)		

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

write.csv(lmTable, paste0(OutDir, " Regression Table test.csv"), row.names=FALSE)

#-----------------------------------------------------------#
#ANNUAL AVERAGE LINES WITH REGRESSION----
#-----------------------------------------------------------#
# Amend to incl. running means and dashed lines for significance
#    ggplot graphics    #
# annaul points, linear regression, and 95% CI
# need to manually set position of a, b, c labels

# gray zone is 95% confidence interval

PlotName = "Annual Means Lines Regressions"

a <- ggplot(yrAvgs) +	geom_smooth(method = lm, aes(cYr, tmaxAvg), na.rm=TRUE,linetype=if(summary(lmTmax)$coefficients[2,4]<0.05) {
  1
} else{2}) +
  geom_line(aes(cYr, tmaxAvg), na.rm=TRUE) + geom_point(aes(cYr, tmaxAvg), na.rm=TRUE) +
  ylab(expression(paste(Tmax, ~({}^o*F)))) + xlab("") +
  scale_x_continuous(breaks=c(1900, 1920, 1940, 1960, 1980, 2000)) +
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
  scale_x_continuous(breaks=c(1900, 1920, 1940, 1960, 1980, 2000))+
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
  scale_x_continuous(breaks=c(1900, 1920, 1940, 1960, 1980, 2000))

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
  scale_x_continuous(breaks=c(1900, 1920, 1940, 1960, 1980, 2000))

if(doP1 == "YES")d <- d + geom_smooth(method = lm, aes(cYr, pptP1), na.rm=TRUE,linetype=if(summary(lmPptP1)$coefficients[2,4]<0.05) {
  1
} else{2})
if(doP2 == "YES")d <- d + geom_smooth(method = lm, aes(cYr, pptP2), na.rm=TRUE,linetype=if(summary(lmPptP2)$coefficients[2,4]<0.05) {
  1
} else{2}) 

rm(lmPpt,lmPptP1,lmPptP2,lmTmax,lmTmaxP1,lmTmaxP2,lmTmin,lmTminP1,lmTminP2,lmTmean,lmTmeanP1,lmTmeanP2)
rm(regsTmax, regsTmin, regsTmean, regsPpt, lmTable)

#4-panel plot		
p1 = plot_grid(a, b, c, d, nrow=4, align="v")
title = ggdraw() + draw_label(paste(SiteID, " - Trends for Reference and Recent \nHistorical Periods", sep=""), 
                              fontface="bold", size=TitleSize, vjust=0.5)
p2 = plot_grid(title, p1, ncol=1, rel_heights = c(0.07, 1)) 
p3 = add_sub(p2, paste("Gray shaded area around regression lines = standard error of predicted y's \nReference period: ", beginRefYr, "-", endRefYr, "; Recent period: ", endRefYr+1, "-", EndYr, "; Overall period: ", BeginYr, "-", EndYr, sep=""),
             y=.5, hjust=0.5, vjust=0.5, size=12)
ggdraw(p3)

OFName <- paste0(OutDir, PlotName, " 4-panel.png")
ggsave(OFName, width=6.5, height=8.5, dpi=dpi)

#2-panel Tmax/Tmin plot
p1 = plot_grid(a, b, nrow=2, align="v")
p2 = plot_grid(title, p1, ncol=1, rel_heights = c(0.1, 1, 0.05)) 
p3 = add_sub(p2, paste("Gray shaded area around regression lines = standard error of predicted y's \nReference period: ", beginRefYr, "-", endRefYr, "; Recent period: ", endRefYr+1, "-", EndYr, "; Overall period: ", BeginYr, "-", EndYr, sep=""),
             y=.5, hjust=0.5, vjust=0.5, size=12)
ggdraw(p3)

OFName <- paste0(OutDir, PlotName, " Tmin Tmax.png")
ggsave(OFName, width=6.5, height=6.5, dpi=dpi)

#2-panel Tmean/Precip plot
p1 = plot_grid(c, d, nrow=2, align="v")
p2 = plot_grid(title, p1, ncol=1, rel_heights = c(0.1, 1, 0.05)) 
p3 = add_sub(p2, paste("Gray shaded area around regression lines = standard error of predicted y's \nReference period: ", beginRefYr, "-", endRefYr, "; Recent period: ", endRefYr+1, "-", EndYr, "; Overall period: ", BeginYr, "-", EndYr, sep=""),
             y=.5, hjust=0.5, vjust=0.5, size=12)
ggdraw(p3)

OFName <- paste0(OutDir, PlotName, " Tmean Precip.png")
ggsave(OFName, width=6.5, height=6.5, dpi=dpi)