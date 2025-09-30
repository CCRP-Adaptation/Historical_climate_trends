library(dplyr)
library(ggplot2)
library(RColorBrewer)

head(yrAvgs)

head(data)

# display.brewer.pal(5, "YlOrRd")
# display.brewer.pal(5, "Blues")
# brewer.pal(5,"YlOrRd")
# brewer.pal(5, "Blues")

temp.pal <- c("#F03B20","#BD0026","#FD8D3C")
ggplot(yrAvgs, aes(x=cYr, y=tAvg)) + geom_line(aes(cYr), colour = temp.pal[1], na.rm=TRUE,size=.2) +
  ylab(expression(paste("Avg Temperature", ~({}^o*F)))) + xlab("") +
  # geom_text(aes(x=1895, y= 13.5, label = "B")) +
  # geom_smooth(aes(Year, TavgF),se=F, method="lm", na.rm=TRUE,linetype=1) +
  geom_line(aes(cYr, rTmean), size=1.3, , color=temp.pal[1]) +
  scale_x_continuous(breaks=c(1900, 1920, 1940, 1960, 1980, 2000, 2020)) +
  # coord_cartesian(ylim = c(min(data$TavgF)-1, max(data$TavgF)))+
  geom_smooth(method = lm,se=F, aes(cYr, tmeanP2), na.rm=TRUE,colour="black",linetype=1) +
  ##! Add aesthetics to make background white
  theme_classic() 
  # scale_color_manual(guide="none",values=temp.pal)
ggsave("NEW_Historical-temp-TS.png", width = 10, height = 6, path = LocalDir)

# brewer.pal(5, "Blues")
ppt.pal <- c("#3182BD","#6BAED6" ,"#08519C")
ggplot(yrAvgs, aes(x=cYr, y=pptAvg)) + geom_line(aes(cYr, pptAvg),color=ppt.pal[3], na.rm=TRUE,size=.2) +
  ylab("Total Precipitation (in/year)") + xlab("") +
  # geom_text(aes(x=1895, y= 13.5, label = "B")) +
  # geom_smooth(aes(Year, TavgF),se=F, method="lm", na.rm=TRUE,linetype=1) +
  geom_line(aes(cYr, rPpt),color=ppt.pal[3], size=1.3) +
  scale_x_continuous(breaks=c(1900, 1920, 1940, 1960, 1980, 2000, 2020)) +
  # coord_cartesian(ylim = c(min(data$PptIn)-5, max(data$PptIn)))+
  geom_smooth(method = lm,se=F, aes(cYr, pptP2), na.rm=TRUE,colour="black",linetype=2) +
  ##! Add aesthetics to make background white
  theme_classic() 
  # scale_color_manual(guide="none",values=ppt.pal)
ggsave("NEW_Historical-precip-TS.png", width = 10, height = 6, path = LocalDir)
