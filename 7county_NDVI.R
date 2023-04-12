# ---------------------------------
# 0. Set up some general file paths
# ---------------------------------
library(ggplot2)
library(gghighlight)


path.google <- "~/Library/CloudStorage/GoogleDrive-breidy@mortonarb.org/My Drive" # Mac



#reading the csv
dat.7c <- read.csv(file.path(path.google, "NDVI_7county_bookends.csv"))

head(dat.7c)



#Subsetting out uncecessary columns 
dat.7c <- dat.7c[ ,c("NAME","month", "mean", "year")]

head(dat.7c)

#attemtption to generte a graph
#png(file.path(path.figs,"Seven County NDVI"), height=4, width=6, units="in", res=320)
ggplot(data=dat.7c) +
 facet_wrap(~NAME )+
  geom_smooth(aes(x=month, y=mean, color=as.factor(year)))+
  scale_color_manual(name="year", values=c("2004"="NA", "2005"="red", "2006"="NA", "2011"="NA", "2012"="#F0E442", "2013"="NA", "2020"="NA", "2021"= "violet","2022"="NA")) +
  labs(title="Seven County NDVI", y="NDVI", x="month", color="year")+
  theme_classic()
dev.off()


ggplot(data=dat.7c) +
  facet_wrap(~NAME )+
  geom_line(aes(x=month, y=mean, color=as.factor(year)))+
  #scale_color_manual(name="year", values=c("2004"="NA", "2005"="red", "2006"="NA", "2011"="NA", "2012"="#F0E442", "2013"="NA", "2020"="NA", "2021"= "violet","2022"="NA")) +
  labs(title="Seven County NDVI", y="NDVI", x="month", color="year")+
  theme_classic()
dev.off()

ggplot(data=dat.7c) +
  facet_wrap(~NAME )+
  geom_point(alpha= 0.5,aes(x=month, y=mean, color=as.factor(year)))+
  #scale_color_manual(name="year", values=c("2004"="NA", "2005"="red", "2006"="NA", "2011"="NA", "2012"="#F0E442", "2013"="NA", "2020"="NA", "2021"= "violet","2022"="NA")) +
  labs(title="Seven County NDVI", y="NDVI", x="month", color="year")+
  theme_classic()
dev.off()



