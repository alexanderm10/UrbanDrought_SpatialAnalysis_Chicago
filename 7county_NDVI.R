# ---------------------------------
# 0. Set up some general file paths
# ---------------------------------
library(ggplot2)
library(gghighlight)


path.google <- "~/Library/CloudStorage/GoogleDrive-breidy@mortonarb.org/My Drive" # Mac



#reading the csv
dat.7c <- read.csv(file.path(path.google, "NDVI_7county_bookends.csv"))

View(dat.7c)



#Subsetting out uncecessary columns 
dat.7c <- dat.7c[ ,c("NAME","month", "mean", "year")]

head(dat.7c)

#attemtption to generte a graph
#png(file.path(path.figs,"Seven County NDVI"), height=4, width=6, units="in", res=320)
ggplot(data=dat.7c) +
 facet_grid(~NAME )+
  geom_boxplot(aes(x=month, y=mean, fill=as.factor(year)))+
  labs(title="Seven County NDVI", y="NDVI", x="month", color="year")+
  theme_classic()
dev.off()

