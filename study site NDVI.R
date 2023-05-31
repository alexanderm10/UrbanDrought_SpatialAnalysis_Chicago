library(ggplot2)
path.google <- "~/Library/CloudStorage/GoogleDrive-breidy@mortonarb.org/My Drive/"
sitendvi<- read.csv(file.path(path.google, "meanNDVISites.csv"))
head(sitendvi)

ggplot(data=sitendvi)+
  facet_wrap(~ site) +
  aes(x=month, y=meanNDVI, group=year) +
  geom_line()+
  geom_line(data=sitendvi[sitendvi$year==2012, ], aes(color="2012"),size=1.0) +
  geom_line(data=sitendvi[sitendvi$year==2005, ], aes(color="2005"),size=1.0) +
  geom_line(data=sitendvi[sitendvi$year==2021, ], aes(color="2021"),size=1.0) +
  scale_color_manual(values=c("2012"='goldenrod', '2005'="red3", '2021'="lightblue")) +
  labs(title="Urban Ecological drought study site NDVI", x="Month") 
#dev.off()

