library(ggplot2)
dir("~/Google Drive/My Drive/UrbanEcoDrought/")
forest <- read.csv("~/Google Drive/My Drive/UrbanEcoDrought/Chi-NDVI-Forest_NLCD.csv")
sav <- read.csv("~/Google Drive/My Drive/UrbanEcoDrought/Chi-NDVI-Savanna_NLCD.csv")
crop <- read.csv("~/Google Drive/My Drive/UrbanEcoDrought/Chi-NDVI-Crop_NLCD.csv")
urbO <- read.csv("~/Google Drive/My Drive/UrbanEcoDrought/Chi-NDVI-UrbanOpen_NLCD.csv")
urbL <- read.csv("~/Google Drive/My Drive/UrbanEcoDrought/Chi-NDVI-UrbanLow_NLCD.csv")
urbM <- read.csv("~/Google Drive/My Drive/UrbanEcoDrought/Chi-NDVI-UrbanMedium_NLCD.csv")
urbH <- read.csv("~/Google Drive/My Drive/UrbanEcoDrought/Chi-NDVI-UrbanHigh_NLCD.csv")
summary(forest)

forest$type = "forest"
sav$type = "savanna"
crop$type = "crop"
urbO$type = "urban-open"
urbL$type = "urban-low"
urbM$type = "urban-med"
urbH$type = "urban-high"

head(forest)


ndvi <- rbind(forest, sav, crop, urbO, urbL, urbM, urbH) 
ndvi$date <- as.Date(ndvi$time)
ndvi$year <- lubridate::year(ndvi$date)
ndvi$yday <- lubridate::yday(ndvi$date)
ndvi$type <- factor(ndvi$type, levels=rev(c("forest", "savanna", "crop", "urban-open", "urban-low", "urban-med", "urban-high")))
head(ndvi)
summary(ndvi)


ggplot(data=ndvi[,], aes(x=yday, y=NDVI, group=year)) +
  facet_wrap(~type) +
  # stat_smooth(color="black", fill=NA, size=0.5) +
  geom_line(color="gray30", size=0.25)+
  # geom_line(data=ndvi[ndvi$year==2012, ], aes(color="2012"), size=1.25) +
  # geom_line(data=ndvi[ndvi$year==2005, ], aes(color="2005"), size=1.25) +
  geom_line(data=ndvi[ndvi$year==2023, ], aes(color="2023"), size=1.5) +
  # stat_smooth(data=ndvi[ndvi$year==2023, ], aes(color="2023"), size=2, fill="firebrick1", alpha=0.8) +
  scale_color_manual(values=c('2023'="firebrick2")) +
  labs(x="Day of Year")  +
  guides(fill=F, color=F) +
  theme_bw()

ggplot(data=ndvi[ndvi$yday<=180,], aes(x=yday, y=NDVI, group=year)) +
  facet_wrap(~type) +
  stat_smooth(color="black", fill=NA, size=0.5) +
  # geom_line(color="gray30", size=0.5)+
  # geom_line(data=ndvi[ndvi$year==2012, ], aes(color="2012"), size=1.25) +
  # geom_line(data=ndvi[ndvi$year==2005, ], aes(color="2005"), size=1.25) +
  # geom_line(data=ndvi[ndvi$year==2023, ], aes(color="2023"), size=1.5) +
  stat_smooth(data=ndvi[ndvi$year==2023, ], aes(color="2023"), size=2, fill="firebrick1", alpha=0.2) +
  scale_color_manual(values=c('2023'="firebrick2")) +
  labs(title="NLCD High Urban Land Coverage NDVI", x="Day of Year")  +
  guides(fill=F, color=F) +
  theme_bw()
