library(ggplot2)
# dir("~/Google Drive/My Drive/UrbanEcoDrought/")
forest <- read.csv("~/Google Drive/My Drive/UrbanEcoDrought/Chi-NDVI-Forest-Landsat_5-9.csv")
sav <- read.csv("~/Google Drive/My Drive/UrbanEcoDrought/Chi-NDVI-Savanna-Landsat_5-9.csv")
crop <- read.csv("~/Google Drive/My Drive/UrbanEcoDrought/Chi-NDVI-Crop-Landsat_5-9.csv")
urbO <- read.csv("~/Google Drive/My Drive/UrbanEcoDrought/Chi-NDVI-UrbanOpen-Landsat_5-9.csv")
urbL <- read.csv("~/Google Drive/My Drive/UrbanEcoDrought/Chi-NDVI-UrbanLow-Landsat_5-9.csv")
urbM <- read.csv("~/Google Drive/My Drive/UrbanEcoDrought/Chi-NDVI-UrbanMedium-Landsat_5-9.csv")
urbH <- read.csv("~/Google Drive/My Drive/UrbanEcoDrought/Chi-NDVI-UrbanHigh-Landsat_5-9.csv")
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

png("~/Google Drive/Shared drives/Urban Ecological Drought/Neighborhood remote sensing analysis/NDVI_Landsat_NLCD_latest.png", height=8, width=11, units="in", res=320)
ggplot(data=ndvi[,], aes(x=yday, y=NDVI)) +
  ggtitle(paste0("Landsat 8 NDVI, last image: ", max(ndvi$date))) +
  facet_wrap(~type) +
  # stat_smooth(color="black", fill=NA, size=0.5) +
  geom_line(aes(group=year), color="gray30", size=0.1)+
  geom_line(data=ndvi[ndvi$year==2005, ], aes(color="2005"), size=0.25) +
  geom_line(data=ndvi[ndvi$year==2012, ], aes(color="2012"), size=0.25) +
  geom_line(data=ndvi[ndvi$year==2023, ], aes(color="2023"), size=0.5) +
  stat_smooth(data=ndvi[!ndvi$year %in% c(2005, 2012, 2023), ],  aes(color="historical", fill="historical"), size=1.5, alpha=0.5) +
  # stat_smooth(data=ndvi[ndvi$yday<=max(ndvi$yday[ndvi$year==2023]) & ndvi$year!=2023, ], aes(color="historical", fill="historical"), size=1.5, alpha=0.8) +
  stat_smooth(data=ndvi[ndvi$year==2005, ], aes(color="2005", fill="2005"), size=1, alpha=0.2, method="gam") +
  stat_smooth(data=ndvi[ndvi$year==2012, ], aes(color="2012", fill="2012"), size=1, alpha=0.2, method="gam") +
  stat_smooth(data=ndvi[ndvi$year==2023, ], aes(color="2023", fill="2023"), size=2, alpha=0.5, method="gam") +
  scale_color_manual(values=c('2023'="firebrick2", '2012'="goldenrod1", '2005'="darkorange2", 'historical'='black')) +
  scale_fill_manual(values=c('2023'="firebrick2", '2012'="goldenrod1", '2005'="darkorange2", 'historical'='black')) +
  coord_cartesian(ylim=c(0,1)) +
  labs(x="Day of Year")  +
  guides(fill=F) +
  theme_bw()
dev.off()
# ggplot(data=ndvi[ndvi$yday<=180,], aes(x=yday, y=NDVI)) +
#   facet_wrap(~type) +
#   # stat_smooth(color="black", fill=NA, size=0.5) +
#   geom_line(aes(group=year), color="gray30", size=0.5)+
#   # geom_line(data=ndvi[ndvi$year==2012, ], aes(color="2012"), size=1.25) +
#   # geom_line(data=ndvi[ndvi$year==2005, ], aes(color="2005"), size=1.25) +
#   # geom_line(data=ndvi[ndvi$year==2023, ], aes(color="2023"), size=1.5) +
#   stat_smooth(data=ndvi[ndvi$yday<=180 & ndvi$year!=2023, ], aes(color="historical", fill="historical"), size=1.5, alpha=0.8) +
#   stat_smooth(data=ndvi[ndvi$year==2023, ], aes(color="2023", fill="2023"), size=2, alpha=0.5) +
#   scale_color_manual(values=c('2023'="firebrick2", 'historical'='black')) +
#   scale_fill_manual(values=c('2023'="firebrick2", 'historical'='black')) +
#   labs(title="NLCD High Urban Land Coverage NDVI", x="Day of Year")  +
#   guides(fill=F, color=F) +
#   theme_bw()
