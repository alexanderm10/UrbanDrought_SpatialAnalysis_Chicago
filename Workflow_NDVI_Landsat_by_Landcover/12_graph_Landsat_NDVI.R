# Update images of landsat-based NDVI with the latest information 
# To be run ~ weekly!

# Note: Down the road this may get moved especially if we do more formal modeling of NDVI to give quantitative assessment of change in NDVI etc.

# Steps:
# 1. Read in the all existing landsat data
# 2. Make and graphs 

library(ggplot2)
# path.google <- ("~/Google Drive/Shared drives/Urban Ecological Drought/data/landsat_NDVI")
path.google <- ("~/Google Drive/My Drive/UrbanEcoDrought_TEST")

# Clunky code, but should pull the latest file
LCforest <- read.csv(file.path(path.google, dir(path.google, "Landsat8_forest")[length(dir(path.google, "Landsat8_forest"))]))
LCgrass <- read.csv(file.path(path.google, dir(path.google, "Landsat8_grassland")[length(dir(path.google, "Landsat8_grassland"))]))
LCcrop <- read.csv(file.path(path.google, dir(path.google, "Landsat8_crop")[length(dir(path.google, "Landsat8_crop"))]))
LCurbO <- read.csv(file.path(path.google, dir(path.google, "Landsat8_urban-open")[length(dir(path.google, "Landsat8_urban-open"))]))
LCurbL <- read.csv(file.path(path.google, dir(path.google, "Landsat8_urban-low")[length(dir(path.google, "Landsat8_urban-low"))]))
LCurbM <- read.csv(file.path(path.google, dir(path.google, "Landsat8_urban-med")[length(dir(path.google, "Landsat8_urban-med"))]))
LCurbH <- read.csv(file.path(path.google, dir(path.google, "Landsat8_urban-high")[length(dir(path.google, "Landsat8_urban-high"))]))
summary(forest)

LCforest$type = "forest"
LCgrass$type = "grassland"
LCcrop$type = "crop"
LCurbO$type = "urban-open"
LCurbL$type = "urban-low"
LCurbM$type = "urban-med"
LCurbH$type = "urban-high"

head(forest)


ndvi <- rbind(forest, grass, crop, urbO, urbL, urbM, urbH) 
ndvi$date <- as.Date(ndvi$date)
ndvi$year <- lubridate::year(ndvi$date)
ndvi$yday <- lubridate::yday(ndvi$date)
ndvi$type <- factor(ndvi$type, levels=rev(c("forest", "grassland", "crop", "urban-open", "urban-low", "urban-med", "urban-high")))
head(ndvi)
summary(ndvi)

png("~/Google Drive/Shared drives/Urban Ecological Drought/Neighborhood remote sensing analysis/NDVI_Landsat_NLCD_latest.png", height=8, width=11, units="in", res=320)
ggplot(data=ndvi[,], aes(x=yday, y=NDVI)) +
  ggtitle(paste0("Landsat 8 NDVI, last image: ", max(ndvi$date))) +
  facet_wrap(~type) +
  # stat_smooth(color="black", fill=NA, size=0.5) +
  geom_line(aes(group=year), color="gray30", size=0.1)+
  # geom_line(data=ndvi[ndvi$year==2005, ], aes(color="2005"), size=0.25) +
  # geom_line(data=ndvi[ndvi$year==2012, ], aes(color="2012"), size=0.25) +
  geom_line(data=ndvi[ndvi$year==2023, ], aes(color="2023"), size=0.5) +
  stat_smooth(data=ndvi[!ndvi$year %in% c(2005, 2012, 2023), ],  aes(color="historical", fill="historical"), size=1.5, alpha=0.5, method="gam") +
  # stat_smooth(data=ndvi[ndvi$yday<=max(ndvi$yday[ndvi$year==2023]) & ndvi$year!=2023, ], aes(color="historical", fill="historical"), size=1.5, alpha=0.8) +
  # stat_smooth(data=ndvi[ndvi$year==2005, ], aes(color="2005", fill="2005"), size=1, alpha=0.2, method="gam") +
  # stat_smooth(data=ndvi[ndvi$year==2012, ], aes(color="2012", fill="2012"), size=1, alpha=0.2, method="gam") +
  stat_smooth(data=ndvi[ndvi$year==2023, ], aes(color="2023", fill="2023"), size=1.25, alpha=0.5, method="gam") +
  scale_color_manual(values=c('2023'="firebrick2", '2012'="goldenrod1", '2005'="darkorange2", 'historical'='black')) +
  scale_fill_manual(values=c('2023'="firebrick2", '2012'="goldenrod1", '2005'="darkorange2", 'historical'='black')) +
  coord_cartesian(ylim=c(0,1)) +
  labs(x="Day of Year")  +
  guides(fill=F) +
  theme_bw()
dev.off()
 
