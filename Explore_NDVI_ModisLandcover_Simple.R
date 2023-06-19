library(ggplot2)

forest <- read.csv("~/Google Drive/My Drive/UrbanEcoDrought/Chi-NDVI-Forest-MODIS.csv")
urban <- read.csv("~/Google Drive/My Drive/UrbanEcoDrought/Chi-NDVI-Urban-MODIS.csv")
crop <- read.csv("~/Google Drive/My Drive/UrbanEcoDrought/Chi-NDVI-Crop-MODIS.csv")
sav <- read.csv("~/Google Drive/My Drive/UrbanEcoDrought/Chi-NDVI-Savanna-MODIS.csv")

forest$type = "forest"
urban$type = "urban"
crop$type = "crop"
sav$type = "savanna"

ndvi <- rbind(forest, urban, crop, sav)
ndvi$date <- as.Date(ndvi$time)
ndvi$year <- lubridate::year(ndvi$date)
ndvi$yday <- lubridate::yday(ndvi$date)
head(ndvi)

ydayLab <- data.frame(date=seq.Date(from=as.Date("2023-01-01"), to=as.Date("2023-12-31"), by="month"))
ydayLab$yday <- lubridate::yday(ydayLab$date)
ydayLab$moName <- lubridate::month(ydayLab$date, label=T)
ydayLab$dayMo <- c("01")

ggplot(data=ndvi, aes(x=date, y=NDVI, color=type)) +
  facet_wrap(~type) +
  geom_line()

png("~/Google Drive/Shared drives/Urban Ecological Drought/Neighborhood remote sensing analysis/Prelim_NDVI_by_ModisLandcover.png", height=8, width=8, units="in", res=180)
ggplot(data=ndvi, aes(x=yday, y=NDVI)) +
  facet_wrap(~type) +
  geom_line(aes(group=year), color="black", size=0.15) +
  geom_line(data=ndvi[ndvi$year %in% c(2005),], aes(color="2005", group=year), size=1.25) +
  geom_line(data=ndvi[ndvi$year %in% c(2012),], aes(color="2012", group=year), size=1.25) +
  # geom_line(data=ndvi[ndvi$year %in% c(2021),], aes(color="2021", group=year), size=1.25) +
  geom_line(data=ndvi[ndvi$year %in% c(2023),], aes(color="2023", group=year), size=2) +
  # scale_color_manual(name="year", values=c("2005"="red2", "2012"="darkorange", "2021"="goldenrod1", "2023"="dodgerblue1"))+
  scale_color_manual(name="year", values=c("2005"="red2", "2012"="darkorange", "2023"="dodgerblue2"))+
  scale_x_continuous(name="Day", breaks=ydayLab$yday[seq(1,12, by=2)], labels=paste0(ydayLab$moName[seq(1,12, by=2)], " 01")) +
  theme_bw() +
  theme(legend.position="top")
dev.off()
  # stat_smooth()


# Doing a quick & dirty gam
library(mgcv)
summary(ndvi)
gamLC <- gam(NDVI ~ s(yday, k=18, by=as.factor(type)) + as.factor(type)-1, data=ndvi)
summary(gamLC)
# plot(gamLC)



df.pred <- data.frame(yday=1:365, type=rep(c("urban", "forest", "savanna", "crop"), each=365))
# df.pred$NDVI <- 
gamPred <- predict(gamLC, newdata=df.pred, se.fit=T)
summary(gamPred)

df.pred$NDVI <- gamPred$fit
df.pred$lb <- gamPred$fit - 2*gamPred$se.fit
df.pred$ub <- gamPred$fit + 2*gamPred$se.fit


ggplot(data=ndvi, aes(x=yday, y=NDVI)) +
  facet_wrap(~type) +
  geom_line(aes(group=year)) +
  geom_ribbon(data=df.pred, aes(ymin=lb, ymax=ub, fill="average"), alpha=0.5, size=1.25) +
  geom_line(data=df.pred, aes(color="average"), size=1.25) +
  scale_color_manual(values=c("average"="green4"))+
  scale_fill_manual(values=c("average"="green4"))+
  theme_bw() +
  theme(legend.position="top")



