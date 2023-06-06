library(ggplot2)
library(tidyverse)
##setting up my path
path.google <- "~/Library/CloudStorage/GoogleDrive-breidy@mortonarb.org/My Drive/eesundry"


##reading in the csv's selecting the columns I want
#Lombard municipal
lmmu<- read.csv(file.path(path.google, "NDVI_Lombard_Municipal.csv"))
head(lmmu)
dat.lm <- lmmu[, c("system.index", "meanNDVI")]
dat.lm$name<-"Lombard Municipal"
head(dat.lm)

#Thornhill 
path.google <- "~/Library/CloudStorage/GoogleDrive-breidy@mortonarb.org/My Drive/eesundry"
thill<- read.csv(file.path(path.google, "NDVI_Thornhill.csv"))
head(thill)
dat.th <- thill[, c("system.index", "meanNDVI")]
dat.th$name <- "Thornhill Lot "
head(dat.th)

#research lot
path.google <- "~/Library/CloudStorage/GoogleDrive-breidy@mortonarb.org/My Drive/eesundry"
rslt<- read.csv(file.path(path.google, "NDVI_Research_Lot.csv"))
head(rslt)
dat.rl <- rslt[, c("system.index", "meanNDVI")]
dat.rl$name <- "Research Lot"
head(dat.rl)

#Big Grass
path.google <- "~/Library/CloudStorage/GoogleDrive-breidy@mortonarb.org/My Drive/eesundry"
bg<- read.csv(file.path(path.google, "NDVI_Big_Grass.csv"))
head(bg)
dat.bg <- bg[, c("system.index", "meanNDVI")]
dat.bg$name <- "Big Grass Lot"
head(dat.bg)

#Morotn Library 
path.google <- "~/Library/CloudStorage/GoogleDrive-breidy@mortonarb.org/My Drive/eesundry"
lb<- read.csv(file.path(path.google, "NDVI_Library.csv"))
head(lb)
dat.lb <- lb[, c("system.index", "meanNDVI")]
dat.lb$name <- "Morton Library Lot "
head(dat.lb)

#Lombard Main Street
path.google <- "~/Library/CloudStorage/GoogleDrive-breidy@mortonarb.org/My Drive/eesundry"
ms<- read.csv(file.path(path.google, "NDVI_Main_Street.csv"))
head(ms)
dat.ms <- ms[, c("system.index", "meanNDVI")]
dat.ms$name <- "Lombard Main Street"
head(dat.ms)

#UIC Lot 5
path.google <- "~/Library/CloudStorage/GoogleDrive-breidy@mortonarb.org/My Drive/eesundry"
l5<- read.csv(file.path(path.google, "NDVI_UIC Lot_5.csv"))
head(l5)
dat.l5 <- l5[, c("system.index", "meanNDVI")]
dat.l5$name <- "UIC Lot 5"
head(dat.l5)

#and in the darkness bind them 
dat.all <- rbind(dat.bg,dat.l5,dat.lb,dat.rl, dat.ms,dat.lm,dat.th)



# Rename the "meanNDVI" column to "NDVI"
colnames(dat.all)[colnames(dat.all) == "meanNDVI"] <- "NDVI"

# Change the "system.index" column to "date" and convert it to the appropriate format
dat.all$date <- as.Date(dat.all$system.index, format = "%Y_%m_%d")

dat.all$year <- lubridate::year(as.Date(dat.all$date))
dat.all$yday <- lubridate::yday(as.Date(dat.all$date))
head(dat.all)

#plotting
ggplot(data=dat.all)+
 facet_wrap(name~.) +
  aes(x=yday, y=NDVI, group=year) +
  geom_line()+
  geom_line(data=dat.all[dat.all$year==2012, ], aes(color="2012"),size=1.0) +
  geom_line(data=dat.all[dat.all$year==2005, ], aes(color="2005"),size=1.0) +
  geom_line(data=dat.all[dat.all$year==2021, ], aes(color="2021"),size=1.0) +
  scale_color_manual(values=c("2012"='goldenrod', '2005'="red3", '2021'="lightblue")) +
  labs(title="Urban Ecological drought study site NDVI", x="Yday") 
#dev.off()

##messing around with spacing the plot
ggplot(data = dat.all) +
  facet_wrap(name ~ ., ncol = 2, as.table = TRUE) +
  aes(x = yday, y = NDVI, group = year) +
  geom_line() +
  geom_line(data = dat.all[dat.all$year == 2012, ], aes(color = "2012"), size = 1.5) +
  geom_line(data = dat.all[dat.all$year == 2005, ], aes(color = "2005"), size = 1.5) +
  geom_line(data = dat.all[dat.all$year == 2021, ], aes(color = "2021"), size = 1.5) +
  scale_color_manual(values = c("2012" = "goldenrod", "2005" = "red3", "2021" = "lightblue")) +
  labs(title = "Urban Ecological Drought Study Site NDVI", x = "Yday") +
  theme(strip.text = element_text(margin = margin(0, 0, 10, 0)))

# ####Doing the same for landsat
# ls<- read.csv(file.path(path.google, "Site_NDVI_landsat.csv"))
# head(ls)
# #dev.off()
#dev.off()
#dev.off()

# head(dat.ls)
# # Separate the Date column into separate columns for date and time
# dat.ls <- dat.ls %>%
#   separate(Date, into = c("Date", "Time"), sep = "T")
# dat.ls <- dat.ls[, c("Date","NDVI", "Name")]
# head(dat.ls)
# 
# dat.ls$year <- lubridate::year(as.Date(dat.ls$Date))
# dat.ls$yday <- lubridate::yday(as.Date(dat.ls$Date))
# head(dat.ls)
# 
# #plotting
# ggplot(data=dat.ls)+
#   facet_wrap(Name~.) +
#   aes(x=yday, y=NDVI, group=year) +
#   geom_line()+
#   geom_line(data=dat.ls[dat.ls$year==2012, ], aes(color="2012"),size=1.0) +
#   geom_line(data=dat.ls[dat.ls$year==2005, ], aes(color="2005"),size=1.0) +
#   geom_line(data=dat.ls[dat.ls$year==2021, ], aes(color="2021"),size=1.0) +
#   scale_color_manual(values=c("2012"='goldenrod', '2005'="red3", '2021'="lightblue")) +
#   labs(title="Urban Ecological drought study site NDVI_landsat", x="Yday") 
# #dev.off()
# 
# ##messing around with spacing the plot
# ggplot(data = dat.ls) +
#   facet_wrap(Name ~ ., ncol = 2, as.table = TRUE) +
#   aes(x = yday, y = NDVI, group = year) +
#   geom_line() +
#   geom_line(data = dat.ls[dat.ls$year == 2012, ], aes(color = "2012"), size = 1.5) +
#   geom_line(data = dat.ls[dat.ls$year == 2005, ], aes(color = "2005"), size = 1.5) +
#   geom_line(data = dat.ls[dat.ls$year == 2021, ], aes(color = "2021"), size = 1.5) +
#   scale_color_manual(values = c("2012" = "goldenrod", "2005" = "red3", "2021" = "lightblue")) +
#   labs(title = "Urban Ecological Drought Study Site NDVI_Landsat", x = "Yday") +
#   theme(strip.text = element_text(margin = margin(0, 0, 10, 0)))

##Trying a different csv
las<- read.csv(file.path(path.google, "siteDVIlandsat.csv"))
head(las)
dat.las <- las[, c("date","meanNDVI", "siteName")]
head(dat.las)

colnames(dat.las)[colnames(dat.las) == "meanNDVI"] <- "NDVI"
colnames(dat.las)[colnames(dat.las) == "siteName"] <- "name"
dat.las$year <- lubridate::year(as.Date(dat.las$date))
dat.las$yday <- lubridate::yday(as.Date(dat.las$date))
head(dat.las)


 #plotting
ggplot(data=dat.las)+
  facet_wrap(name~.) +
  aes(x=yday, y=NDVI, group=year) +
  geom_line()+
  geom_line(data=dat.las[dat.las$year==2012, ], aes(color="2012"),size=1.0) +
  geom_line(data=dat.las[dat.las$year==2005, ], aes(color="2005"),size=1.0) +
  geom_line(data=dat.las[dat.las$year==2021, ], aes(color="2021"),size=1.0) +
  scale_color_manual(values=c("2012"='goldenrod', '2005'="red3", '2021'="lightblue")) +
  labs(title="Urban Ecological drought study site NDVI_landsat", x="Yday")
#dev.off()

##messing around with spacing the plot
ggplot(data = dat.las) +
  facet_wrap(name ~ ., ncol = 2, as.table = TRUE,scales = "free_y") +
  aes(x = yday, y = NDVI, group = year) +
  geom_line() +
  geom_line(data = dat.las[dat.las$year == 2012, ], aes(color = "2012"), size = 1.5) +
  geom_line(data = dat.las[dat.las$year == 2023, ], aes(color = "2023"), size = 1.5) +
  scale_color_manual(values = c("2023" = "red", "2021" = "lightblue")) +
  labs(title = "Urban Ecological Drought Study Site NDVI_Landsat", x = "Yday") +
  theme(strip.text = element_text(margin = margin(0, 0, 10, 0)))
