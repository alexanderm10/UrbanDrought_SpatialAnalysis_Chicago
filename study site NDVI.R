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

#####Doing the same for landsat
##Loading them in removing weird brackets seperating columns and constraining valeues to numeric values
#library lot
l7<- read.csv(file.path(path.google, "Library_Parking_Lot_LS.csv"))
l7$mean <- gsub("\\{|\\}", "", l7$mean)
l7 <- l7 %>%
 separate(mean, into = c("mean", "NDVI"), sep = "=", remove = FALSE)
l7 <- subset(l7, NDVI != "null")
l7$NDVI <- as.numeric(l7$NDVI)
l7 <- l7[l7$NDVI >= -1.0 & l7$NDVI <= 1.0, ]
head(l7)


#Big Grass
b7<- read.csv(file.path(path.google, "Big_grass_LS.csv"))
b7$mean <- gsub("\\{|\\}", "", b7$mean)
b7 <- b7 %>%
  separate(mean, into = c("mean", "NDVI"), sep = "=", remove = FALSE)
b7 <- subset(b7, NDVI != "null")
b7$NDVI <- as.numeric(b7$NDVI)
b7 <- b7[b7$NDVI >= -1.0 & b7$NDVI <= 1.0, ]
head(b7)

##UIC
u7<- read.csv(file.path(path.google, "UIC_Lot_5_SES_lot_LS.csv"))
u7$mean <- gsub("\\{|\\}", "", u7$mean)
u7 <- u7 %>%
  separate(mean, into = c("mean", "NDVI"), sep = "=", remove = FALSE)
u7 <- subset(u7, NDVI != "null")
u7$NDVI <- as.numeric(u7$NDVI)
u7 <- u7[u7$NDVI >= -1.0 & u7$NDVI <= 1.0, ]
head(u7)

##Thornhill
t7<- read.csv(file.path(path.google, "Thornhill_Parking_lot_LS.csv"))
t7$mean <- gsub("\\{|\\}", "", t7$mean)
t7 <- t7 %>%
  separate(mean, into = c("mean", "NDVI"), sep = "=", remove = FALSE)
t7 <- subset(t7, NDVI != "null")
t7$NDVI <- as.numeric(t7$NDVI)
t7 <- t7[t7$NDVI >= -1.0 & t7$NDVI <= 1.0, ]
head(t7)

##Research Lot
r7<- read.csv(file.path(path.google, "Research_Parking_Lot_LS.csv"))
r7$mean <- gsub("\\{|\\}", "", r7$mean)
r7 <- r7 %>%
  separate(mean, into = c("mean", "NDVI"), sep = "=", remove = FALSE)
r7 <- subset(r7, NDVI != "null")
r7$NDVI <- as.numeric(r7$NDVI)
r7 <- r7[r7$NDVI >= -1.0 & r7$NDVI <= 1.0, ]
head(r7)

##Lombard Municipal
m7<- read.csv(file.path(path.google, "Lombard_Municipal_Site_LS.csv"))
m7$mean <- gsub("\\{|\\}", "", m7$mean)
m7 <- m7 %>%
  separate(mean, into = c("mean", "NDVI"), sep = "=", remove = FALSE)
m7 <- subset(m7, NDVI != "null")
m7$NDVI <- as.numeric(m7$NDVI)
m7 <- m7[m7$NDVI >= -1.0 & m7$NDVI <= 1.0, ]
head(m7)

##Lombard Main Street
s7<- read.csv(file.path(path.google, "Lombard_Main_Street_LS.csv"))
s7$mean <- gsub("\\{|\\}", "", s7$mean)
s7 <- s7 %>%
  separate(mean, into = c("mean", "NDVI"), sep = "=", remove = FALSE)
s7 <- subset(s7, NDVI != "null")
s7$NDVI <- as.numeric(s7$NDVI)
s7 <- s7[s7$NDVI >= -1.0 & s7$NDVI <= 1.0, ]
head(s7)

##one line to bind them
dat.ls <- rbind(s7,m7,r7,l7,t7,u7,b7)

dat.ls$year <- lubridate::year(as.Date(dat.ls$date))
dat.ls$yday <- lubridate::yday(as.Date(dat.ls$date))
dat.ls <- subset(dat.ls, yday >= 60 & yday <= 335)
head(dat.ls)

#plotting
ggplot(data=dat.ls)+
  facet_wrap(site~.) +
  aes(x=yday, y=NDVI, group=year) +
  geom_line()+
  geom_line(data=dat.ls[dat.ls$year==2012, ], aes(color="2012"),size=1.0) +
  geom_line(data=dat.ls[dat.ls$year==2005, ], aes(color="2005"),size=1.0) +
  geom_line(data=dat.ls[dat.ls$year==2021, ], aes(color="2021"),size=1.0) +
  scale_color_manual(values=c("2012"='goldenrod', '2005'="red3", '2021'="lightblue")) +
  labs(title="Urban Ecological drought study site NDVI_landsat", x="Yday")
#dev.off()

##messing around with spacing the plot
ggplot(data = dat.ls) +
  facet_wrap(site ~ ., ncol = 2, as.table = TRUE,scales = "free_y") +
  aes(x = yday, y = NDVI, group = year) +
  geom_line() +
  geom_line(data = dat.ls[dat.ls$year == 2023, ], aes(color = "2023"), size = 1.5) +
  geom_line(data = dat.ls[dat.ls$year == 2021, ], aes(color = "2021"), size = 1.5) +
  scale_color_manual(values = c("2023" = "goldenrod", "2021" = "darkblue")) +
  labs(title = "Urban Ecological Drought Study Site NDVI_Landsat", x = "Yday") +
  theme(strip.text = element_text(margin = margin(0, 0, 10, 0)))


# ####Doing the same for landsat with a slightly different mask
# #Loading them in removing weird brackets seperating columns and constraining valeues to numeric values
# #library lot
# lm<- read.csv(file.path(path.google, "Library_Parking_LotTO.csv"))
# lm$mean <- gsub("\\{|\\}", "", lm$mean)
# lm <- lm %>%
#   separate(mean, into = c("mean", "NDVI"), sep = "=", remove = FALSE)
# lm <- subset(lm, NDVI != "null")
# lm$NDVI <- as.numeric(lm$NDVI)
# lm <- lm[lm$NDVI >= -1.0 & lm$NDVI <= 1.0, ]
# head(lm)
# 
# 
# #Big Grass
# bm<- read.csv(file.path(path.google, "Big_grassTO.csv"))
# bm$mean <- gsub("\\{|\\}", "", bm$mean)
# bm <- bm %>%
#   separate(mean, into = c("mean", "NDVI"), sep = "=", remove = FALSE)
# bm <- subset(bm, NDVI != "null")
# bm$NDVI <- as.numeric(bm$NDVI)
# bm <- bm[bm$NDVI >= -1.0 & bm$NDVI <= 1.0, ]
# head(bm)
# 
# ##UIC
# um<- read.csv(file.path(path.google, "UIC_Lot_5_SES_lotTO.csv"))
# um$mean <- gsub("\\{|\\}", "", um$mean)
# um <- um %>%
#   separate(mean, into = c("mean", "NDVI"), sep = "=", remove = FALSE)
# um <- subset(um, NDVI != "null")
# um$NDVI <- as.numeric(um$NDVI)
# um <- um[um$NDVI >= -1.0 & um$NDVI <= 1.0, ]
# head(um)
# 
# ##Thornhill
# tm<- read.csv(file.path(path.google, "ThornhillParking_lotTO.csv"))
# tm$mean <- gsub("\\{|\\}", "", tm$mean)
# tm <- tm %>%
#   separate(mean, into = c("mean", "NDVI"), sep = "=", remove = FALSE)
# tm <- subset(tm, NDVI != "null")
# tm$NDVI <- as.numeric(tm$NDVI)
# tm <- tm[tm$NDVI >= -1.0 & tm$NDVI <= 1.0, ]
# head(tm)
# 
# ##Research Lot
# rm<- read.csv(file.path(path.google, "Research_Parking_LotTo.csv"))
# rm$mean <- gsub("\\{|\\}", "", rm$mean)
# rm <- rm %>%
#   separate(mean, into = c("mean", "NDVI"), sep = "=", remove = FALSE)
# rm <- subset(rm, NDVI != "null")
# rm$NDVI <- as.numeric(rm$NDVI)
# rm <- rm[rm$NDVI >= -1.0 & rm$NDVI <= 1.0, ]
# head(rm)
# 
# ##Lombard Municipal
# mm<- read.csv(file.path(path.google, "Lombard_Municipal_SiteTo.csv"))
# mm$mean <- gsub("\\{|\\}", "", mm$mean)
# mm <- mm %>%
#   separate(mean, into = c("mean", "NDVI"), sep = "=", remove = FALSE)
# mm <- subset(mm, NDVI != "null")
# mm$NDVI <- as.numeric(mm$NDVI)
# mm <- mm[mm$NDVI >= -1.0 & mm$NDVI <= 1.0, ]
# head(mm)
# 
# ##Lombard Main Street
# sm<- read.csv(file.path(path.google, "Lombard_Main_StreetTO.csv"))
# sm$mean <- gsub("\\{|\\}", "", sm$mean)
# sm <- sm %>%
#   separate(mean, into = c("mean", "NDVI"), sep = "=", remove = FALSE)
# sm <- subset(sm, NDVI != "null")
# sm$NDVI <- as.numeric(sm$NDVI)
# sm <- sm[sm$NDVI >= -1.0 & sm$NDVI <= 1.0, ]
# head(sm)
# 
# ##one line to bind them
# dat.lm <- rbind(sm,mm,rm,lm,tm,um,bm)
# 
# dat.lm$year <- lubridate::year(as.Date(dat.lm$date))
# dat.lm$yday <- lubridate::yday(as.Date(dat.lm$date))
# dat.lm <- subset(dat.lm, yday >= 60 & yday <= 335)
# head(dat.lm)
# 
# #plotting
# ggplot(data=dat.lm)+
#   facet_wrap(site~.) +
#   aes(x=yday, y=NDVI, group=year) +
#   geom_line()+
#   geom_line(data=dat.lm[dat.lm$year==2012, ], aes(color="2012"),size=1.0) +
#   geom_line(data=dat.lm[dat.lm$year==2005, ], aes(color="2005"),size=1.0) +
#   geom_line(data=dat.lm[dat.lm$year==2021, ], aes(color="2021"),size=1.0) +
#   scale_color_manual(values=c("2012"='goldenrod', '2005'="red3", '2021'="lightblue")) +
#   labs(title="Urban Ecological drought study site NDVI_landsat", x="Yday")
# #dev.off()
# 
# ##messing around with spacing the plot
# ggplot(data = dat.lm) +
#   facet_wrap(site ~ ., ncol = 2, as.table = TRUE,scales = "free_y") +
#   aes(x = yday, y = NDVI, group = year) +
#   geom_line() +
#   geom_line(data = dat.lm[dat.lm$year == 2023, ], aes(color = "2023"), size = 1.0) +
#   geom_line(data = dat.lm[dat.lm$year == 2021, ], aes(color = "2021"), size = 1.0) +
#   scale_color_manual(values = c("2023" = "goldenrod", "2021" = "lightblue")) +
#   labs(title = "Urban Ecological Drought Study Site NDVI_Landsat", x = "Yday") +
#   theme(strip.text = element_text(margin = margin(0, 0, 10, 0)))

##trying with landsat 7&8 together 
#Loading them in removing weird brackets seperating columns and constraining valeues to numeric values
#library lot
l15<- read.csv(file.path(path.google, "Library_Parking_Lot5.csv"))
l15$mean <- gsub("\\{|\\}", "", l15$mean)
l15 <- l15 %>%
  separate(mean, into = c("mean", "NDVI"), sep = "=", remove = FALSE)
l15 <- subset(l15, NDVI != "null")
l15$NDVI <- as.numeric(l15$NDVI)
l15 <- l15[l15$NDVI >= -1.0 & l15$NDVI <= 1.0, ]
head(l15)
view(l15)

#Big Grass
b15<- read.csv(file.path(path.google, "Big_grass5.csv"))
b15$mean <- gsub("\\{|\\}", "", b15$mean)
b15 <- b15 %>%
  separate(mean, into = c("mean", "NDVI"), sep = "=", remove = FALSE)
b15 <- subset(b15, NDVI != "null")
b15$NDVI <- as.numeric(b15$NDVI)
b15 <- b15[b15$NDVI >= -1.0 & b15$NDVI <= 1.0, ]
head(b15)

##UIC
u15<- read.csv(file.path(path.google, "UIC_Lot_5_SES_lot5.csv"))
u15$mean <- gsub("\\{|\\}", "", u15$mean)
u15 <- u15 %>%
  separate(mean, into = c("mean", "NDVI"), sep = "=", remove = FALSE)
u15 <- subset(u15, NDVI != "null")
u15$NDVI <- as.numeric(u15$NDVI)
u15 <- u15[u15$NDVI >= -1.0 & u15$NDVI <= 1.0, ]
head(u15)

##Thornhill
t15<- read.csv(file.path(path.google, "Thornhill_Parking_lot5.csv"))
t15$mean <- gsub("\\{|\\}", "", t15$mean)
t15 <- t15 %>%
  separate(mean, into = c("mean", "NDVI"), sep = "=", remove = FALSE)
t15 <- subset(t15, NDVI != "null")
t15$NDVI <- as.numeric(t15$NDVI)
t15 <- t15[t15$NDVI >= -1.0 & t15$NDVI <= 1.0, ]
head(t15)

##Research Lot
r15<- read.csv(file.path(path.google, "Research_Parking_Lot5.csv"))
r15$mean <- gsub("\\{|\\}", "", r15$mean)
r15 <- r15 %>%
  separate(mean, into = c("mean", "NDVI"), sep = "=", remove = FALSE)
r15 <- subset(r15, NDVI != "null")
r15$NDVI <- as.numeric(r15$NDVI)
r15 <- r15[r15$NDVI >= -1.0 & r15$NDVI <= 1.0, ]
head(r15)

##Lombard Municipal
m15<- read.csv(file.path(path.google, "Lombard_Municipal_Site5.csv"))
m15$mean <- gsub("\\{|\\}", "", m15$mean)
m15 <- m15 %>%
  separate(mean, into = c("mean", "NDVI"), sep = "=", remove = FALSE)
m15 <- subset(m15, NDVI != "null")
m15$NDVI <- as.numeric(m15$NDVI)
m15 <- m15[m15$NDVI >= -1.0 & m15$NDVI <= 1.0, ]
head(m15)

##Lombard Main Street
s15<- read.csv(file.path(path.google, "Lombard_Main_Street5.csv"))
s15$mean <- gsub("\\{|\\}", "", s15$mean)
s15 <- s15 %>%
  separate(mean, into = c("mean", "NDVI"), sep = "=", remove = FALSE)
s15 <- subset(s15, NDVI != "null")
s15$NDVI <- as.numeric(s15$NDVI)
s15 <- s15[s15$NDVI >= -1.0 & s15$NDVI <= 1.0, ]
head(s15)

##one line to bind them
dat.l15 <- rbind(s15,m15,r15,l15,t15,u15,b15)

dat.l15$year <- lubridate::year(as.Date(dat.l15$date))
dat.l15$yday <- lubridate::yday(as.Date(dat.l15$date))
dat.l15 <- subset(dat.l15, yday >= 60 & yday <= 335)
head(dat.l15)

#plotting
ggplot(data=dat.l15)+
  facet_wrap(site~.) +
  aes(x=yday, y=NDVI, group=year) +
  geom_line()+
  geom_line(data=dat.l15[dat.l15$year==2012, ], aes(color="2012"),size=1.0) +
  geom_line(data=dat.l15[dat.l15$year==2005, ], aes(color="2005"),size=1.0) +
  geom_line(data=dat.l15[dat.l15$year==2021, ], aes(color="2021"),size=1.0) +
  scale_color_manual(values=c("2012"='goldenrod', '2005'="red3", '2021'="lightblue")) +
  labs(title="Urban Ecological drought study site NDVI_landsat", x="Yday")
#dev.off()

##messing around with spacing the plot
ggplot(data = dat.l15) +
  facet_wrap(site ~ ., ncol = 2, as.table = TRUE,scales = "free_y") +
  aes(x = yday, y = NDVI, group = year) +
  geom_line() +
  geom_line(data = dat.l15[dat.l15$year == 2023, ], aes(color = "2023"), size = 1.0) +
  geom_line(data = dat.l15[dat.l15$year == 2021, ], aes(color = "2021"), size = 1.0) +
  scale_color_manual(values = c("2023" = "goldenrod", "2021" = "lightblue")) +
  labs(title = "Urban Ecological Drought Study Site NDVI_Landsat", x = "Yday") +
  theme(strip.text = element_text(margin = margin(0, 0, 10, 0)))

### Loading in csv's of all of the morton arboretum and all of UIC in order to compare NDVI 
# Via remote sensing products

##Comparison of MODIS and Landsat at the Morton arb. 
Amod<- read.csv(file.path(path.google, "Arb_NDVI_mod.csv"))
head(Amod)
dat.am <- Amod[, c("date", "mean_ndvi")]
# adding a column for the method of data collection
dat.am$method <-"Modis"
# changing the NDVI column name to "NDVI" so columns and match and merge when data sets are bound
colnames(dat.am)[colnames(dat.am) == "mean_ndvi"] <- "NDVI"
head(dat.am)

Als<- read.csv(file.path(path.google, "Arb_NDVI_L8.csv"))
head(Als)
dat.al <- Als[, c("date", "meanNDVI")]
dat.al$method <-"Landsat"
colnames(dat.al)[colnames(dat.al) == "meanNDVI"] <- "NDVI"
head(dat.al)

dat.arb <- rbind(dat.al, dat.am)
dat.arb <- na.omit(dat.arb)
dat.arb$year <- lubridate::year(as.Date(dat.arb$date))
dat.arb$yday <- lubridate::yday(as.Date(dat.arb$date))

ggplot(data=dat.arb)+
  facet_grid(method~.) +
  aes(x=yday, y=NDVI, color=as.factor(year)) +
  geom_line()+
  labs(title="Arboretum NDVI by remote sensing product", x="Day of year")+
  scale_color_discrete(name = "Year") 
#dev.off()


##Comparison of MODIS and Landsat at UIC. 
Umod<- read.csv(file.path(path.google, "Uic_NDVI_mod.csv"))
head(Umod)
dat.um <- Umod[, c("date", "mean_ndvi")]
# adding a column for the method of data collection
dat.um$method <-"Modis"
# changing the NDVI column name to "NDVI" so columns and match and merge when data sets are bound
colnames(dat.um)[colnames(dat.um) == "mean_ndvi"] <- "NDVI"
head(dat.um)

Uls<- read.csv(file.path(path.google, "UIC_NDVI_L8.csv"))
head(Uls)
dat.ul <- Uls[, c("date", "meanNDVI")]
dat.ul$method <-"Landsat"
colnames(dat.ul)[colnames(dat.ul) == "meanNDVI"] <- "NDVI"
head(dat.ul)

dat.uic <- rbind(dat.ul, dat.um)
dat.uic <- na.omit(dat.uic)
dat.uic$year <- lubridate::year(as.Date(dat.uic$date))
dat.uic$yday <- lubridate::yday(as.Date(dat.uic$date))
head(dat.uic)

ggplot(data=dat.uic)+
  facet_grid(method~.) +
  aes(x=yday, y=NDVI, color=as.factor(year)) +
  geom_line()+
  labs(title="UIC NDVI by remote sensing product", x="Year")+
  scale_color_discrete(name = "Day of year ") 
#dev.off()