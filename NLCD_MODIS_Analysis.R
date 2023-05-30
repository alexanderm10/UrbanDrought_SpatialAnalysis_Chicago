library(ggplot2)
path.google <- "~/Library/CloudStorage/GoogleDrive-breidy@mortonarb.org/Shared drives/Urban Ecological Drought/Neighborhood remote sensing analysis/Data"
forndvi<- read.csv(file.path(path.google, "Chi-NDVI-Forest-NLCD.csv"))
crpndvi<- read.csv(file.path(path.google, "Chi-NDVI-Crop-NLCD.csv"))
savndvi<- read.csv(file.path(path.google, "Chi-NDVI-Savanna-NLCD.csv"))
ourbndvi<- read.csv(file.path(path.google, "Chi-NDVI--Open-Urban-NLCD.csv"))
lurbndvi<- read.csv(file.path(path.google, "Chi-NDVI--Low-Urban-NLCD.csv"))
murbndvi<- read.csv(file.path(path.google, "Chi-NDVI--Medium-Urban-NLCD.csv"))
hurbndvi<- read.csv(file.path(path.google, "Chi-NDVI--High-Urban-NLCD.csv"))

### Forest NDVI to test

forndvi$year <- lubridate::year(as.Date(forndvi$time))
forndvi$yday <- lubridate::yday(as.Date(forndvi$time))
head(forndvi)

ggplot(data=forndvi, aes(x=yday, y=NDVI, group=year))+
geom_line()+
geom_line(data=forndvi[forndvi$year==2012, ], aes(color="2012"),size=2) +
geom_line(data=forndvi[forndvi$year==2005, ], aes(color="2005"),size=2) +
scale_color_manual(values=c("2012"='goldenrod', '2005'="red3")) +
labs(title="NLCD Forest Land Coverage NDVI", x="Day of Year")  

#dev.off()
  
## Open urban NDVI 
ourbndvi$year <- lubridate::year(as.Date(ourbndvi$time))
ourbndvi$yday <- lubridate::yday(as.Date(ourbndvi$time))
head(ourbndvi)

ggplot(data=ourbndvi, aes(x=yday, y=NDVI, group=year))+
  geom_line()+
  geom_line(data=ourbndvi[ourbndvi$year==2012, ], aes(color="2012"),size=2) +
  geom_line(data=ourbndvi[ourbndvi$year==2005, ], aes(color="2005"),size=2) +
  scale_color_manual(values=c("2012"='goldenrod', '2005'="red3")) +
  labs(title="NLCD Open Urban Land Coverage NDVI", x="Day of Year")  

#dev.off()

## Low urban NDVI 
lurbndvi$year <- lubridate::year(as.Date(lurbndvi$time))
lurbndvi$yday <- lubridate::yday(as.Date(lurbndvi$time))
head(lurbndvi)

ggplot(data=lurbndvi, aes(x=yday, y=NDVI, group=year))+
  geom_line()+
  geom_line(data=lurbndvi[lurbndvi$year==2012, ], aes(color="2012"),size=2) +
  geom_line(data=lurbndvi[lurbndvi$year==2005, ], aes(color="2005"),size=2) +
  scale_color_manual(values=c("2012"='goldenrod', '2005'="red3")) +
  labs(title="NLCD Low Urban Land Coverage NDVI", x="Day of Year")  

#dev.off()
##Medium Urban NDVI
murbndvi$year <- lubridate::year(as.Date(murbndvi$time))
murbndvi$yday <- lubridate::yday(as.Date(murbndvi$time))
head(murbndvi)

ggplot(data=murbndvi, aes(x=yday, y=NDVI, group=year))+
  geom_line()+
  geom_line(data=murbndvi[murbndvi$year==2012, ], aes(color="2012"),size=2) +
  geom_line(data=murbndvi[murbndvi$year==2005, ], aes(color="2005"),size=2) +
  scale_color_manual(values=c("2012"='goldenrod', '2005'="red3")) +
  labs(title="NLCD Medium Urban Land Coverage NDVI", x="Day of Year") 
#dev.off()

##High Urban NDVI
hurbndvi$year <- lubridate::year(as.Date(hurbndvi$time))
hurbndvi$yday <- lubridate::yday(as.Date(hurbndvi$time))
head(hurbndvi)

ggplot(data=hurbndvi, aes(x=yday, y=NDVI, group=year))+
  geom_line()+
  geom_line(data=hurbndvi[hurbndvi$year==2012, ], aes(color="2012"),size=2) +
  geom_line(data=hurbndvi[hurbndvi$year==2005, ], aes(color="2005"),size=2) +
  scale_color_manual(values=c("2012"='goldenrod', '2005'="red3")) +
  labs(title="NLCD High Urban Land Coverage NDVI", x="Day of Year") 
#dev.off()

###doing a facet of all urban land classes
##adding a class name column 
ourbndvi$name <- "Open Urban"
lurbndvi$name <- "Low Urban"
murbndvi$name <- "Medium Urban"
hurbndvi$name <- "High Urban"

#binding
urb.all <- rbind(ourbndvi,lurbndvi,murbndvi,hurbndvi)
summary(urb.all)
View(urb.all)

ggplot(data=urb.all)+
  facet_wrap(~ name) +
  aes(x=yday, y=NDVI, group=year) +
  geom_line()+
  geom_line(data=urb.all[urb.all$year==2012, ], aes(color="2012"),size=1.5) +
  geom_line(data=urb.all[urb.all$year==2005, ], aes(color="2005"),size=1.5) +
  geom_line(data=urb.all[urb.all$year==2021, ], aes(color="2021"),size=1.5) +
  scale_color_manual(values=c("2012"='goldenrod', '2005'="red3", '2021'="lightblue")) +
  labs(title="NLCD Urban Land Coverage NDVI", x="Day of Year") 
#dev.off()