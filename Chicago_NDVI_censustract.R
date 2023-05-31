library(sp); library(raster)
library(ggplot2)

test <- brick("~/Downloads/chicensusNDVI-0000000768-0000001536.tif")
test

plot(test[[1]]) # Looks weird
plot(test[[2]])

chicoords <- coordinates(test)
dim(chicoords)

chiVals1 <- getValues(test[[1]])
chiVals <- data.frame(chicoords, chiVals1)
summary(chiVals)

chiVals$chiVals1[chiVals$chiVals1>1 | chiVals$chiVals1<0] <- NA
summary(chiVals)

ggplot(data=chiVals[!is.na(chiVals$chiVals1),], aes(x=x, y=y, fill=chiVals1)) +
  geom_tile()
