# loading the required packages
library(readr)
library(ggplot2)
library(ggmap)

# creating a sample data.frame with your lat/lon points
dataset <- read_delim("coordinates.csv", ";", escape_double = FALSE, trim_ws = TRUE)

# getting the map
mapbrasil <- get_map(location = c(lon = mean(dataset$LONG), lat = mean(dataset$LAT)), zoom = 5,
                      maptype = "satellite", scale = 2)

mapbrasil <- get_map(location = c(lon = mean(dataset$LONG), lat = mean(dataset$LAT)), zoom = 5,
                     source = "stamen", maptype = "watercolor", scale = 2)
mapbrasil <- get_map(location = c(lon = mean(dataset$LONG), lat = mean(dataset$LAT)), zoom = 5,
                     source = "osm", scale = 2)
ggmap(mapbrasil)


# plotting the map with some points on it
ggmap(mapbrasil) +
  geom_point(data = dataset, aes(x = LONG, y = LAT, fill = "red"), size = 2, shape = 21) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE) 


#Including shape file - Atlantic forest polygon
#install.packages("rgdal", type = "source")
library(rgdal)

shpData <- readOGR(dsn=path.expand("~/Documents/TROPIMUNDO/MASTER THESIS/MS_Elena_master/code/mapa/limites_MA"), layer="limite_ma_wwf_200ecprregions_albers_sad69")
proj4string(shpData) # describes data’s current coordinate reference system
# to change to correct projection:
shpData <- spTransform(shpData,CRS("+proj=longlat +datum=WGS84")) 

ggmap(mapbrasil) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE) + 
  geom_polygon(aes(x = long, y = lat, group=group), data = shpData, color ="black", fill ="green", alpha = .4, size = .3)+
  geom_point(data = dataset, aes(x = LONG, y = LAT, fill = "red"), size = 2, shape = 21)












#Importing raster map from Natural Earth
library(raster)
natural_map <- raster("NE1_LR_LC_SR_W/NE1_LR_LC_SR_W.tif")
plot(natural_map)
cropbox1 <- drawExtent()
cropbox1
e<- extent (-61.63, -28.40, -34.10, -2.90)
crop_map <- crop(natural_map, e)

#plot normally
plot(crop_map, axes=T, xlab="longitude", ylab="latitude", main="Study areas")+
  plot(shpData, add = TRUE, col = alpha("darkseagreen3", 0.7), border = "dim gray", lwd = 1)+
  points(x=dataset$LONG, y = dataset$LAT, col = "black", pch = 21, bg="red", cex=1.2)

#plot it with ggmap - problem is that layers don't overlay goodly
ggmap(mapbrasil) + inset_raster(as.raster(crop_map), xmin =-62.8, xmax =-27, ymin =-38.8, ymax =-1.92) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE) + 
  geom_polygon(aes(x = long, y = lat, group=group), data = shpData, color ="black", fill ="green", alpha = .4, size = .3)+
  geom_point(data = dataset, aes(x = LONG, y = LAT, fill = "red"), size = 2, shape = 21)
#There is another function called geom_raster(fill="your raster data") but can't manage


#Read raster as STACK because it has 3 bands (RGB) - FINAL ONE!!!!!!!!!!!!!!!!!!!!!
natural_map2 <- stack("NE1_LR_LC_SR_W/NE1_LR_LC_SR_W.tif")
e<- extent (-61.63, -28.40, -34.10, -2.90)
crop_natural <- crop(natural_map2, e)

plotRGB(crop_natural, axes=T, xlab="longitude", ylab="latitude", main="Study areas") +
  plot(shpData, add = TRUE, col = alpha("darkseagreen3", 0.7), border = "dim gray", lwd = 1)+
  points(x=dataset$LONG, y = dataset$LAT, col = "black", pch = 21, bg="red", cex=1.2)
#problem with the axes





#trying to add grid lines - unsuccessful, gives value 1 for all coordinates
grd <- gridlines(crop_natural)
plotRGB(crop_natural) +
  plot(shpData, add = TRUE, col = alpha("darkseagreen3", 0.7), border = "dim gray", lwd = 1)+
  points(x=dataset$LONG, y = dataset$LAT, col = "black", pch = 21, bg="red", cex=1.2)+
  plot(grd, add = TRUE, col = grey(.8))+
  text(labels(grd), col = grey(.7), offset=1.5)


#Trying to correct the axis

par(oma=c(4,1,1,1))
plotRGB(crop_natural, main="Study areas") +
  plot(shpData, add = TRUE, col = alpha("olivedrab2", 0.7), border = "dim gray", lwd = 1)+
  points(x=dataset$LONG, y = dataset$LAT, col = "black", pch = 21, bg="orangered1", cex=1.4, lwd=1.4) + 
  plot(grd, add = TRUE, col = grey(.7))+axis(2, pos=-61.63)+ 
  axis(1, pos=-34.10)+
  text(-66, -17, "latitude", srt=90)+ 
  mtext(side=1, "longitude", line=2, outer=T)

#BLACK AND WHITE new import raster map from Natural Earth
library(raster)
BW_map <- stack("GRAY_HR_SR_W/GRAY_HR_SR_W.tif")  
e<- extent (-59.63, -33.40, -32.10, -9)
crop_BW <- crop(BW_map, e)

plotRGB(crop_BW, main="Study areas") +
  plot(shpData, add = TRUE, col = alpha("black", 0.5), border = "dim gray", lwd = 1)+
  points(x=dataset$LONG, y = dataset$LAT, col = "black", pch = 21, bg="#FFCC66", cex=1.0, lwd=1.0) + 
  axis(2, pos=-59.63)+ 
  axis(1, pos=-32.10, at=c(-60, -55, -50, -45, -40, -35, -30))+
  text(-62, -22, "latitude", srt=90)+ 
  mtext(side=1, "longitude", line=2, outer=T)

       