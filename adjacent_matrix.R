
############################################################
'https://stackoverflow.com/questions/26499010/finding-adjacent-polygons-in-r-neighbors'

library(rgeos);library(rgdal)
library(latticeExtra);library(sp)
library(magrittr)
coord
######################## load in data ######################
# WGS84 +towgs84
city_shp <- readOGR("C:/Users/WXY/Documents/R_learning/map/data/City/CN_city.shp",use_iconv=TRUE,encoding='UTF-8',stringsAsFactors = F)
gadm36 <- readOGR("C:/Users/WXY/Documents/R_learning/map/data/gadm36_CHN_shp/gadm36_CHN_2.shp",use_iconv=TRUE,encoding='UTF-8',stringsAsFactors = F)
dijishi_2004 <- readOGR("C:/Users/WXY/Documents/R_learning/map/data/xingzhengquhua_shp/dijishi_2004.shp",use_iconv=TRUE,encoding='UTF-8',stringsAsFactors = F)
county_2004 <- readOGR("C:/Users/WXY/Documents/R_learning/map/data/xingzhengquhua_shp/county_2004.shp",use_iconv=TRUE,encoding='UTF-8',stringsAsFactors = F)


plot(county_2004)
proj4string(diquJie_polyline)
# Define a function to  get center point
get_center <- function(x){
  coordinate <- coordinates( gCentroid (city_shp_2[city_shp_2$NAME==x,]))
  return(coordinate)
}

coordinate <- t(sapply(city_shp_2$NAME, get_center)) # coordinate of centres
unique(rownames(coordinate))
rownames(coordinate)[which(duplicated(rownames(coordinate)))]

################### find adjacent polygons *1* {sp} ###################
# Create SpatialPoints
SP <- SpatialPoints(coords = coordinate)

# Add label variable
SP$ID <- rownames(coordinate)%>%as.factor(.)

# Plot
plot <- spplot(city_shp, zcol = "NAME")
plot <- spplot(SP, zcol ="ID")
labels <- layer(sp.text(coordinates(SP), txt = SP$ID, pos = 1))

# plot + label layer
plot + labels

# adjcent matrix
adj_mat <- gTouches(city_shp, byid=TRUE)
adj_mat[adj_mat==TRUE] <- 1;adj_mat[adj_mat==FALSE] <- 0
rownames(adj_mat) <- city_shp$NAME; colnames(adj_mat) <- city_shp$NAME

################### find adjacent polygons *2* {spdep} ###################

pacman::p_load(spdep)
row.names(city_shp) <- as.character(city_shp$NAME)
nb <- poly2nb(city_shp)
mat <- nb2mat(nb, style="B")
colnames(mat) <- rownames(mat)

################# calculate area #################
library(raster)
# x <- shapefile('file.shp')
crs(ghana)
ghana$area_sqkm <- area(ghana) / 1000000

library(maptools)
plot(SP)
pointLabel(coordinates(SP),labels=SP$ID)

################# point falls in a polygon *1* #################
'https://www.r-bloggers.com/r-and-gis-working-with-shapefiles/'
library(rgeos)
p<-SpatialPoints(list(lng,lat), proj4string=crswgs84)
gContains(fdg,pt)

################# point falls in a polygon *2* #################
'https://stackoverflow.com/questions/8751497/latitude-longitude-coordinates-to-state-code-in-r/8751965#8751965'

