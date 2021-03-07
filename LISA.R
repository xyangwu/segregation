library('usdm')
file <- system.file("external/spain.grd", package="usdm")

r <- brick(file) # reading a RasterBrick object including 10 rasters in Spain

r
par(mar=c(1,1,1,1))
plot(r) # visualize the raster layers

plot(r[[1]]) # visualize the first raster layer

r.I <- lisa(x=r[[1]],d1=0,d2=25000,statistic="I") # local Moran's I

plot(r.I)

# entering r instead of r[[1]], givees the indicator for each layer:
r.I <- lisa(x=r,d1=0,d2=25000,statistic="I")
plot(r.I)

r.c <- lisa(x=r[[1]],d1=0,d2=25000,statistic="c") # local Geary's c

plot(r.c)

r.g <- lisa(x=r[[1]],d1=0,d2=25000,statistic="G") # G statistic

plot(r.g)

r.g2 <- lisa(x=r[[1]],d1=0,d2=25000,statistic="G*") # G* statistic

plot(r.g2)

r.K1 <- lisa(x=r[[1]],d1=0,d2=30000,statistic="K1") # gives K1 statistic for each layer

plot(r.K1)

lisa(x=r,d1=0,d2=30000,cell=2000,statistic="I") # gives local Moran's I at cell number 2000
#for each raster layer in r

lisa(x=r,d1=0,d2=30000,cell=c(2000,2002,2003),statistic="c") # calculates local Moran's I
# at cell numbers of 2000,2002, and 2003 for each raster layer in r

sp <- sampleRandom(r[[1]],20,sp=TRUE) # draw 20 random points from r, 
# and returns a SpatialPointsDataFrame

plot(r[[1]])

points(sp)

lisa(x=r,y=sp,d1=0,d2=30000,statistic="I") # calculates the local Moran's I at 
# point locations in sp for each raster layer in r

## End(Not run)