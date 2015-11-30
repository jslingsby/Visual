library(raster)
library(rgl)
library(rasterVis)
dem <- raster("/Users/jasper/GIT/CapePoint/Data/GIS/dem_landsat_30m.grd")

fact <- 3
cellsize <- 30

x <- as.data.frame(as.matrix(aggregate(dem,fact)))
y <- x
x[is.na(x)] <- 1

pdf("/Users/jasper/Dropbox/Fun/Visualizations/testmap1.pdf", width=10/(nrow(x)/ncol(x)), height=10)
plot(ts(as.numeric(x[1,]), end=ncol(x), frequency=ncol(x)), xlab="", ylab="", bty="n", xaxt="n", yaxt="n", ylim=c(-nrow(x)*(cellsize*fact), max(x,na.rm=T)))

for(i in seq(fact,nrow(x),fact))
{
  lines(ts(as.numeric(x[i,])-((i-1)*(cellsize*fact)), end=ncol(x), frequency=ncol(x)))
}
dev.off()

pdf("/Users/jasper/Dropbox/Fun/Visualizations/testmap2.pdf", width=10/(nrow(y)/ncol(y)), height=10)
plot(ts(as.numeric(y[1,]), end=ncol(y), frequency=ncol(y)), xlab="", ylab="", bty="n", xaxt="n", yaxt="n", ylim=c(-nrow(y)*(cellsize*fact), max(y,na.rm=T)))

for(i in seq(fact,nrow(y),fact))
{
  lines(ts(as.numeric(y[i,])-((i-1)*(cellsize*fact)), end=ncol(y), frequency=ncol(y)))
}
dev.off()

pdf("/Users/jasper/Dropbox/Fun/Visualizations/testmap3.pdf", width=10/(nrow(x)/ncol(x)), height=10)
plot(ts(as.numeric(x[1,]), end=ncol(x), frequency=ncol(x)), xlab="", ylab="", bty="n", xaxt="n", yaxt="n", ylim=c(-nrow(x)*(cellsize*fact), max(x,na.rm=T)), col="grey")

for(i in seq(fact,nrow(x),fact))
{
lines(ts(as.numeric(x[i,])-((i-1)*(cellsize*fact)), end=ncol(x), frequency=ncol(x)), col="grey")
lines(ts(as.numeric(y[i,])-((i-1)*(cellsize*fact)), end=ncol(y), frequency=ncol(y)))
}
dev.off()

#Other playing
contour(dem, levels=seq(-50,1100,40))

library(plot3D)
#hist3D(z=as.matrix(dem))

persp3d(z=x)
