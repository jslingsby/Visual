##############################################################################
######## Mapping the Tankwa - a test of contour rendering and hillshading
##############################################################################
######## Compiled by Jasper Slingsby 2016
######## Last edited: 17 March 2016
##############################################################################

library(raster)

dem <- raster("/Users/jasper/Documents/GIS/ASTER30m/tankwa_March2016.tif")

nex <- extent(19.06666667, 20.8833333, -33.4333333, -31.333333)

dem <- crop(dem, nex)

NAvalue(dem) <- -32768

#Basic 20m contours
pdf("Output/Tankwa_30mASTERdem_20mContours_0.05Line_500Mp.pdf", width = 80, height = 80*(nrow(dem)/ncol(dem)))
contour(dem, levels = seq(0,2300,20), drawlabels=F, lwd=0.05, maxpixels=50000000)
dev.off()

# darkslategrey 20m contours with thicker black 100m
x <- seq(20,2300,20)
x<- x[-which(x %in% seq(100,2300,100))]
pdf("Output/Tankwa_30mASTERdem_20mContours_0.05Line_500Mp.pdf", width = 80, height = 80*(nrow(dem)/ncol(dem)))
contour(dem, levels = x, drawlabels=F, lwd=0.05, maxpixels=50000000, col="darkslategray")
contour(dem, levels = seq(100,2300,100), drawlabels=F, lwd=0.1, maxpixels=50000000)
dev.off()

pdf("Output/Tankwa_30mASTERdem_50mContours_0.05Line_500Mp.pdf", width = 20, height = 20*(nrow(dem)/ncol(dem)))
contour(dem, levels = seq(0,2300,50), drawlabels=F, lwd=0.05, maxpixels=50000000)
dev.off()

slp <- terrain(dem, "slope")

asp <- terrain(dem, "aspect")

hs <- hillShade(slp, asp)

pdf("Output/Tankwa_30mASTERdem_Terrain_Hillshade.pdf", width = 80, height = 80*(nrow(dem)/ncol(dem)))
plot(dem, legend=F, interpolate=T, col=terrain.colors(10000))
plot(hs, col=grey(seq(0,1,1/60), alpha=.35), add=T)
#contour(dem, levels = seq(0,1200,20), drawlabels=F, add=T, lwd=0.05, maxpixels=500000000)
dev.off()

jpeg("Output/TM_10mDEM_20mContours_0.05Line_500Mp_hillshade.jpg", width = 20, height = 20*(nrow(dem)/ncol(dem)), units = "in", pointsize = 1, quality = 100, res=600)
plot(dem, legend=F, interpolate=F)
plot(hs, col=grey(seq(0,1,1/60), alpha=.35), add=T)
contour(dem, levels = seq(0,1200,20), drawlabels=F, add=T, lwd=0.1, maxpixels=500000000)
dev.off()