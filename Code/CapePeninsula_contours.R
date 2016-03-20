##############################################################################
######## Mapping the Cape Peninsula - a test of contour rendering and hillshading
##############################################################################
######## Compiled by Jasper Slingsby 2016
######## Last edited: 17 March 2016
##############################################################################

library(raster)

dem <- raster("/Users/jasper/Documents/GIS/CapePeninsula/DEM10m_LO19/cct_10m")

nex <- extent(-64029.64, -45589.64, -3803538, -3749978)

dem <- crop(dem, nex)

slp <- terrain(dem, "slope")

asp <- terrain(dem, "aspect")

hs <- hillShade(slp, asp)

pdf("Output/TM_10mDEM_5mContours_0.05Line_500Mp_hillshade_rev.pdf", width = 80, height = 80*(nrow(dem)/ncol(dem)))
plot(dem, legend=F, interpolate=F, col=terrain.colors(1000))
plot(hs, col=grey(seq(0,1,1/240), alpha=.35), add=T)
contour(dem, levels = seq(0,1200,5), drawlabels=F, add=T, lwd=0.05, maxpixels=500000000)
dev.off()

pdf("Output/TM_10mDEM_20mContours_0.05Line_500Mp_hillshade_rev.pdf", width = 80, height = 80*(nrow(dem)/ncol(dem)))
plot(dem, legend=F, interpolate=F, col=terrain.colors(1000))
plot(hs, col=grey(seq(0,1,1/60), alpha=.35), add=T)
contour(dem, levels = seq(0,1200,20), drawlabels=F, add=T, lwd=0.05, maxpixels=500000000)
dev.off()

jpeg("Output/TM_10mDEM_20mContours_0.05Line_500Mp_hillshade.jpg", width = 20, height = 20*(nrow(dem)/ncol(dem)), units = "in", pointsize = 1, quality = 100, res=600)
plot(dem, legend=F, interpolate=F)
plot(hs, col=grey(seq(0,1,1/60), alpha=.35), add=T)
contour(dem, levels = seq(0,1200,20), drawlabels=F, add=T, lwd=0.1, maxpixels=500000000)
dev.off()