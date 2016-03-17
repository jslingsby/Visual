library(raster)

dem <- raster("/Users/jasper/Documents/GIS/CapePeninsula/DEM10m_LO19/cct_10m")

nex <- extent(-64029.64, -45589.64, -3803538, -3749978)

dem <- crop(dem, nex)

slp <- terrain(dem, "slope")

asp <- terrain(dem, "aspect")

hs <- hillShade(slp, asp)

pdf("/Users/jasper/Desktop/TM_10mDEM_20mContours_0.05Line_500Mp_hillshade.pdf", width = 80, height = 80*(nrow(dem)/ncol(dem)))
plot(dem, legend=F, interpolate=F)
plot(hs, col=grey(seq(0,1,1/60), alpha=.35), add=T)
contour(dem, levels = seq(0,1200,20), drawlabels=F, add=T, lwd=0.05, maxpixels=500000000)
dev.off()

jpeg("/Users/jasper/Desktop/TM_10mDEM_20mContours_0.05Line_500Mp_hillshade.jpg", width = 20, height = 20*(nrow(dem)/ncol(dem)), units = "in", pointsize = 1, quality = 100, res=600)
plot(dem, legend=F, interpolate=F)
plot(hs, col=grey(seq(0,1,1/60), alpha=.35), add=T)
contour(dem, levels = seq(0,1200,20), drawlabels=F, add=T, lwd=0.1, maxpixels=500000000)
dev.off()