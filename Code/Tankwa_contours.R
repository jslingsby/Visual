##############################################################################
######## Mapping the Tankwa
##############################################################################
######## Compiled by Jasper Slingsby 2016
######## Last edited: 27 March 2016
##############################################################################
###Get libraries
library(raster)

#Set slingsby map colours
smcol <- c("#C8C5E2", "#CDCAE4", "#D1CFE7", "#D6D4EA", "#DBD9EC", "#E0DEEF", "#E6E5F2", "#EBEAF5", "#EDECF4", "#F2F2F6", "#F8F8F4", "#F4F3EF", "#F0EFE8", "#ECEBE3", "#E7E6DD", "#E3E2D8", "#E1E0D4", "#DFDED1", "#DEDCCE", "#DBDACA", "#DAD8C7", "#D8D6C3", "#D6D4C1", "#D4D2BD", "#D1CFBA", "#D0CBB6", "#CDC7B2", "#CBC3AE", "#CBC2AC", "#CBC0AA", "#CDC1A9", "#CFC1A9", "#D1C1A8", "#D3C1A7", "#D5C1A6", "#D7C1A6", "#D9C2A5", "#DAC2A4", "#DDC2A3", "#DEC2A2", "#E0C2A1", "#DEC0A0", "#DEC09F", "#DDBF9E", "#DEBE9E", "#DEBE9C", "#DEBD9B", "#DDBB9A", "#DDBB99", "#DDB997", "#DBB997", "#DCBD98", "#DCBF98", "#DEC399", "#DCC498", "#DAC597", "#D8C696", "#D7C796", "#D4C894", "#D3C893", "#D0C992", "#CEC991", "#CCCB90", "#CACB8F", "#C8CC8F", "#C5CC8D", "#C4CE8D", "#C1CE8B", "#C0CF8B", "#BDD089", "#BAD08A", "#B8D18A", "#B5D18B", "#B3D38C", "#B0D38C", "#AED68D", "#ABD58D", "#A9D48D", "#A6D38E", "#A4D28E", "#A1D18E", "#9ED08E", "#9CD08E", "#99CF8E", "#97CE8E", "#94CD8E", "#92CD8E", "#92CD8E", "#8ECB8F", "#8ACA8F", "#87CA8F", "#85C98F", "#82C88F", "#80C78F", "#7DC78F", "#7BC68F")

###Get data and set extent
dem <- raster("/Users/jasper/Documents/GIS/ASTER30m/tankwa_March2016.tif")

#nex <- extent(19.06666667, 20.8833333, -33.4333333, -31.333333)
nex <- extent(20, 20.25, -32.25, -32)

dem <- crop(dem, nex)

NAvalue(dem) <- -32768 #Set NA values

###Smooth the DEM
demSmooth3 <- focal(dem, w=matrix(1, 3, 3), mean)
demSmooth5 <- focal(dem, w=matrix(1, 5, 5), mean)
demSmooth7 <- focal(dem, w=matrix(1, 7, 7), mean)

###Calculate hillshade
slp <- terrain(demSmooth5, "slope")
asp <- terrain(demSmooth5, "aspect")
hs <- hillShade(slp, asp)

###Draw Slingsby Map terrain colours
x <- seq(20,1940,20)
pdf("Output/Tankwa_30mASTERdem_20In_Smoothed7_SMapTerrain.pdf", width = 20, height = 20*(nrow(demSmooth5)/ncol(demSmooth5)), colormodel="cmyk")
image(dem, breaks = x, col = rev(smcol))
dev.off()

###Draw Slingsby Map terrain colours with hillshade
x <- seq(20,1940,20)
pdf("Output/Tankwa_30mASTERdem_20In_Smoothed7_hillshade_SMapTerrain.pdf", width = 20, height = 20*(nrow(demSmooth5)/ncol(demSmooth5)), colormodel="cmyk")
image(dem, breaks = x, col = rev(smcol))
plot(hs, col=grey(seq(0,1,1/60), alpha=.35), add=T, legend=F)
dev.off()

###Draw Slingsby Map terrain colours with hillshade and black contours
x <- seq(20,1940,20)
pdf("Output/Tankwa_30mASTERdem_20mContours_0.05Line_5Mp_20In_Smoothed7_hillshade_SMapTerrain.pdf", width = 20, height = 20*(nrow(demSmooth5)/ncol(demSmooth5)), colormodel="cmyk")
image(dem, breaks = x, col = rev(smcol))
plot(hs, col=grey(seq(0,1,1/60), alpha=.35), add=T, legend=F)
contour(demSmooth7, levels = x, col = c("grey30", "grey30", "grey30", "grey30", "black"), drawlabels=F, lwd=0.05, maxpixels=5000000, add=T)
dev.off()

###Draw Slingsby Map terrain colours with hillshade and unique contours
x <- seq(20,1940,20)
pdf("Output/Tankwa_30mASTERdem_20mContours_0.05Line_5Mp_20In_5col_Smoothed7_hillshade_SMapTerrain.pdf", width = 20, height = 20*(nrow(demSmooth5)/ncol(demSmooth5)), colormodel="cmyk")
image(dem, breaks = x, col = rev(smcol))
plot(hs, col=grey(seq(0,1,1/60), alpha=.35), add=T, legend=F)
contour(demSmooth7, levels = x, drawlabels=F, lwd=0.05, maxpixels=5000000, col= c("grey100", "grey75", "grey50", "grey25", "black"), add=T)
dev.off()

######

pdf("Output/Tankwa_30mASTERdem_50mContours_0.05Line_50Mp.pdf", width = 20, height = 20*(nrow(dem)/ncol(dem)))
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