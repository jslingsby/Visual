##############################################################################
######## Mapping Torres del Paine
##############################################################################
######## Compiled by Jasper Slingsby 2016
######## Last edited: 18 March 2016
##############################################################################

library(raster)
library(rasterVis)
library(RColorBrewer)
?brewer.pal

#dem <- raster("/Users/jasper/Documents/GIS/ASTER30m/TorresDelPaine_March2016.tif")

dem1 <- raster("/Users/jasper/Documents/GIS/DEM_de_Ferranti/Patagonia/M18/S50W073.hgt")
dem2 <- raster("/Users/jasper/Documents/GIS/DEM_de_Ferranti/Patagonia/M18/S51W073.hgt")
dem3 <- raster("/Users/jasper/Documents/GIS/DEM_de_Ferranti/Patagonia/M18/S52W073.hgt")
dem4 <- raster("/Users/jasper/Documents/GIS/DEM_de_Ferranti/Patagonia/M18/S50W074.hgt")
dem5 <- raster("/Users/jasper/Documents/GIS/DEM_de_Ferranti/Patagonia/M18/S51W074.hgt")
dem6 <- raster("/Users/jasper/Documents/GIS/DEM_de_Ferranti/Patagonia/M18/S52W074.hgt")

dem <- merge(dem1, dem2, dem3, dem4, dem5, dem6)

#TdP_Park <- extent(19.06666667, 20.8833333, -33.4333333, -31.333333)
TdP_Route <- extent(-73.34, -72.65, -51.15, -50.75)

dem <- crop(dem, TdP_Route)

dem30m <- disaggregate(dem, fact=3, method = "bilinear")

dem10m <- disaggregate(dem30m, fact=3, method = "bilinear")

#NAvalue(dem) <- -32768

pdf("Output/TdP_90mdeFerrantidem_50mContours_0.05Line_500Mp.pdf", width = 80, height = 80*(nrow(dem)/ncol(dem)))
contour(dem, levels = seq(0,2900,50), drawlabels=F, lwd=0.05, maxpixels=50000000)
dev.off()

pdf("Output/TdP_30from90mdeFerrantidem_50mContours_0.05Line_500Mp.pdf", width = 80, height = 80*(nrow(dem30m)/ncol(dem30m)))
contour(dem30m, levels = seq(0,2900,50), drawlabels=F, lwd=0.05, maxpixels=50000000)
dev.off()

pdf("Output/TdP_10from30from90mdeFerrantidem_50mContours_0.05Line_500Mp.pdf", width = 80, height = 80*(nrow(dem10m)/ncol(dem10m)))
contour(dem10m, levels = seq(0,2900,50), drawlabels=F, lwd=0.05, maxpixels=50000000)
dev.off()

pdf("Output/TdP_90mdeFerrantidem_20mContours_0.05Line_500Mp.pdf", width = 80, height = 80*(nrow(dem)/ncol(dem)))
contour(dem, levels = seq(0,2900,20), drawlabels=F, lwd=0.05, maxpixels=50000000)
dev.off()


slp <- terrain(dem10m, "slope")

asp <- terrain(dem10m, "aspect")

hs <- hillShade(slp, asp)

pdf("Output/TdP_10from90mdeFerrantidem_Terrain_Hillshade.pdf", width = 80, height = 80*(nrow(dem10m)/ncol(dem10m)))
plot(dem10m, legend=F, interpolate=T, col=terrain.colors(10000))
plot(hs, col=grey(seq(0,1,1/60), alpha=.35), add=T)
#contour(dem, levels = seq(0,1200,20), drawlabels=F, add=T, lwd=0.05, maxpixels=500000000)
dev.off()

#jpeg("Output/TM_10mDEM_20mContours_0.05Line_500Mp_hillshade.jpg", width = 20, height = 20*(nrow(dem)/ncol(dem)), units = "in", pointsize = 1, quality = 100, res=600)
#plot(dem, legend=F, interpolate=F)
#plot(hs, col=grey(seq(0,1,1/60), alpha=.35), add=T)
#contour(dem, levels = seq(0,1200,20), drawlabels=F, add=T, lwd=0.1, maxpixels=500000000)
#dev.off()


###Doing terrain with ggplot2
gplot(dem, maxpixels= 500000) + geom_tile(aes(fill = value)) +
  theme_bw() + coord_equal() + 
scale_fill_gradientn(colours=terrain.colors(30))



the_plot = ggplot(df) + 
  geom_raster(aes(x, y, fill=layer)) +
  
print(the_plot)


#convert the raster to points for plotting
map.p <- rasterToPoints(dem30m)

#Make the points a dataframe for ggplot
df <- data.frame(map.p)
#Make appropriate column headings
colnames(df) <- c("Longitude", "Latitude", "Elevation")

#Now make the map
ggplot(data=df, aes(y=Latitude, x=Longitude)) +
  geom_raster(aes(fill=Elevation, col=terrain.colors(10000))) +
  theme_bw() +
  coord_equal() 
+ scale_fill_gradient(“MAP (mm/yr)”, limits=c(0,2500)) +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16, angle=90),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "right",
        legend.key = element_blank()
  )