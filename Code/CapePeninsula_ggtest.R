##############################################################################
######## Mapping the Cape Peninsula - a test with ggmap
##############################################################################
######## Compiled by Jasper Slingsby 2015
######## Last edited: 5 November 2015
##############################################################################

# check out library(tmap) and library(leaflet)

### Get libraries
library(ggmap)
library(raster)
library(rgdal)
library(dplyr)
#library(reshape2)
#library(ggplot2)
#library(scales)
#library(animation)

### Downloading the images and formatting them for plotting

# Get fire data
fire <- readOGR(dsn="/Users/jasper/GIT/CapePoint/Data/GIS/capepenfire2011.shp", layer="capepenfire2011")

fire <- spTransform(fire, CRS("+proj=longlat +datum=WGS84"))

# Prep fire data for plotting
fd <- tbl_df(as.data.frame(fire))
ffd <- fortify(fire)
fd$id <- as.character(0:(nrow(fd)-1))
ffd <- left_join(ffd, fd, by="id", copy=T)

# Get CP 10m DEM, trim and reproject
dem <- raster("/Users/jasper/Documents/GIS/CapePeninsula/DEM10m_LO19/cct_10m")

dem <- crop(dem, c(-64030, -44030, -3810000, -3750000))
#CP_dem <- crop(dem, c(-60000, -44000, -3803538, -3785000))
CP_dem <- projectRaster(dem, crs = CRS("+proj=longlat +datum=WGS84"), extent=extent(fire))
CP_dem <- aggregate(CP_dem, 3)

# Get basemap
bb <- bbox(fire)
b <- (bb - rowMeans(bb)) * 1.05 + rowMeans(bb)
#CP <- get_map("Smitswinkel Bay", zoom=11, source="stamen", maptype="watercolor")
#CP <- get_map(c(18.46127, -34.09663), zoom=11, source="stamen", maptype="watercolor")
CP_toner <- ggmap(get_map(location = b, source = "stamen", maptype = "toner", crop = TRUE, zoom = 11))

CP_wc <- ggmap(get_map(location = b, source = "stamen", maptype = "watercolor", crop = TRUE, zoom = 11))


### Make the plot
x <- as.data.frame(rasterToPoints(CP_dem))

ggplot() + geom_raster(data=x, aes(x=x, y=y, fill=cct_10m, alpha=.5)) + coord_fixed(1.313324) + theme(legend.position="none")

CP_toner + geom_polygon(data = ffd, aes(x = long, y = lat, group = id), alpha = 0.25) + coord_map()

CP_wc + geom_polygon(data = ffd, aes(x = long, y = lat, group = id), alpha = 0.25) + coord_map()

CP_wc + geom_raster(data=x, aes(x=x, y=y, fill=cct_10m, alpha=.25)) + coord_fixed(1.313324)  + theme(legend.position="none")
  
ggsave("Output/CapePeninsula_watercolour.pdf", CP_wc, width=5, height=8)

# Just fires
#p <- ggplot() + geom_polygon(data = ffd, aes(x = long, y = lat, group = id), alpha = 0.5) + coord_map()
#p

# Fires coloured by month
#m <- ggplot() + geom_polygon(data = ffd, aes(x = long, y = lat, group = id, color = as.factor(YEAR)), size = 0.25) + coord_map()

#m

