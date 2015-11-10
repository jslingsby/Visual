##############################################################################
######## Animating weather data for the Cape for 1990-2009 from Wilson and Silander 2014
##############################################################################
######## Compiled by Jasper Slingsby 2015
######## Last edited: 5 November 2015
##############################################################################

### Get libraries
library(raster)
library(reshape2)
library(ggplot2)
library(scales)
library(animation)

### Get data
tmax_mean <- stack("/Users/jasper/Dropbox/Fun/Data/yearmean.nc", varname="tmax_mean")
tmax_sd <- stack("/Users/jasper/Dropbox/Fun/Data/yearmean.nc", varname="tmax_sd")
tmin_mean <- stack("/Users/jasper/Dropbox/Fun/Data/yearmean.nc", varname="tmin_mean")
tmin_sd <- stack("/Users/jasper/Dropbox/Fun/Data/yearmean.nc", varname="tmin_sd")
ppt_mean <- stack("/Users/jasper/Dropbox/Fun/Data/yearmean.nc", varname="ppt_mean")
ppt_sd <- stack("/Users/jasper/Dropbox/Fun/Data/yearmean.nc", varname="ppt_sd")

# Prep data and merge
tmax_mean <- aggregate(tmax_mean, 5, na.rm=T)
tmax_mean <- tmax_mean - mean(tmax_mean)
rng <- range(values(tmax_mean), na.rm=T)

x <- as.data.frame(cbind(coordinates(tmax_mean), values(tmax_mean)))
colnames(x) <- c("x","y", as.character(1990:2009))
y <- melt(x, id=c("x","y"))

tmax_sd <- aggregate(tmax_sd, 5, na.rm=T)

xsd <- as.data.frame(cbind(coordinates(tmax_sd), values(tmax_sd)))
colnames(xsd) <- c("x","y", as.character(1990:2009))
ysd <- melt(xsd, id=c("x","y"))
colnames(ysd)[4] <- "sd"

y <- merge(y, ysd)

#Plot the data one year at a time

year <- as.character(1990:2009)


saveGIF({
for(i in 1:20)
{
x <- na.omit(y[which(y$variable==year[i]),])

print(ggplot(x, aes(x,y)) + theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + geom_point(aes(colour=value, size=1/sd)) + coord_cartesian(xlim=c(17.75,25.8), ylim=c(-34.9,-30.9)) + scale_colour_gradient2(low = muted("blue"), mid = "white", high = muted("red"), midpoint=0, limits = rng, na.value = "grey85", guide = "colourbar") + ggtitle(year[i]))
}
  
}, movie.name = "CapeClimate.gif", ani.width = 700, ani.height = 400, interval=1)


# Plot the data as one set of panels
ggplot(y, aes(x,y)) + theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + geom_point(aes(colour=value)) + coord_cartesian(xlim=c(17.75,25.8), ylim=c(-34.9,-30.9)) + scale_colour_gradient2(low = muted("blue"), mid = "white", high = muted("red"), midpoint=0, limits = rng, na.value = "grey85", guide = "colourbar") + facet_wrap(~variable)



par(mfrow=c(1,1))

# Plot fires with ggplot2
ffd <- fortify(fire)

fd$id <- as.character(0:3500)

ffd <- left_join(ffd, fd, by="id", copy=T)

p <- ggplot() + geom_polygon(data = ffd
                             , aes(x = long, y = lat, group = ID, color = Region)
                             , size = 0.25) +
  coord_map()

p

# Colour by month
m <- ggplot() + geom_polygon(data = ffd
                             , aes(x = long, y = lat, group = ID, color = as.factor(Month))
                             , size = 0.25) +
  coord_map()

m


# Histograms of fires by month
m <- ggplot(fd) + geom_histogram(binwidth=1, position="dodge", aes(x=Month, fill=Region))

m



#ggsave(p, file = "map2.png", width = 5, height = 4.5) # to save image
