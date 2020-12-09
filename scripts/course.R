setwd("/home/adam/Downloads/x_vectorSpatial-main/")

america <- read.csv("data/z3/america.csv")
baffin <- read.csv("data/z3/baffin.csv")
eurasia <- read.csv("data/z3/eurasia.csv")

# 
# install.packages("sp")

# load it in 
library(sp)

# There are two important clas types: S3 and S4
# An S3 class - $
# $element
a<- 1:10
b <- 2:11
mod <- lm(b ~ a)


# S4 objects are in sp
# @: slot
amPoly <- Polygon(america)
baPoly <- Polygon(baffin)
euPoly <- Polygon(eurasia)

# 1. column: x coordinate
# 2. column: y

# american shapes with a character ID
as <- Polygons(list(amPoly, baPoly), ID="0")
# Eurasian shape with a character ID
eu <- Polygons(list(euPoly), ID="1")

# SpatialPolygons need a Coordinate Reference System
# This is how R knows that these are longitude and latitude coordinates
# Final level of organization
all <- SpatialPolygons(
	list(as, eu),
	proj4string=CRS("+proj=longlat") 
)

# there is default plotting
plot(all)
plot(all, col="brown")
plot(all, col="brown", border=NA)

###############
# input/output is handled by 'rgdal'
# install.packages("rgdal")

library(rgdal)

# function to read in an ESRI shapefile
wb <- readOGR("data/world_borders/wb.shx")

# this is a SpatialPolygonsDataFrame (SPDF)
class(wb)

# It is the same as a SpatialPolygons but also has a @data slot.
data <- wb@data

# which is a data.frame. The spatial features are linked to the data
# using the rownames of the @data and the IDs of the Polygons(es)
rownames(data)

# For instance, Germany has the ID of "71"
# subsetting just the data
germanyData <- data["71", ]

# subsetting the SPDF
germany <- wb["71", ]

# plotting this shape
plot(germany)

# Country where you come from (I am from Hungary)
# 1. look in the data frame for ID of country (rownames)
# 2. use it to subset
hungary <- wb["80", ]
plot(hungary)


# Default plotting method to plot one of the columns of the @data
spplot(wb, z="POP2005")

############################################
# Triassic fossil localities
dat <- read.csv("data/pbdbloc/triassic.csv")

# draw a map, which is drawn in longitude, latitude space
plot(wb)

# the basic point plotting works fine: x-longitude, y-latitude
points(dat$lng, dat$lat, pch=3, col="red")

# Plotting only Germany
germany <- wb["71", ]
plot(germany)
points(dat$lng, dat$lat, pch=3, col="red")

##################
# How to show only those entries that are in Germany?
# 1. Transform the coordinates to SpatialPoints
# 2. over()

# SpatialPoints=coordinates+CRS
spp <- SpatialPoints(
	dat[, c("lng", "lat")], 
	CRS("+proj=longlat"))

# Returns a data.frame, 
# ncol(ger): the number of columns in germany@data
# nrow(ger): the number of points in spp
# conserves order of points, copies the data entries 
# if not found, the row remains NA
ger <- over(spp, germany)

# How many entries are there that fall within Germany?
table(ger$NAME)

# index subsetting (the column doesn't matter, all of them are NA,
# if the point is not within the shape)
# TRUE: within Germany, FALSE: outside
bInd <- !is.na(ger$LON)

# Use that to select the points to plot, either the SpatialPoints
germanPoints <- spp[bInd, ]

# plot
plot(germany)

# ... or the original data
points(dat[bInd, c("lng", "lat")], col="red", pch=3)

# outside Germany
points(dat[!bInd, c("lng", "lat")], col="blue", pch=3)


################
# Projection change CRS
# projection string for Mollweide
proj <- CRS("+proj=moll")

# transform every coordinate of the SPDF to mollweide - THE STRUCTURE REMAINS THE SAME!
moll <- spTransform(wb, proj)
plot(moll)

# Now the plot is also in Mollweide space, the bounds are
moll@bbox

# the points have to be transformed too:
mollSpp <- spTransform(spp, proj)
points(mollSpp, col="red", pch=3)

