##############################
# R Training: Session 2 Demo #
##############################

rm(list=ls()) # this is a handy way to clear all objects from memory before re-running script each time

options(stringsAsFactors = FALSE) # this changes the default behavior of data frames to no automatically convert strings to factors

library(Hmisc)
library(rgdal)

D = read.csv('crimes2017_sample.csv') # read demo data as csv file

head(D)
tail(D)
names(D)
str(D)
describe(D) # various ways to explore data 

boundaries = readOGR("chi_community_areas.geojson", "OGRGeoJSON") # importing a spatial file stored in geojson format

proj4string(boundaries) # checking the projection (CRS)

plot(boundaries, col="grey") # plotting the shape file

points(D$Longitude, D$Latitude, col="#aa000077", pch=20, cex=.8) # adding crime locations

p1 = ggplot() + geom_polygon(data=boundaries, aes(x=long, y=lat, group=group)) 
p1 # using ggplot for mapping the community area boundaries

p1 + geom_point(data=D, aes(x=Longitude, y=Latitude), color="red") # adding crime locations

p1 + geom_point(data=D, aes(x=Longitude, y=Latitude, color=Primary.Type)) # adding crime locations and coloring by crime type

D_agg = aggregate(X~Community.Area, data=D, FUN=length) # aggregating by Community Area

D_agg = aggregate(X~Primary.Type + Community.Area, data=D, FUN=length) # aggregating by CA and crime type

D_agg_theft = D_agg[D_agg$Primary.Type=="THEFT",] # filtering only thefts

D_agg_burglary = D_agg[D_agg$Primary.Type=="BURGLARY",] # filtering only burglaries

D_agg_merge = merge(D_agg_theft, D_agg_murder, by="Community.Area", all=TRUE)

names(D_agg_merge)[3] = "n_theft"
names(D_agg_merge)[5] = "n_burglary"

D_agg_merge$n_burglary[is.na(D_agg_merge$n_burglary)] = 0
D_agg_merge$n_theft[is.na(D_agg_merge$n_theft)] = 0

plot(D_agg_merge$n_theft, D_agg_merge$n_burglary)

M = lm(n_theft ~ n_burglary, data=D_agg_merge)
summary(M)
