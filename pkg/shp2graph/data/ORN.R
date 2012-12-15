require(maptools)
rn<-readShapeLines(system.file("shapes/test.shp", package="shp2graph")[1],proj4string=CRS(as.character(NA)))