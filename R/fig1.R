#### Figure 1 ####
png("./figures/fig1.png",width=18,height=18,units="cm",res=500)


proj <- "+proj=lcc +lat_1=77 +lat_2=49 +lat_0=49 +lon_0=-95 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs "

domain <- readOGR(dsn = "shapefiles",layer = "domain_withislands")
habitat <- readOGR(dsn = "shapefiles",layer = "habitat_0to250m")
gridbox <- readOGR(dsn = "shapefiles",layer = "grid_boxes")
dbbox <- bbox(domain)

blues <- colorRampPalette(c("darkblue", "cyan"))
greys <- colorRampPalette(c(grey(0.4),grey(0.99)))

pac <- getNOAA.bathy(dbbox[1,1],dbbox[1,2],dbbox[2,1],dbbox[2,2],resolution=1,keep=TRUE)

par(mar=c(4,4,1,1))
plot.new()
plot.window(xlim=dbbox[1,],
            ylim=dbbox[2,],
            xaxs="i",
            yaxs="i")
axis(1);axis(2);box()

plot.bathy(pac,
           image = TRUE,
           land = TRUE,
           n=0,
           bpal = list(c(0, max(pac), greys(100)),
                       c(min(pac), 0, blues(100))),
           xaxt='n',
           yaxt='n',
           add=TRUE)
plot(domain,add=T,lwd=3)
plot(habitat,col=rgb(0, 1, 0,alpha=0.4),add=T,border="transparent")
plot(gridbox,border=rgb(0, 1, 0),add=T,lwd=1)
title(xlab="Longitude",ylab="Latitude")

dev.off()
