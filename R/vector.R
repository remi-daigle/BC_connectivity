require(raster)
require(rgdal)
require(rgeos)
require(ncdf4)
require(pracma)
#### get the map ready ####
USA <- getData('GADM', country="USA", level=1)
Canada <- getData('GADM', country="CAN", level=1)

coast <- Canada[Canada@data$NAME_1=="British Columbia",]
coast <- rbind(coast,USA[USA@data$NAME_1=="Alaska"|USA@data$NAME_1=="Washington"|USA@data$NAME_1=="Oregon",][-1,])

grid <- readOGR(dsn = "data/shapefiles",layer = "grid_boxes")

proj <- "+proj=lcc +lat_1=77 +lat_2=49 +lat_0=49 +lon_0=-95 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs "
grid <- spTransform(grid,CRS(proj))
coast <- spTransform(coast,CRS(proj))

grid_bb <- as(extent(as.vector(t(bbox(gBuffer(grid,byid=TRUE,width=50000))))), "SpatialPolygons")
proj4string(grid_bb) <- proj
xlim <- bbox(grid)[1,]
ylim <- bbox(grid)[2,]

coast <- gIntersection(coast, grid_bb, byid = TRUE)




#### make 'nearest' spatial ####
# nearest <- read.csv("data/temperature/nearest_1998.csv")
filenames <- list.files(path="data/temperature", pattern="nearest_", full.names=TRUE,recursive=T)
require(readr)
require(data.table)
datalist <- lapply( filenames, read_csv)
names(datalist) <- substr(filenames,26,29)
nearest <- rbindlist(datalist,idcol = TRUE)

# test <- dataset %>% 
#     group_by(row,col,rowf,colf) %>% 
#     summarize(meandist=mean(meanabsdiff,na.rm=TRUE),
#               sddist=std(meanabsdiff))

nearest$u <- nearest$row-nearest$rowf
nearest$v <- nearest$colf-nearest$col

# nearest$u[is.na(nearest$u)] <- 0
# nearest$v[is.na(nearest$v)] <- 0

nc <- nc_open("data/temperature/98_gb_his_0001.nc")

lon <- ncvar_get(nc,"lon_rho")
lat <- ncvar_get(nc,"lat_rho")

coords <- data.frame(lon=as.vector(lon[cbind(nearest$row,nearest$col)]),
                     lat=as.vector(lat[cbind(nearest$row,nearest$col)])
                     )
coords <- SpatialPoints(coords,CRS("+proj=longlat +ellps=WGS84 +no_defs"))
coords <- spTransform(coords,proj)

# plot(grid)
# plot(coords,add=T)
cont <- gContains(grid,coords,byid = TRUE)
grid$WE <- 0
grid$SN <- 0
for(i in seq(grid)){
  grid$WE[i] <- mean(nearest$u[cont[,i]],na.rm=T)
  grid$SN[i] <- mean(nearest$v[cont[,i]],na.rm=T)
  
}

grid$WE[is.nan(grid$WE)] <- 0
grid$SN[is.nan(grid$SN)] <- 0
# plot(coast,col="grey",border="transparent",xlim=xlim,ylim=ylim,axes=FALSE,xlab="",ylab="")
# 
# # plot(grid)
# quiver(coordinates(grid)[,1],coordinates(grid)[,2],grid$WE,grid$SN,scale=150,lwd=2)
# 
plot(coast,col="grey",border="transparent",xlim=xlim,ylim=ylim,axes=FALSE,xlab="",ylab="")
arrow.plot(a1=coordinates(grid)[,1],a2=coordinates(grid)[,2],v=grid$SN,u=grid$WE,arrowfun=p.arrows,size=0.1,fill='black')


### summary ###
data <- grid@data
require(ggplot2)
require(dplyr)
require(tidyr)
data <- grid@data %>% gather(Direction,Distance,WE,SN)
ggplot(data, aes(Distance*4,colour=Direction)) + 
  geom_freqpoly(binwidth=80,size=3)+
  scale_y_log10()+
  labs(x="Distance (km)")+
    theme_classic()+
    theme(legend.position="bottom",
          axis.line.x = element_line(colour = "black"),
          axis.line.y = element_line(colour = "black"))
    
sd(grid@data$SN*4)
