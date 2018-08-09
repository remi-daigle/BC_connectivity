if(!"robis" %in% installed.packages()) devtools::install_github("iobis/robis")

require(robis)
require(rgeos)
require(rgdal)
require(leaflet)
habitat <- readOGR(dsn = "shapefiles",layer = "habitat_0to250m")

# Canada <- getData('GADM', country="CAN", level=1)
bb <- bbox(habitat)

lat1 <- bb[2,1]
lat2 <- bb[2,2]
lng1 <- bb[1,1]
lng2 <- bb[1,2]
dlat <- (lat2-lat1)/10
dlng <- (lng2-lng1)/10

grid <- expand.grid(lat=0:9,lng=0:9)
grid$lng1 <- lng1+dlng*grid$lng
grid$lat1 <- lat1+dlat*grid$lat
grid$lng2 <- lng2-dlng*(9-grid$lng)
grid$lat2 <- lat2-dlat*(9-grid$lat)

for(i in 1:nrow(grid)){ 
    print(paste("downloading",i,"of",nrow(grid)))
    bb <- matrix(grid[i,3:6],nrow=2)
    geom <- paste0("POLYGON ((",
                   bb[1,2]," ",bb[2,2],", ",
                   bb[1,2]," ",bb[2,1],", ",
                   bb[1,1]," ",bb[2,1],", ",
                   bb[1,1]," ",bb[2,2],", ",
                   bb[1,2]," ",bb[2,2],"))")
    # geom <- as_wkt(res)
    assign(paste0("data_",i),occurrence(geometry=geom))
}

# make a list of output
data_list <- lapply(ls(pattern="data_*"),get)

# remove empties
data_list <- data_list[unlist(lapply(data_list,nrow))>0]

# fill in empty columns
nam <- unique(unlist(lapply(data_list,names)))
for(i in 1:length(data_list)){
    missing <- NULL
    df <- data_list[[i]]
    missing <- nam[!nam %in% names(df)]
    df[missing] <- NA
    data_list[[i]] <- df[nam]
}

data <- unique(do.call('rbind',data_list))
write.csv(data,"obis_BC_eez.csv")
rm(list=ls())
