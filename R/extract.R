require(ncdf4)
require(abind)
require(igraph)
require(rgdal)
require(rgeos)
# nc <- nc_open("98_gb_his_0001.nc")

# memory.limit(16304*2)

for(year in 1998:2007){
  
  year2 <- year+100
  
  #### present ####
  if(year>2020){
    filenames <- list.files(path=paste0("G:/future/",year,"a"),pattern="_.....nc", full.names=TRUE,recursive=T)
    o <- "2060-01-01"
    
  } else{
    filenames <- list.files(path=paste0("G:/1998_2007/",year,"a"),pattern="_.....nc", full.names=TRUE,recursive=T)
    o <- "1990-01-01"
  }
  
  ocean_time <- lapply(lapply(filenames,nc_open), FUN = function(x){ncvar_get(x,varid = 'ocean_time')[1];nc_close(x)})
  ocean_time <- as.POSIXct(unlist(ocean_time), origin = o)
  
  # temp <- nc_open(filenames[1])
  # temp2 <- array(ncvar_get(temp,"temp")[,,,1],dim=c(146,410,30,1))
  # temp <- ncvar_get(temp,"temp")[,,,1]
  # dim(temp2)
  # dim(temp)
  # identical(temp2[,,,1],temp)
  
  temp <- lapply(lapply(filenames,nc_open), FUN = function(x){y=array(ncvar_get(x,varid = 'temp')[,,,1],dim=c(146,410,30,1));nc_close(x);return(y)})
  temp <- do.call("abind",list(temp))
  temp[temp>1000] <- NA
  # temp <- array(temp,dim=c(nrow(temp),ncol(temp),30*191))
  
  #### future ####  
  
  if(year2>2020){
    filenames <- list.files(path=paste0("G:/future/",year2,"a"),pattern="_.....nc", full.names=TRUE,recursive=T)[60:250]
    o <- "2060-01-01"
    
  } else{
    filenames <- list.files(path=paste0("G:/1998_2007/",year2,"a"),pattern="_.....nc", full.names=TRUE,recursive=T)
    o <- "1990-01-01"
  }
  
  ocean_time2 <- lapply(lapply(filenames,nc_open), FUN = function(x){ncvar_get(x,varid = 'ocean_time')[1];nc_close(x)})
  ocean_time2 <- as.POSIXct(unlist(ocean_time2), origin = o)
  
  temp2 <- lapply(lapply(filenames,nc_open), FUN = function(x){y=array(ncvar_get(x,varid = 'temp')[,,,1],dim=c(146,410,30,1));nc_close(x);return(y)})
  temp2 <- do.call("abind",temp2)
  temp2[temp2>1000] <- NA
  
  #### extract mask & depth ####
  nc <- nc_open(filenames[1])
  mask <- ncvar_get(nc,"mask_rho")
  depth <- ncvar_get(nc,'h')
  
  #### distance graph ####
  gr <- read.graph('distance_graph.net', format="pajek")
  
  nearest <- data.frame(which(mask==1,arr.ind = TRUE))
  nearest$rowf <- NA
  nearest$colf <- NA
  nearest$meanabsdiff <- NA
  
  grid <- readOGR(dsn = "data/shapefiles",layer = "grid_boxes")
  
  proj <- "+proj=lcc +lat_1=77 +lat_2=49 +lat_0=49 +lon_0=-95 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs "
  grid <- spTransform(grid,CRS(proj))
  
  lon <- ncvar_get(nc,"lon_rho")
  lat <- ncvar_get(nc,"lat_rho")
  
  coords <- data.frame(lon=as.vector(lon[cbind(nearest$row,nearest$col)]),
                       lat=as.vector(lat[cbind(nearest$row,nearest$col)])
  )
  coords <- SpatialPoints(coords,CRS("+proj=longlat +ellps=WGS84 +no_defs"))
  coords <- spTransform(coords,proj)
  
  cont <- gContains(grid,coords,byid = TRUE)
  nearest <- nearest[apply(cont,1,any),]
  
  
  
  for(r in sample(seq(1,nrow(nearest))[is.na(nearest$rowf)] )){
    i <- nearest$row[r]
    j <- nearest$col[r]
    print(paste(i,j))
    if(any(is.na(nearest[r,3:4]))){
      margin <- 0.05
      depthindex <- which(depth>(depth[i,j]*(1-margin))&depth<(depth[i,j]*(1+margin)),arr.ind=TRUE)
      
      
      x <- apply(depthindex,1,function(x) sum(as.vector((temp2[x[1],x[2],,])-as.vector(temp[i,j,,]))^2))
      
      
      nearest[r,c('rowf','colf')] <- depthindex[which(min(x,na.rm = TRUE)==x,arr.ind = TRUE),]
      nearest[r,'meanabsdiff'] <- mean(abs(temp[nearest[r,'row'],nearest[r,'col'],,]-
                                             temp2[nearest[r,'rowf'],nearest[r,'colf'],,]))
      
    }
  }
  write.csv(nearest,paste0("data/temperature/nearest_",year,".csv"),row.names = FALSE)
  gc()
}