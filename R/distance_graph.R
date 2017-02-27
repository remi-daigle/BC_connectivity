nc <- nc_open("G:/future/2098a/98_gb_his_0060.nc")
mask <- ncvar_get(nc,"mask_rho")
require(sp)

coords <- data.frame(lon=as.vector(ncvar_get(nc,"lon_rho")),lat=as.vector(ncvar_get(nc,"lat_rho")))
coords <- SpatialPoints(coords,CRS("+proj=longlat +ellps=WGS84 +no_defs"))
coords <- spTransform(coords,CRS("+proj=utm +zone=20 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))
require(rgeos)

coordsspdf <- gBuffer(coords,width=1,byid = T)



spdf2graph <- function(spdf){
  nb <- poly2nb(spdf,snap=6500)
  df <- NULL
  spts <- SpatialPoints(spdf)
  for(i in seq(length(spdf))){
    df <- rbind(df,data.frame(from=i,to=nb[[i]],distance=c(gDistance(spts[i,],spts[nb[[i]],],byid=TRUE))))
  }
  m <- as.numeric(sapply(1:nrow(df), function(i)(df[i,1:2])))
  gr <- graph.empty(length(spdf)) %>%
    add.edges(m,weight=df$distance)
  return(gr)
}
require(igraph)
require(spdep)

gr <- spdf2graph(coordsspdf)
write.graph(gr, 'distance_graph.net', format="pajek")
gr <- read.graph('distance_graph.net', format="pajek")

shortest.paths(gr,1,900)