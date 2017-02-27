##### get the list of summarized data files ####
filenames <- list.files("data/summaries", pattern="summary_G.", full.names=F,recursive=F)

for(fn in filenames){
    assign(substr(fn,1,nchar(fn)-4),read_csv(paste0("data/summaries/",fn)))
}

# make master dataframe
df_list <- mget(ls(pattern='._data_.+'))
all <- bind_rows(df_list)
all$id <- rep(names(df_list), sapply(df_list, nrow))
all <- all %>% 
    separate(id,c('useless3','type','useless','year','useless2','pld'),sep="_") %>%
    dplyr::select(-starts_with("useless"))

all$climate <- "Present"
all$climate[all$year>2020] <- "Future"
rm(list=ls(pattern='._data_.+'))

all$pld <- as.numeric(all$pld)


#### load con_mat data ####
filenames <- list.files("data/con_mat", pattern="con_mat_per_.", full.names=F,recursive=F)

con_mat_data <- lapply(filenames,function(x) as.matrix(assign(substr(x,1,nchar(x)-4),read.csv(paste0("data/con_mat/",x),row.names=1))))
con_mat_data <- simplify2array(con_mat_data)
con_mat_data_legend <- matrix(unlist(sapply(filenames,strsplit,"_")),nrow=length(filenames),byrow=T)
con_mat_data_legend <- data.frame(cbind(type=substr(con_mat_data_legend[,4],1,1),
                                year=substr(con_mat_data_legend[,4],2,5),
                                PLD=substr(con_mat_data_legend[,6],1,nchar(con_mat_data_legend[,6])-4),
                                time="Present"),
                          stringsAsFactors=FALSE)
con_mat_data_legend$time[con_mat_data_legend$year>2015] <- "Future"

# function to get the right matrices
get_matrix_fun <- function(con_mat_data,con_mat_data_legend,year=c(1998:2007,2098:2107),time=c("Present","Future"),PLD=c(0:17*7+1),FUN){
    apply(con_mat_data[,,con_mat_data_legend$year%in%year&
                   con_mat_data_legend$PLD%in%PLD&
                   con_mat_data_legend$time%in%time], c(1,2), FUN)
}


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

coast <- gIntersection(coast, grid_bb, byid = T)
