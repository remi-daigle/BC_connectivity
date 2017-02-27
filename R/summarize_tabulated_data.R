# uncomment to download, commented by default since it is extremely time consuming
# require(readr)
# require(fields)
# require(dplyr)
# require(rgeos)
# require(rgdal)
# require(tidyr)
# 
# # this is where the raw data from figshare was decompressed
# datadir <- 'G:/Positions/processed/'
# 
# # get the list of raw data files
# filenames <- list.files(datadir, pattern=paste0("^","G_data_"), full.names=F,recursive=F)
# 
# # set default projection
# proj <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# 
# grid <- readOGR(dsn = "shapefiles",layer = "grid_boxes")
# 
# for(fn in filenames){
#     print(paste('loading',fn,round(which(fn==filenames)/length(filenames),2)))
#     
#     fn2 <- paste0(datadir,fn)
#     if(!file.exists(paste0("data/summary_",fn))){
#         temp_data <- read.csv(fn2,nrow=1)
#         raw_data <- read_csv(fn2,col_types = c('idddiicciiiiddiiddi') )
#         assign(substr(fn,1,nchar(fn)-4), raw_data %>% group_by(site) %>% summarize(mean_lat0=mean(lat0),
#                                                                                    mean_long0=mean(long0),
#                                                                                    mean_dist=mean(Dispersal_distance),
#                                                                                    mean_inseadist=mean(Dispersal_insea_distance),
#                                                                                    n_nodes=length(unique(settle_site))))
#         print("writing...")
#         write_csv(get(substr(fn,1,nchar(fn)-4)),paste0("data/summary_",fn))
#     } else {
#         assign(substr(fn,1,nchar(fn)-4),read_csv(paste0(datadir,"summary_",fn)))
#     }
# }