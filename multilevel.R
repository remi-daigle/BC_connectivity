library(sf)
require(sdmpredictors)
require(HMSC)
library(tidyverse)
library(gridExtra)
source("R/infomap_functions.R")
#### load and clean spatial files ####
proj <- "+proj=lcc +lat_1=77 +lat_2=49 +lat_0=49 +lon_0=-95 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"
habitat <- st_read("shapefiles/habitat_0to250m.shp") %>% st_buffer(dist=0) %>% st_transform(proj)
grid_boxes <- st_read("shapefiles/grid_boxes.shp") %>% st_transform(proj)
grid_boxes$ID <- paste0("gb",sprintf("%04d", 1:nrow(grid_boxes)))

grid <- st_intersection(habitat,grid_boxes) %>% 
    st_cast("MULTIPOLYGON")
borderlimits <- grid_boxes %>%
    st_union() %>% 
    st_cast("MULTILINESTRING") %>%
    st_combine() %>%
    st_intersection(.,.) %>%
    st_cast("LINESTRING", group_or_split = T) %>% 
    st_sf(geometry=.) %>% 
    st_transform(proj)
coast <- st_read("shapefiles/coast.shp")

#### LTRANS connectivity ####
allYears <- c(1998:2007)
allPLD <- seq(1,120,7)

for(y in allYears){
    for(p in allPLD){
        conmat <- read.csv(paste0("data/con_mat/con_mat_per_G",y,"_pld_",p,".csv"),
                           row.names=paste0("gb",sprintf("%04d", 1:nrow(grid_boxes))),
                           col.names=paste0("gb",sprintf("%04d", 0:nrow(grid_boxes))))[,-1]
        
        grid_boxes_tree <- infomap(conmat,grid_boxes, filename = "OBIS", infomap_wd = "C:/Users/Remi-Work/Documents/Infomap/")
        
        borders <- bOrder(grid_boxes,grid_boxes_tree,keepisolated = FALSE)
        
        print(paste("year =",y,"PLD =",p,"names = "))
        print(names(borders))
        
        if(y==min(allYears)&p==min(allPLD)){
            masterBorders <- borders %>% select(-order)
            nlvl <- data.frame(Order=c("5","4","3","2","1"),
                               nlvl=c(rep(0,7-ncol(borders)),rep(1,ncol(borders)-2)),
                               stringsAsFactors = FALSE)
        } else {
            while(ncol(borders)-ncol(masterBorders)>1){
                missing <- sort(names(borders[,-2])[!(names(borders[,-2]) %in% names(masterBorders))])
                masterBorders[[missing[1]]] <- 0
                
            }
            
            for(l in paste0("lvl_",1:5)){
                if(l %in% names(borders))
                    masterBorders[[l]] <- masterBorders[[l]]+borders[[l]]
                
            }
            
            nlvl$nlvl <- nlvl$nlvl + c(rep(0,7-ncol(borders)),rep(1,ncol(borders)-2))
            # blvl <- 3:ncol(borders)
            # mblvl <- rev(blvl+ncol(masterBorders)-ncol(borders))
            # 
            # masterBorders[,mblvl] <- data.frame(masterBorders)[,mblvl]+data.frame(borders)[,blvl]
            # 
        }
        print(nlvl$nlvl)
        
        # plotInfomap(borders,grid_boxes_tree,coast)+labs(title=sp)
    }
}

borderlimits <- grid_boxes %>%
    st_union() %>% 
    st_cast("MULTILINESTRING") %>%
    st_combine() %>%
    st_intersection(.,.) %>%
    st_cast("LINESTRING", group_or_split = T) %>% 
    st_sf(geometry=.) %>% 
    st_transform(proj)

masterBordersLong <- masterBorders %>% 
    gather(Order,count,-geometry) %>% 
    mutate(Order=gsub("lvl_","",Order)) %>% 
    left_join(nlvl,by = "Order") %>% 
    st_sf() %>% 
    st_set_crs(st_crs(masterBorders)) %>% 
    mutate(Probability=count/nlvl,
           Label=paste0("LTRANS Order = ",Order," (n = ",nlvl,")"),
           border=lengths(st_covers(.,borderlimits))>0)

head(masterBordersLong)

#limits
xlim <- st_bbox(grid_boxes_tree)[c(1,3)]#+c(-40000,+40000)
ylim <- st_bbox(grid_boxes_tree)[c(2,4)]#+c(-40000,+40000)

masterBordersLongSorted <- masterBordersLong[with(masterBordersLong,order(Order,Probability)),] %>% 
    filter(border==FALSE,Order<4)

# ggplot()+
#     geom_sf(data=coast,colour="transparent",fill='grey') +
#     geom_sf(data=masterBordersLongSorted,aes(colour=Probability),size=2)+
#     # geom_sf(data=filter(x,Order=="3"),aes(colour=Probability),size=2)+
#     coord_sf(xlim = xlim, ylim = ylim,expand = F)+
#     scale_color_distiller(direction = 1)+
#     facet_wrap(~Label)

# p1 <- ggplot()+
#     geom_sf(data=coast,colour="transparent",fill='grey') +
#     geom_sf(data=filter(masterBordersLongSorted,Order=="1"),aes(colour=Probability),size=2)+
#     coord_sf(xlim = xlim, ylim = ylim,expand = F)+
#     scale_color_distiller(direction = 1)+
#     facet_wrap(~Label)
# p2 <- ggplot()+
#     geom_sf(data=coast,colour="transparent",fill='grey') +
#     geom_sf(data=filter(masterBordersLongSorted,Order=="2"),aes(colour=Probability),size=2)+
#     coord_sf(xlim = xlim, ylim = ylim,expand = F)+
#     scale_color_distiller(direction = 1)+
#     facet_wrap(~Label)
# p3 <- ggplot()+
#     geom_sf(data=coast,colour="transparent",fill='grey') +
#     geom_sf(data=filter(masterBordersLongSorted,Order=="3"),aes(colour=Probability),size=2)+
#     coord_sf(xlim = xlim, ylim = ylim,expand = F)+
#     scale_color_distiller(direction = 1)+
#     facet_wrap(~Label)

# #######################################################################################################################
# #### OBIS connectivity ####
# #### create neighbours matrix ####
# # nb <- poly2nb(as(grid_boxes,"Spatial"))
# # nb <- nblag(nb,50)
# # 
# # nbmat <- matrix(0,nrow=nrow(grid),ncol=nrow(grid))
# # for(i in 1:nrow(grid_boxes)){
# #     for(j in 1:50)
# #         nbmat[i,nb[[j]][[i]]] <- 1/j^2
# # }
# # save(nbmat,file='nbmat.RData')
# # image(log(nbmat))
# load('nbmat.RData')
# 
# #### load OBIS data ####
# # source("get_OBIS.R")
# OBIS <- read_csv("obis_BC_eez.csv") %>% 
#     st_as_sf(coords = c("decimalLongitude", "decimalLatitude"),
#              crs="+proj=longlat +datum=WGS84") %>% 
#     filter(!is.na(species)) %>% 
#     st_transform(proj) %>% 
#     mutate(gridID=st_covered_by(.,grid)) %>% #remove records not in the habitat grid
#     filter(lengths(gridID)>0) %>% 
#     mutate(gridID=grid$ID.1[unlist(gridID)])
# 
# ### clean and filter OBIS data ###
# 
# OBISgrid <- OBIS %>% 
#     data.frame() %>% 
#     group_by(species,gridID) %>% 
#     summarize(abundance=n())
# 
# # unique(OBIS$phylum)
# 
# usable <- OBIS %>%
#     data.frame() %>%
#     filter(phylum!="Chordata") %>% 
#     group_by(species) %>%
#     summarize(records=n(),gridID=list(gridID)) %>%
#     filter(records>50,lengths(gridID)>50) %>%
#     dplyr::select(species,gridID) %>%
#     ungroup()
# 
# OBISusable <- OBISgrid %>% filter(species %in% usable$species)
# 
# 
# #### make conmat ####
# 
# 
# for(sp in unique(OBISusable$species)){
#     conmat <- data.frame(matrix(0,nrow=nrow(grid),ncol=nrow(grid)),row.names = grid$ID.1)
#     names(conmat) <- grid$ID.1
#     index <- which(grid$ID.1 %in% OBISusable$gridID[OBISusable$species==sp])
#     # geometric mean
#     tempmat <- sqrt(OBISusable$abundance[OBISusable$species==sp]%*%t(OBISusable$abundance[OBISusable$species==sp]))
#     conmat[index,index]<- tempmat/rowSums(tempmat)
#     rowSums(conmat)
#     # weight link by influence
#     conmat <- conmat*nbmat    
#     
#     grid_boxes_tree <- infomap(conmat,grid_boxes, filename = "OBIS", infomap_wd = "C:/Users/Remi-Work/Documents/Infomap/")
#     
#     borders <- bOrder(grid_boxes,grid_boxes_tree,keepisolated=FALSE)
#     
#     print(paste("species =",sp,"names = "))
#     print(names(borders))
#     
#     if(sp==unique(OBISusable$species)[1]){
#         masterBordersOBIS <- borders %>% select(-order)
#         nlvl <- data.frame(Order=c("5","4","3","2","1"),
#                            nlvl=c(rep(0,7-ncol(borders)),rep(1,ncol(borders)-2)),
#                            stringsAsFactors = FALSE)
#     } else {
#         while(ncol(borders)-ncol(masterBordersOBIS)>1){
#             missing <- sort(names(borders[,-2])[!(names(borders[,-2]) %in% names(masterBordersOBIS))])
#             masterBordersOBIS[[missing[1]]] <- 0
#             
#         }
#         
#         for(l in paste0("lvl_",1:5)){
#             if(l %in% names(borders))
#                 masterBordersOBIS[[l]] <- masterBordersOBIS[[l]]+borders[[l]]
#             
#         }
#         
#         nlvl$nlvl <- nlvl$nlvl + c(rep(0,7-ncol(borders)),rep(1,ncol(borders)-2))
#         # blvl <- 3:ncol(borders)
#         # mblvl <- rev(blvl+ncol(masterBordersOBIS)-ncol(borders))
#         # 
#         # masterBordersOBIS[,mblvl] <- data.frame(masterBordersOBIS)[,mblvl]+data.frame(borders)[,blvl]
#         # 
#     }
#     print(nlvl$nlvl)
#     
#     # plotInfomap(borders,grid_boxes_tree,coast)+labs(title=sp)
# }
# 
# masterBordersOBISLong <- masterBordersOBIS %>% 
#     gather(Order,count,-geometry) %>% 
#     mutate(Order=gsub("lvl_","",Order)) %>% 
#     left_join(nlvl,by = "Order") %>% 
#     st_sf() %>% 
#     st_set_crs(st_crs(masterBorders)) %>% 
#     mutate(Probability=count/nlvl,
#            Label=paste0("OBIS Order = ",Order," (n = ",nlvl,")"),
#            border=lengths(st_covers(.,borderlimits))>0)
# 
# #limits
# xlim <- st_bbox(grid_boxes_tree)[c(1,3)]#+c(-40000,+40000)
# ylim <- st_bbox(grid_boxes_tree)[c(2,4)]#+c(-40000,+40000)
# 
# masterBordersOBISLongSorted <- masterBordersOBISLong[with(masterBordersOBISLong,order(Order,Probability)),] %>% 
#     filter(border==FALSE)
# 
# ggplot()+
#     geom_sf(data=coast,colour="transparent",fill='grey') +
#     geom_sf(data=masterBordersOBISLongSorted,aes(colour=Probability),size=2)+
# #     # geom_sf(data=filter(x,Order=="3"),aes(colour=Probability),size=2)+
#     coord_sf(xlim = xlim, ylim = ylim,expand = F)+
#     scale_color_distiller(direction = 1)+
#     facet_wrap(~Label)
# 
# # p4 <- ggplot()+
# #     geom_sf(data=coast,colour="transparent",fill='grey') +
# #     geom_sf(data=filter(masterBordersOBISLongSorted,Order=="1"),aes(colour=Probability),size=2)+
# #     coord_sf(xlim = xlim, ylim = ylim,expand = F)+
# #     scale_color_distiller(direction = 1)+
# #     facet_wrap(~Label)
# # p5 <- ggplot()+
# #     geom_sf(data=coast,colour="transparent",fill='grey') +
# #     geom_sf(data=filter(masterBordersOBISLongSorted,Order=="2"),aes(colour=Probability),size=2)+
# #     coord_sf(xlim = xlim, ylim = ylim,expand = F)+
# #     scale_color_distiller(direction = 1)+
# #     facet_wrap(~Label)
# # p6 <- ggplot()+
# #     geom_sf(data=coast,colour="transparent",fill='grey') +
# #     geom_sf(data=filter(masterBordersOBISLongSorted,Order=="3"),aes(colour=Probability),size=2)+
# #     coord_sf(xlim = xlim, ylim = ylim,expand = F)+
# #     scale_color_distiller(direction = 1)+
# #     facet_wrap(~Label)
# # 
# # grid.arrange(p1,p2,p3,p4,p5,p6,layout_matrix = matrix(1:6,ncol=3,byrow = TRUE))

################################################### HMSC ###############################################
#### load OBIS data ####

# source("get_OBIS.R")
OBIS_raw <- st_as_sf(read_csv("obis_BC_eez.csv"),
                     coords = c("decimalLongitude", "decimalLatitude"),
                     crs="+proj=longlat +datum=WGS84") %>%
    st_transform(proj) %>% 
    filter(st_covers(st_union(grid_boxes),.,sparse=FALSE))

usable <- OBIS_raw %>%
    mutate(gridID=st_covered_by(.,grid)) %>% #remove records not in the habitat grid
    filter(lengths(gridID)>0) %>% 
    mutate(gridID=grid$ID.1[unlist(gridID)]) %>% 
    data.frame() %>%
    filter(phylum!="Chordata",!is.na(species)) %>% 
    group_by(species) %>%
    summarize(records=n(),gridID=list(gridID)) %>%
    filter(records>50,lengths(gridID)>5) %>%
    dplyr::select(species,gridID) %>%
    ungroup()

OBIS <- OBIS_raw %>% 
    filter(species %in% usable$species)
rm(OBIS_raw)
# # Overlay occurrences on grid
# sp_abun_raw <- st_within(OBIS,grid_boxes,sparse=FALSE) %>% 
#     data.frame() %>% 
#     setNames(grid_boxes$ID) %>% 
#     bind_cols(OBIS,.) %>%
#     data.frame() %>% 
#     dplyr::select(species,starts_with("gb")) %>% 
#     gather(gridID,overlaps,starts_with("gb")) %>% 
#     group_by(species,gridID) %>% 
#     summarize(obs=sum(overlaps)) %>% 
#     ungroup() %>% 
#     spread(species,obs) %>% 
#     remove_rownames() %>% 
#     as.data.frame() %>% 
#     column_to_rownames("gridID")%>%
#     sapply(as.numeric) %>% 
#     as.matrix()
# 
# sp_pres_raw <- (sp_abun_raw>0) %>%
#     data.frame() %>%
#     sapply(as.numeric)

######### Environmental ########################
layers_future <- list_layers_future()
layers <- list_layers(terrestrial=FALSE,marine=TRUE) %>% 
    filter(layer_code %in% layers_future$current_layer_code)

grid_boxes_ll <- grid_boxes %>% 
    st_transform("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") %>% 
    as("Spatial")
enviro_raster <- load_layers(layers$layer_code,datadir=paste0(getwd(),"/data/SDM/")) %>% 
    crop(grid_boxes_ll) %>% 
    mask(grid_boxes_ll)

# enviro_raw <- raster::extract(enviro_raster, grid_boxes_ll, fun = mean, na.rm = T)
enviro_raw <- as.matrix(enviro_raster)

########## abundance and presence #####################
enviro_pts <- st_as_sf(as(enviro_raster,"SpatialPointsDataFrame")) %>% 
    st_transform(proj)

which.min(st_distance(enviro_pts$geometry,OBIS$geometry[1]))
OBIS <- OBIS %>% 
    rowwise() %>% 
    mutate(enviro_cell = which.min(st_distance(enviro_pts,st_sfc(list(geometry),crs=proj))))

sp_abun_raw <- OBIS %>% 
    group_by(enviro_cell,scientificName) %>% 
    summarize(abundance=n()) %>% 
    spread(scientificName,abundance,fill=0) 

sp_pres_raw <- OBIS %>% 
    group_by(enviro_cell,scientificName) %>% 
    summarize(presence=n()>0) %>% 
    spread(scientificName,presence,fill='FALSE')

########## remove NAs and useless ######################
removeNA <-  enviro_raw %>% 
    is.na() %>% 
    which(.,arr.ind = T) %>% 
    data.frame() %>% 
    dplyr::select(row) %>% 
    unique() %>% 
    unlist()

useless <- (apply(enviro_raw,2,var,na.rm = TRUE)==0) %>% which()

enviro <- enviro_raw[-removeNA,-useless]
sp_abun <- merge(sp_abun_raw,data.frame(enviro_cell=1:nrow(enviro)),all=TRUE) %>% 
    dplyr::select(-enviro_cell)
sp_abun[is.na(sp_abun)] <- 0
sp_pres <- merge(sp_pres_raw,data.frame(enviro_cell=1:nrow(enviro)),all=TRUE) %>% 
    dplyr::select(-enviro_cell)
sp_pres[is.na(sp_pres)] <- FALSE



# Creating HMSC dataset for analyses
HMSCdata <- as.HMSCdata(Y = as.matrix(sp_abun),
                        X = as.matrix(enviro),
                        Random = data.frame(enviro_cell=factor(1:nrow(enviro))))

memory.limit(memory.limit()*4)
rm(model)
rm(HMSCpred)
gc()
model <- hmsc(HMSCdata)
# Generate predictions
HMSCpred <- cbind(data.frame(enviro_cell=factor(1:nrow(enviro))),predict(model))

# more.rows <- data.frame(ID=paste0("gb",sprintf("%04d", removeNA)), row.names = removeNA, stringsAsFactors=F)
# HMSCpred[(nrow(HMSCpred) + 1):(nrow(HMSCpred) + nrow(more.rows)), names(more.rows)] <- more.rows
# HMSCpred <- HMSCpred[order(HMSCpred$ID),]

# for(sp in 2:(ncol(HMSCpred)-1)){
#     tempdata <- cbind(HMSCpred[,sp],sp_abun=sp_abun[,(sp-1)]/max(sp_abun[,(sp-1)])) %>% 
#         gather(measure,value,-geometry)
#     p1 <- ggplot(tempdata)+
#         geom_sf(aes(fill=value))+
#         facet_grid(~measure)
#     ggsave(paste0("figures/HMSC/",gsub("\\.","_",names(HMSCpred[,sp])[1]),".png"),p1)
# }


#### create neighbours matrix ####
# require(spdep)
# nb <- poly2nb(as(enviro,'SpatialPolygons'))
# nb <- nblag(nb,50)
# 
# nbmat <- matrix(0,nrow=nrow(enviro),ncol=nrow(enviro))
# for(i in 1:nrow(enviro)){
#     for(j in 1:50)
#         nbmat[i,nb[[j]][[i]]] <- 1/j^2
# }
# save(nbmat,file='nbmat_raster.RData')
# image(log(nbmat))
load('nbmat_raster.RData')

conmat <- data.frame(matrix(1,nrow=nrow(enviro),ncol=nrow(enviro)),row.names = 1:nrow(enviro))* nbmat

grid_boxes_raster <- as(as(enviro_raster,"SpatialPolygonsDataFrame"),"sf") %>% 
    st_transform(proj)
grid_boxes_raster$ID <- paste0("gb",sprintf("%04d", 1:nrow(enviro))) 
if(exists('masterBordersHMSC')) rm(masterBordersHMSC)
for(sp in 2:(ncol(HMSCpred)-1)){
    # tempdata <- cbind(HMSCpred[-removeNA,sp],sp_abun=sp_abun[,(sp-1)]/max(sp_abun[,(sp-1)]))
    # plot(tempdata)
    tempdata <- data.frame(HMSCpred)[,sp]
    if(sum(tempdata<0.5,na.rm = TRUE)<(length(tempdata)-30)){
        tempdata[tempdata<0.5] <- 0
        conmat <- data.frame(sqrt(tempdata%*%t(tempdata))) * nbmat
        names(conmat) <- 1:nrow(enviro)
        row.names(conmat) <- 1:nrow(enviro)
        # image(as.matrix(conmat))
        grid_boxes_tree <- infomap(conmat,grid_boxes_raster, filename = "OBIS", infomap_wd = "C:/Users/Remi-Work/Documents/Infomap/")
        
        borders <- bOrder(grid_boxes_raster,grid_boxes_tree,keepisolated=FALSE)
        
        print(paste("species =",names(HMSCpred)[sp],"names = "))
        print(names(borders))
        
        if(!exists('masterBordersHMSC')){
            masterBordersHMSC <- borders %>% dplyr::select(-order)
            nlvl <- data.frame(Order=c("5","4","3","2","1"),
                               nlvl=c(rep(0,7-ncol(borders)),rep(1,ncol(borders)-2)),
                               stringsAsFactors = FALSE)
        } else {
            while(ncol(borders)-ncol(masterBordersHMSC)>1){
                missing <- sort(names(borders[,-2])[!(names(borders[,-2]) %in% names(masterBordersHMSC))])
                masterBordersHMSC[[missing[1]]] <- 0
                
            }
            
            for(l in paste0("lvl_",1:5)){
                if(l %in% names(borders))
                    masterBordersHMSC[[l]] <- masterBordersHMSC[[l]]+borders[[l]]
                
            }
            
            nlvl$nlvl <- nlvl$nlvl + c(rep(0,7-ncol(borders)),rep(1,ncol(borders)-2))
            # blvl <- 3:ncol(borders)
            # mblvl <- rev(blvl+ncol(masterBordersHMSC)-ncol(borders))
            # 
            # masterBordersHMSC[,mblvl] <- data.frame(masterBordersHMSC)[,mblvl]+data.frame(borders)[,blvl]
            # 
        }
    }
}

borderlimits_raster <- grid_boxes_raster %>%
    st_union() %>% 
    st_cast("MULTILINESTRING") %>%
    st_combine() %>%
    st_intersection(.,.) %>%
    st_cast("LINESTRING", group_or_split = T) %>% 
    st_sf(geometry=.) %>% 
    st_transform(proj)

masterBordersHMSCLong <- masterBordersHMSC %>% 
    gather(Order,count,-geometry) %>% 
    mutate(Order=gsub("lvl_","",Order)) %>% 
    left_join(nlvl,by = "Order") %>% 
    st_sf() %>% 
    st_set_crs(st_crs(masterBordersHMSC)) %>% 
    mutate(Probability=count/nlvl,
           Label=paste0("HMSC Order = ",Order," (n = ",nlvl,")"),
           border=lengths(st_covers(.,borderlimits_raster))>0)

#limits
xlim <- st_bbox(grid_boxes_tree)[c(1,3)]#+c(-40000,+40000)
ylim <- st_bbox(grid_boxes_tree)[c(2,4)]#+c(-40000,+40000)

masterBordersHMSCLongSorted <- masterBordersHMSCLong[with(masterBordersHMSCLong,order(Order,Probability)),] %>% 
    filter(border==FALSE)


masterBordersall <- rbind(masterBordersLongSorted,masterBordersHMSCLongSorted)

ggplot()+
    geom_sf(data=coast,colour="transparent",fill='grey') +
    geom_sf(data=masterBordersall,aes(colour=Probability),size=2)+
    coord_sf(xlim = xlim, ylim = ylim,expand = F)+
    scale_color_distiller(direction = 1,palette = "PuBuGn")+
    facet_wrap(~Label)
