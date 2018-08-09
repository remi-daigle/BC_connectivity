require(readr)
require(sf)
require(sdmpredictors)
require(HMSC)
require(tidyverse)
source("R/infomap_functions.R")

proj <- "+proj=lcc +lat_1=77 +lat_2=49 +lat_0=49 +lon_0=-95 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs "
#### load and clean spatial files ####
habitat <- st_read("shapefiles/habitat_0to250m.shp") %>% st_buffer(dist=0) %>% st_transform(proj)
grid_boxes <- st_read("shapefiles/grid_boxes.shp")%>% st_transform(proj)
grid_boxes$ID <- paste0("gb",sprintf("%03d", 1:nrow(grid_boxes)))

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
coast <- st_read("shapefiles/coast.shp") %>% st_transform(proj)

#### load OBIS data ####

# source("get_OBIS.R")
OBIS_raw <- st_as_sf(read_csv("obis_BC_eez.csv"),
                 coords = c("decimalLongitude", "decimalLatitude"),
                 crs="+proj=longlat +datum=WGS84") %>%
    st_transform(proj)

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
# Overlay occurrences on grid
sp_abun_raw <- st_within(OBIS,grid_boxes,sparse=FALSE) %>% 
    data.frame() %>% 
    setNames(grid_boxes$ID) %>% 
    bind_cols(OBIS,.) %>%
    data.frame() %>% 
    dplyr::select(species,starts_with("gb")) %>% 
    gather(gridID,overlaps,starts_with("gb")) %>% 
    group_by(species,gridID) %>% 
    summarize(obs=sum(overlaps)) %>% 
    ungroup() %>% 
    spread(species,obs) %>% 
    remove_rownames() %>% 
    as.data.frame() %>% 
    column_to_rownames("gridID")%>%
    sapply(as.numeric) %>% 
    as.matrix()

sp_pres_raw <- (sp_abun_raw>0) %>%
    data.frame() %>%
    sapply(as.numeric)

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

enviro_raw <- raster::extract(enviro_raster, grid_boxes_ll, fun = mean, na.rm = T)

########## removeNAs and useless ######################
removeNA <-  enviro_raw %>% 
    is.na() %>% 
    which(.,arr.ind = T) %>% 
    data.frame() %>% 
    dplyr::select(row) %>% 
    unique() %>% 
    unlist()

useless <- (apply(enviro_raw,2,var,na.rm = TRUE)==0) %>% which()

enviro <- enviro_raw[-removeNA,-useless]
sp_abun <- sp_abun_raw[-removeNA,]
sp_pres <- sp_pres_raw[-removeNA,]


# Creating HMSC dataset for analyses
HMSCdata <- as.HMSCdata(Y = sp_abun,
                        X = enviro,
                        Random = data.frame(sampling_unit = grid_boxes$ID[-removeNA]))

memory.limit(memory.limit()*4)
rm(model)
rm(HMSCpred)
gc()
model <- hmsc(HMSCdata)
# Generate predictions
HMSCpred <- cbind(grid_boxes[-removeNA,],predict(model))

more.rows <- data.frame(ID=paste0("gb",sprintf("%03d", removeNA)), row.names = removeNA, stringsAsFactors=F)
HMSCpred[(nrow(HMSCpred) + 1):(nrow(HMSCpred) + nrow(more.rows)), names(more.rows)] <- more.rows
HMSCpred <- HMSCpred[order(HMSCpred$ID),]

# for(sp in 2:(ncol(HMSCpred)-1)){
#     tempdata <- cbind(HMSCpred[,sp],sp_abun=sp_abun[,(sp-1)]/max(sp_abun[,(sp-1)])) %>% 
#         gather(measure,value,-geometry)
#     p1 <- ggplot(tempdata)+
#         geom_sf(aes(fill=value))+
#         facet_grid(~measure)
#     ggsave(paste0("figures/HMSC/",gsub("\\.","_",names(HMSCpred[,sp])[1]),".png"),p1)
# }

load('nbmat.RData')

conmat <- data.frame(matrix(1,nrow=nrow(grid),ncol=nrow(grid)),row.names = grid$ID.1)* nbmat

if(exists('masterBordersHMSC')) rm(masterBordersHMSC)
for(sp in 2:(ncol(HMSCpred)-1)){
    # tempdata <- cbind(HMSCpred[-removeNA,sp],sp_abun=sp_abun[,(sp-1)]/max(sp_abun[,(sp-1)]))
    # plot(tempdata)
    tempdata <- data.frame(HMSCpred)[,sp]
    if(sum(tempdata<0.5,na.rm = TRUE)<(length(tempdata)-30)){
        tempdata[tempdata<0.5] <- 0
        conmat <- data.frame(sqrt(tempdata%*%t(tempdata))) * nbmat
        names(conmat) <- grid$ID.1
        row.names(conmat) <- grid$ID.1
        # image(as.matrix(conmat))
        grid_boxes_tree <- infomap(conmat,grid_boxes, filename = "OBIS", infomap_wd = "C:/Users/Remi-Work/Documents/Infomap/")
        
        borders <- bOrder(grid_boxes,grid_boxes_tree,keepisolated=FALSE)
        
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


masterBordersHMSCLong <- masterBordersHMSC %>% 
    gather(Order,count,-geometry) %>% 
    mutate(Order=gsub("lvl_","",Order)) %>% 
    left_join(nlvl,by = "Order") %>% 
    st_sf() %>% 
    st_set_crs(st_crs(masterBordersHMSC)) %>% 
    mutate(Probability=count/nlvl,
           Label=paste0("HMSC Order = ",Order," (n = ",nlvl,")"),
           border=lengths(st_covers(.,borderlimits))>0)

#limits
xlim <- st_bbox(grid_boxes_tree)[c(1,3)]#+c(-40000,+40000)
ylim <- st_bbox(grid_boxes_tree)[c(2,4)]#+c(-40000,+40000)

masterBordersHMSCLongSorted <- masterBordersHMSCLong[with(masterBordersHMSCLong,order(Order,Probability)),] %>% 
    filter(border==FALSE)

ggplot()+
    geom_sf(data=coast,colour="transparent",fill='grey') +
    geom_sf(data=masterBordersHMSCLongSorted,aes(colour=Probability),size=2)+
    #     # geom_sf(data=filter(x,Order=="3"),aes(colour=Probability),size=2)+
    coord_sf(xlim = xlim, ylim = ylim,expand = F)+
    scale_color_distiller(direction = 1)+
    facet_wrap(~Label)
