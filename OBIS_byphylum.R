# devtools::install_github("edzer/sfr")
require(readr)
require(sf)
require(tidyverse)
require(spdep)

#### load and clean spatial files ####
habitat <- st_read("shapefiles/habitat_0to250m.shp") %>% st_buffer(dist=0)
grid_boxes <- st_read("shapefiles/grid_boxes.shp")
grid_boxes$ID <- paste0("gb",1:nrow(grid_boxes))

grid <- st_intersection(habitat,grid_boxes) %>%
    st_cast("MULTIPOLYGON")

# #### get the map ready ####
# USA <- getData('GADM', country="USA", level=1)
# Canada <- getData('GADM', country="CAN", level=1)
#
# coast <- Canada[Canada@data$NAME_1=="British Columbia",]
# coast <- rbind(coast,USA[USA@data$NAME_1=="Alaska"|USA@data$NAME_1=="Washington"|USA@data$NAME_1=="Oregon",][-1,])
#
# grid_boxes <- readOGR(dsn = "data/shapefiles",layer = "grid_boxes")
#
# proj <- "+proj=lcc +lat_1=77 +lat_2=49 +lat_0=49 +lon_0=-95 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs "
# grid_boxes <- spTransform(grid_boxes,CRS(proj))
# coast <- spTransform(coast,CRS(proj))
#
# grid_bb <- as(extent(as.vector(t(bbox(gBuffer(grid_boxes,byid=TRUE,width=50000))))), "SpatialPolygons")
# proj4string(grid_bb) <- proj
# xlim <- bbox(grid_boxes)[1,]
# ylim <- bbox(grid_boxes)[2,]
#
# coast <- SpatialPolygonsDataFrame(gIntersection(coast, grid_bb, byid = T),data.frame(ID=1:4),match.ID = F)
# writeOGR(coast,dsn="shapefiles",layer="coast",driver="ESRI Shapefile")
coast <- st_read("shapefiles/coast.shp")

#### create neighbours matrix ####
nb <- poly2nb(as(grid_boxes,"Spatial"))
nb <- nblag(nb,50)

nbmat <- matrix(0,nrow=nrow(grid),ncol=nrow(grid))
for(i in 1:nrow(grid_boxes)){
    for(j in 1:50)
        nbmat[i,nb[[j]][[i]]] <- 1/j^2
}
image(log(nbmat))

#### load OBIS data ####
# source("get_OBIS.R")
OBIS <- read_csv("obis_BC_eez.csv")
OBIS <- st_as_sf(OBIS,
                 coords = c("decimalLongitude", "decimalLatitude"),
                 crs="+proj=longlat +datum=WGS84") %>%
    filter(!is.na(species))

#remove records not in the habitat grid
OBIS <- OBIS %>%
    mutate(gridID=st_covered_by(.,grid)) %>%
    filter(lengths(gridID)>0) %>%
    mutate(gridID=grid$ID.1[unlist(gridID)])

### clean and filter OBIS data ###

OBISgrid <- OBIS %>%
    data.frame() %>%
    group_by(species,gridID) %>%
    summarize(abundance=n())

# species <- c('Bucephala albeola',
# 'Calcigorgia beringi',
# 'Cepphus columba',
# 'Glyptocephalus zachirus',
# 'Hippoglossoides elassodon',
# 'Hydrolagus colliei',
# 'Larus glaucescens',
# 'Lyopsetta exilis',
# 'Melanitta perspicillata',
# 'Microstomus pacificus',
# 'Oncorhynchus nerka',
# 'Oncorhynchus tshawytscha',
# 'Pandalus jordani',
# 'Phalacrocorax urile',
# 'Sebastes melanops',
# 'Thaleichthys pacificus',
# 'Theragra chalcogramma',
# 'Uria aalge',
# 'Atheresthes stomias')


# for(sp in unique(OBISgrid$species)){
# for(sp in unique(OBIS$species[OBIS$phylum=="Mollusca"])){
for(sp in unique(OBIS$phylum)){

unique(OBIS$phylum)
# for(sp in species){
    usable <- OBIS %>%
        data.frame() %>%
        filter(phylum==sp) %>%
        group_by(species) %>%
        summarize(records=n(),gridID=list(unique(gridID))) %>%
        # filter(records>5000) %>%
        dplyr::select(species,gridID) %>%
        # filter(species==sp)  %>%
        ungroup()
    usable <- usable[lengths(usable$gridID)>1,]
    
    if(nrow(usable)<1) next
    
    OBISusable <- OBISgrid %>% filter(species %in% usable$species)
    
    # plot(grid_boxes$geometry)
    # plot(OBIS$geometry[OBIS$species %in% usable$species],add=T)
    # conmat <- matrix(1,nrow=nrow(grid),ncol=nrow(grid))
    
    conmat <- data.frame(matrix(0,nrow=nrow(grid),ncol=nrow(grid)),row.names = grid$ID.1)
    names(conmat) <- grid$ID.1
    
    for(s in unique(OBISusable$species)){
        index <- which(names(conmat) %in% OBISusable$gridID[OBISusable$species==s])
        
        conmat[index,index] <- conmat[index,index]+1
        # conmat[index,index] <- conmat[index,index]+OBISusable$abundance[OBISusable$species==s]
        
        # conmat[index,index] <- conmat[index,index]+OBISusable$abundance[OBISusable$species==s]%*%t(OBISusable$abundance[OBISusable$species==s])
        
    }
    # conmat <- data.frame(matrix(1,nrow=nrow(grid),ncol=nrow(grid)),row.names = grid$ID.1)
    # names(conmat) <- grid$ID.1
    
    # weight link by influence
    conmat <- conmat*nbmat
    
    # remove non-linked cells
    conmat <- conmat[rowSums(conmat)>0,rowSums(conmat)>0]
    
    
    
    # standardize conmat
    # conmat <- conmat/(sum(OBISusable$abundance)^2)#/rowSums(conmat)
    conmat <- conmat/max(rowSums(conmat))
    # conmat <- conmat/rowSums(conmat)
    # conmat <- conmat/length(unique(OBISusable$species))
    
    
    
    image(log(as.matrix(conmat)))
    
    ### infomap ###
    con_link <- conmat %>% 
        mutate(source=as.numeric(gsub("gb","",row.names(.)))) %>% 
        gather(key=settle,value=conn,starts_with("gb")) %>% 
        mutate(settle=as.numeric(gsub("gb","",settle)))
    
    infomap_wd <- "C:/Users/Remi-Work/Documents/Infomap/"
    
    write.table(con_link,paste0(infomap_wd,"OBIS.net"),sep=" ",col.names=F,row.names=F)
    
    system(paste0(infomap_wd,"/Infomap ",infomap_wd,"OBIS.net ",infomap_wd,"test --clu --bftree --tree --map -i 'link-list'"),intern=T)
    
    grid_boxes_tree <- read.table(paste0(infomap_wd,"test/OBIS.tree")) %>% 
        mutate(ID=paste0("gb",V3)) %>% 
        filter(ID %in% unique(unlist(usable$gridID))) %>% 
        mutate(nlevels=lengths(strsplit(x=as.character(V1),split=":"))-1,
               cell=gsub(".+:","",V1),
               community=gsub(":[0-9]+$","",V1)) %>% 
        separate(col=community,sep=":",into=paste0('lvl',1:max(.$nlevels)),extra="merge",fill="right",remove=FALSE) %>% 
        full_join(grid_boxes,.,by='ID') %>% 
        st_transform("+proj=lcc +lat_1=77 +lat_2=49 +lat_0=49 +lon_0=-95 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs")
    
    
    #### define grid_difference function ####
    grid_difference <- function(x,y,grid_boxes_tree){
        # browser()
        x2 <- x %>% st_cast("MULTILINESTRING") %>% st_combine()
        y2 <- y %>% st_cast("MULTILINESTRING") %>% st_combine()
        g2 <- grid_boxes_tree %>%
            filter(nlevels>=2) %>% st_cast("MULTILINESTRING") %>% st_transform(st_crs(x)) %>% st_combine()
        
        xg_int <- st_intersection(g2,x2)
        gy_diff <- st_difference(g2,y2)
        gx_diff <- st_difference(g2,x2)
        gxgy_diff <- st_difference(gy_diff,gx_diff)
        if(length(gxgy_diff)==0){
            return(st_sf(a=1,b=1,st_sfc(st_multipoint(matrix(0,2,2)),crs=st_crs(x))))
        } else {
            return(st_sf(a=1,b=1,gxgy_diff))
        }
    }
    
    #### create grid_lvls ####
    grid_lvl1 <- grid_boxes_tree %>%
        filter(nlevels>=1) %>% 
        group_by(lvl1) %>%
        summarize(geometry=st_union(geometry)) %>% 
        ungroup() %>% 
        transmute(order="1",
                  lvl=1,
                  geometry=geometry) %>% 
        st_cast("MULTILINESTRING")
    
    if(max(grid_boxes_tree$nlevels,na.rm=T)>=2){
        grid_lvl2 <- grid_boxes_tree %>%
            filter(nlevels>=2) %>% 
            mutate(lvl1=paste(lvl1,lvl2)) %>% 
            group_by(lvl1) %>%
            summarize(geometry=st_union(geometry)) %>% 
            mutate(lvl=lvl1) %>%                               # WHY?! no idea why I need this
            grid_difference(.,grid_lvl1,grid_boxes_tree) %>%
            st_cast("MULTILINESTRING") %>%
            transmute(order="2",
                      lvl=2,
                      geometry=geometry)
    }
    
    if(max(grid_boxes_tree$nlevels,na.rm=T)>=3){
        grid_lvl3 <- grid_boxes_tree %>%
            filter(nlevels>=3) %>% 
            mutate(lvl1=paste(lvl1,lvl2,lvl3)) %>% 
            group_by(lvl1) %>%
            summarize(geometry=st_union(geometry)) %>% 
            mutate(lvl=lvl1) %>%                               # WHY?! no idea why I need this 
            grid_difference(.,grid_lvl1,grid_boxes_tree) %>%
            grid_difference(.,grid_lvl2,grid_boxes_tree) %>%
            st_cast("MULTILINESTRING") %>%
            transmute(order="3",
                      lvl=3,
                      geometry=geometry)
    }
    
    
    if(max(grid_boxes_tree$nlevels,na.rm=T)>=3){
        grid_lvls <- rbind(grid_lvl1,grid_lvl2,grid_lvl3) 
    } else if(max(grid_boxes_tree$nlevels,na.rm=T)>=2){
        grid_lvls <- rbind(grid_lvl1,grid_lvl2) 
    } else {
        grid_lvls <- grid_lvl1
    }
    rm(grid_lvl1)
    rm(grid_lvl2)
    rm(grid_lvl3)
    
    
    ### plot ###
    #limits
    xlim <- bbox(as(grid_boxes_tree,"Spatial"))[c(1,3)]+c(-40000,+40000)
    ylim <- bbox(as(grid_boxes_tree,"Spatial"))[c(2,4)]+c(-40000,+40000)
    
    #colors
    n <- ceiling((length(unique(grid_boxes_tree$community))-1)/4)
    colorpal <- c(rbind(rainbow(n,s=0.25),rainbow(n,s=0.5),rainbow(n,s=0.75),rainbow(n,s=1)))
    
    #plot order
    
    lvls <- starts_with('lvl',vars=names(grid_boxes_tree))
    grid_boxes_tree[,lvls] <- grid_boxes_tree[,lvls] %>%
        lapply(as.numeric) %>%
        data.frame() %>%
        select(-geometry) %>% 
        lapply(as.numeric) 
    
    if(max(grid_boxes_tree$nlevels,na.rm=T)>=3){
        grid_boxes_tree <-  grid_boxes_tree[with(grid_boxes_tree,order(lvl1,lvl2,lvl3)),]
    } else if(max(grid_boxes_tree$nlevels,na.rm=T)>=2){
        grid_boxes_tree <-  grid_boxes_tree[with(grid_boxes_tree,order(lvl1,lvl2)),]
    } else {
        grid_boxes_tree <-  grid_boxes_tree[with(grid_boxes_tree,order(lvl1)),]
    }
    grid_boxes_tree$community <- factor(grid_boxes_tree$community,ordered=T,levels=unique(grid_boxes_tree$community))
    
    
    p <- ggplot()+
        geom_sf(data=coast,colour="transparent",fill='grey') +
        geom_sf(data=grid_boxes_tree,aes(fill=community),colour="transparent") +
        geom_sf(data=grid_lvls,fill='transparent',aes(linetype=order,size=order,color=order)) +
        # geom_sf(data=OBIS[OBIS$species %in% usable,]) +
        scale_fill_manual("Community",values=colorpal) +
        scale_linetype_manual("Order",values=c('1'='solid','2'='dashed','3'='dotted')) +
        scale_size_manual("Order",values=c('1'=1.25,'2'=1,'3'=0.75)) +
        scale_color_manual("Order", values=c('1'='black','2'='grey20','3'='grey40')) +
        ggtitle(sp) +
        coord_sf(xlim = xlim, ylim = ylim,expand = F)
    ggsave(paste0("figures/OBIS/phylum_",sp,".png"),plot=p)
}


