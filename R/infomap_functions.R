#### bOrder ####
bOrder <- function(grid_boxes,grid_boxes_tree,keepisolated=TRUE){
    if(keepisolated==FALSE){
        keep <- table(grid_boxes_tree$community)>1
        keep <- names(keep[keep])
        # grid_boxes <- grid_boxes[keep,]
        
        grid_boxes_tree <- grid_boxes_tree[grid_boxes_tree$community %in% keep,]
    }
    
    borders <- grid_boxes %>%
        st_cast("MULTILINESTRING") %>%
        st_combine() %>%
        st_intersection(.,.) %>%
        st_cast("LINESTRING", group_or_split = T) %>% 
        st_sf(geometry=.) %>% 
        st_transform(proj)

    for(i in max(grid_boxes_tree$nlevels,na.rm = TRUE):1){
        
        index <- grid_boxes_tree %>%
            filter(nlevels>=i) %>% 
            data.frame() %>% 
            unite("lvlID",starts_with("lvl")[1:i],sep=":") %>% 
            st_as_sf() %>%
            group_by(lvlID) %>%
            summarize(geometry=st_union(geometry)) %>%
            mutate(gibberish=NA) %>% 
            st_cast("MULTILINESTRING") %>%
            st_covered_by(borders,.) %>% 
            lengths()>=1
        
        borders$order[index] <- as.character(i)
        
        borders[[paste0("lvl_",i)]] <- index
    }
    
    return(borders)
}




#### getTree ####
getTree <- function(grid_boxes, tree, infomap_wd = "C:/Users/Remi-Work/Documents/Infomap/",proj=st_crs(grid_boxes)){
    grid_boxes_tree <- read.table(paste0(infomap_wd,tree)) %>% 
        mutate(ID=paste0("gb",sprintf("%04d",V3))) %>% 
        mutate(nlevels=lengths(strsplit(x=as.character(V1),split=":"))-1,
               cell=gsub(".+:","",V1),
               community=gsub(":[0-9]+$","",V1)) %>% 
        separate(col=community,sep=":",into=paste0('lvl',1:max(.$nlevels)),extra="merge",fill="right",remove=FALSE) %>% 
        full_join(grid_boxes,.,by='ID') %>% 
        st_transform(proj)
    return(grid_boxes_tree)
}


#### infomap ####
infomap <- function(conmat,grid_boxes, filename, infomap_wd = "C:/Users/Remi-Work/Documents/Infomap/",proj=st_crs(grid_boxes) ){
    con_link <- conmat %>%
        as.data.frame() %>% 
        mutate(source=as.numeric(gsub("gb","",row.names(.)))) %>% 
        gather(key=settle,value=conn,-source) %>% 
        mutate(settle=as.numeric(gsub("gb","",settle)))
    
    write.table(con_link,paste0(infomap_wd,filename,".net"),sep=" ",col.names=F,row.names=F)
    
    system(paste0(infomap_wd,"/Infomap ",infomap_wd,filename,".net ",infomap_wd,"test --clu --bftree --tree --map -i 'link-list'"),intern=T)
    
    grid_boxes_tree <- getTree(grid_boxes = grid_boxes, tree = paste0("test/",filename,".tree"), infomap_wd = infomap_wd ,proj = proj)
    
    return(grid_boxes_tree)
    
    
}

#### plotInfomap ####
plotInfomap <- function(borders,grid_boxes_tree,coast){
    # remove non-borders
    borders <- borders %>% filter(!is.na(order))
    
    #limits
    xlim <- st_bbox(grid_boxes_tree)[c(1,3)]#+c(-40000,+40000)
    ylim <- st_bbox(grid_boxes_tree)[c(2,4)]#+c(-40000,+40000)
    
    #colors
    a <- 0.8
    n <- ceiling((length(unique(grid_boxes_tree$community)))/4)
    colorpal <- c(rbind(rainbow(n,s=0.25,alpha=a),
                        rainbow(n,s=0.5,alpha=a),
                        rainbow(n,s=0.75,alpha=a),
                        rainbow(n,s=1,alpha=a)))
    
    
    p <- ggplot()+
        geom_sf(data=coast,colour="transparent",fill='grey') +
        geom_sf(data=grid_boxes_tree,aes(fill=community),colour="transparent") +
        geom_sf(data=borders,aes(linetype=order,size=order,color=order)) +
        scale_fill_manual("Community",values=colorpal) +
        scale_linetype_manual("Order",values=c('1'='solid','2'='dashed','3'='dotted','4' = 'longdash','5' = 'longdash')) +
        scale_size_manual("Order",values=c('1'=1.25,'2'=1,'3'=0.75, '4'=0.5, '5'=0.25)) +
        scale_color_manual("Order", values=c('1'='black','2'='grey20','3'='grey40','4'='grey60','5' = 'grey80')) +
        coord_sf(xlim = xlim, ylim = ylim,expand = F)
    return(p)
}

# #### test ####
# 
# # devtools::install_github("edzer/sfr")
# # require(readr)
# require(sf)
# require(tidyverse)
# # require(spdep)
# proj <- "+proj=lcc +lat_1=77 +lat_2=49 +lat_0=49 +lon_0=-95 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"
# #### load and clean spatial files ####
# grid_boxes <- st_read("shapefiles/grid_boxes.shp") %>% st_transform(proj)
# grid_boxes$ID <- paste0("gb",1:nrow(grid_boxes))
# grid_boxes$gibberish <- NA
# coast <- st_read("shapefiles/coast.shp") %>% st_transform(proj)
# 
# conmat <- read.csv("data/con_mat/con_mat_per_G1998_pld_57.csv",
#                    row.names=paste0("gb",1:nrow(grid_boxes)),
#                    col.names=paste0("gb",0:nrow(grid_boxes)))[,-1]
# 
# grid_boxes_tree <- infomap(conmat,grid_boxes, filename = "OBIS", infomap_wd = "C:/Users/Remi-Work/Documents/Infomap/")
# 
# borders <- bOrder(grid_boxes,grid_boxes_tree) 
# 
# plotInfomap(borders,grid_boxes_tree)
 
