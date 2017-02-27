#### Figure 6 ####
png(paste0("./figures/fig6.png"),width=18,height=18,units="cm",res=500)
layout(matrix(c(1,2,5,6,3,4,7,8),ncol=4,byrow = TRUE),widths=c(2,5,5,2))

#### loop for pld and years ####
for(t in c("Future","Present")){
    par(mar=c(1,1,4,1))
    image(matrix(c(0:10),nrow=1),col=rev(heat.colors(11)),axes=F)
    text(0,c(0:10)/10,c(0:10))
    box(bty="o")
    count <- rep(0,length(grid))
    for(pld in seq(1,120,7)){
        for(year in unique(con_mat_data_legend$year[con_mat_data_legend$time==t])){
            tree <- read.table(paste0(infomap_wd,"test/G",year,"_pld_",pld,".tree"))
            tree$smallest <- unlist(lapply(strsplit(as.character(tree$V1),":"),function(x){
                len=length(x)-1
                return(paste0(x[len],"-",len))
            }))
            # merge polygons
            tree <- tree[order(tree$V3),]
            mergedgrid <- unionSpatialPolygons(grid,tree$smallest)
            boundaries <- gBuffer(gBoundary(mergedgrid,byid = T),width=boundary_width,byid=T,joinStyle = "BEVEL")
            boundaries <- unionSpatialPolygons(boundaries,rep(year,length(boundaries)))
            boundaries <- gDifference(boundaries,grid_bounds)
            # plot(boundaries,add=T,col='black')
            # assign(paste0("b_",year),boundaries)
            count <- count+gOverlaps(boundaries,grid,byid=T)
        }
    }
    count <- count/180*10
    par(mar=c(1,0,4,1))
    plot(coast,col="grey",border="transparent",xlim=xlim,ylim=ylim,axes=FALSE,xlab="",ylab="")
    plot(grid,col=heat.colors(11)[10-count],border="transparent",add=T)
    plot(coast,col="transparent",border="darkgrey",xlim=xlim,ylim=ylim,axes=FALSE,xlab="",ylab="",add=TRUE)
#     title(paste0("PLD = ",pld))
#     if(pld==8) mtext(t,2,-1.5)
}
dev.off()
