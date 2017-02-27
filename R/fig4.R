#### Figure 4 ####
png(paste0("./figures/fig4.png"),width=18,height=18,units="cm",res=500)
layout(matrix(c(1:8),ncol=4,byrow = TRUE))
par(mar=c(1,0,4,1))
infomap_wd <- "../../Infomap/"

for(t in c("Future","Present")){
    for(pld in c(8,29,64,120)){
        con_mat <- get_matrix_fun(con_mat_data,con_mat_data_legend,time=t,PLD=pld,FUN='mean')
        
        tree <- read.table(paste0(infomap_wd,"test/G",t,"_pld_",pld,".tree"))
        tree$smallest <- unlist(lapply(strsplit(as.character(tree$V1),":"),function(x){
            # len=length(x)-1
            # return(paste0(x[len],"-",len))
            return(paste(x[seq(length(x)-1)],collapse=":"))
            
        }))
        
        colors <- rainbow(length(unique(tree$smallest)))
        
        plot(coast,col="grey",border="transparent",xlim=xlim,ylim=ylim,axes=FALSE,xlab="",ylab="")
        
        # merge polygons
        tree <- tree[order(tree$V3),]
        mergedgrid <- unionSpatialPolygons(grid,tree$smallest)
        plot(mergedgrid,col=colors,border="transparent",add=TRUE)
        plot(coast,col="transparent",border="darkgrey",xlim=xlim,ylim=ylim,axes=FALSE,xlab="",ylab="",add=TRUE)
        
        title(paste("PLD =",pld))
        if(pld==8) mtext(t,2,-1.5)
        
    }
}
dev.off()
