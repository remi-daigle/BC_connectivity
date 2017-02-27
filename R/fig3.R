#### Figure 3 ####
png("./figures/fig3.png",width=18,height=18,units="cm",res=500)
# prepare data for diffusion
all_summary <- all %>% group_by(site,year,pld) %>% summarize(inseadist=mean(mean_inseadist/1000),
                                                             inseadistSD=sd(mean_inseadist/1000),
                                                             nodes=mean(n_nodes),
                                                             nodesSD=sd(n_nodes))
all_summary$trans_dist <- all_summary$inseadist^2
all_summary$climate <- "Present"
all_summary$climate[all_summary$year>2020] <- "Future"

# calculate slope
coefs <- data.frame(all_summary %>% group_by(climate,year,site) %>% 
                        do(slopes=coef(lm(trans_dist~0+pld,data=.))))
coefs2 <- coefs %>% group_by(climate,site) %>% 
    summarize(slopemean=mean(as.numeric(slopes)),
              slopesd=sd(as.numeric(slopes)))


# prepare for plotting
layout(matrix(c(1:10),ncol=5,byrow = TRUE),width=c(2,5,5,5,2))
par(mar=c(1,1,4,1))

# plot diffusion
zmax <- max(coefs2$slopemean)
colors <- rev(heat.colors(zmax))
image(matrix(c(0:zmax),nrow=1),col=colors,axes=F)
text(0,c(1:9)/10,round(c(1:9)/10*zmax))
box(bty="o")

for(t in c("Future","Present")){
    colors <- rev(heat.colors(max(coefs2$slopemean)))
    plot(coast,col="grey",border="darkgrey",xlim=xlim,ylim=ylim,axes=FALSE,xlab="",ylab="")
    
    for(s in sapply(slot(grid, "polygons"), function(x) slot(x, "ID"))){
        plot(x <- grid[sapply(slot(grid, "polygons"), function(x) slot(x, "ID"))==s,],add=T,
             xlim=xlim,ylim=ylim,border=NA,
             col=colors[coefs2$slopemean[coefs2$climate==t&coefs2$site==s]])
    }
    title(paste(t,"Diffusion"))
    plot(coast,col="transparent",border="darkgrey",xlim=xlim,ylim=ylim,axes=FALSE,xlab="",ylab="",add=T)
}

# plot difference in diffusion
zmax <- max(coefs2$slopemean[coefs2$climate=="Future"]-coefs2$slopemean[coefs2$climate=="Present"])
zmin <- min(coefs2$slopemean[coefs2$climate=="Future"]-coefs2$slopemean[coefs2$climate=="Present"])
colorspos <- colorRampPalette(c("white","blue"))(zmax)
colorsneg <- colorRampPalette(c("white","yellow"))(abs(zmin))

plot(coast,col="grey",border="transparent",xlim=xlim,ylim=ylim,axes=FALSE,xlab="",ylab="")

for(s in seq_along(unique(coefs2$site))){
    diff <- coefs2$slopemean[coefs2$climate=="Future"&coefs2$site==s]-coefs2$slopemean[coefs2$climate=="Present"&coefs2$site==s]
    if(diff>=0){
        col=colorspos[diff]
    } else {
        col=colorsneg[abs(diff)]
    }
    plot(grid[s,],add=T,
         xlim=xlim,ylim=ylim,border=NA,
         col=col
    )
}

plot(coast,col="transparent",border="darkgrey",xlim=xlim,ylim=ylim,axes=FALSE,xlab="",ylab="",add=T)

title("Difference in Diffusion")
image(matrix(c(0:zmax),nrow=1),col=c(rev(colorsneg),colorspos),axes=F)
text(0,c(1:9)/10,round(seq.int(zmin,zmax,length.out = 11)[2:10]))
box(bty="o")

# prepare data for connectivity
all_summary$trans_nodes <- all_summary$nodes^2
all_summary$climate <- "Present"
all_summary$climate[all_summary$year>2020] <- "Future"
coefs <- data.frame(all_summary %>% group_by(climate,year,site) %>% 
                        do(slopes=coef(lm(trans_nodes~0+pld,data=.))))
coefs2 <- coefs %>% group_by(climate,site) %>% 
    summarize(slopemean=mean(as.numeric(slopes)),
              slopesd=sd(as.numeric(slopes)))

# plot connectivity
zmax <- max(coefs2$slopemean)
colors <- rev(heat.colors(zmax))
image(matrix(c(0:zmax),nrow=1),col=colors,axes=F)
text(0,c(1:9)/10,round(c(1:9)/10*zmax))
box(bty="o")
for(t in c("Future","Present")){
    plot(coast,col="grey",border="darkgrey",xlim=xlim,ylim=ylim,axes=FALSE,xlab="",ylab="")
    
    for(s in sapply(slot(grid, "polygons"), function(x) slot(x, "ID"))){
        plot(x <- grid[sapply(slot(grid, "polygons"), function(x) slot(x, "ID"))==s,],add=T,
             xlim=xlim,ylim=ylim,border=NA,
             col=colors[coefs2$slopemean[coefs2$climate==t&coefs2$site==s]])
    }
    title(paste(t,"Connectivity"))
    plot(coast,col="transparent",border="darkgrey",xlim=xlim,ylim=ylim,axes=FALSE,xlab="",ylab="",add=T)
    
}

# plot difference in connectivity
zmax <- max(coefs2$slopemean[coefs2$climate=="Future"]-coefs2$slopemean[coefs2$climate=="Present"])
zmin <- min(coefs2$slopemean[coefs2$climate=="Future"]-coefs2$slopemean[coefs2$climate=="Present"])
colorspos <- colorRampPalette(c("white","blue"))(zmax)
colorsneg <- colorRampPalette(c("white","yellow"))(abs(zmin))

plot(coast,col="grey",border="transparent",xlim=xlim,ylim=ylim,axes=FALSE,xlab="",ylab="")

for(s in seq_along(unique(coefs2$site))){
    diff <- coefs2$slopemean[coefs2$climate=="Future"&coefs2$site==s]-coefs2$slopemean[coefs2$climate=="Present"&coefs2$site==s]
    if(diff>=0){
        col=colorspos[diff]
    } else {
        col=colorsneg[abs(diff)]
    }
    plot(grid[s,],add=T,
         xlim=xlim,ylim=ylim,border=NA,
         col=col
    )
}

plot(coast,col="transparent",border="darkgrey",xlim=xlim,ylim=ylim,axes=FALSE,xlab="",ylab="",add=T)
title("Difference in Connectivity")

image(matrix(c(0:zmax),nrow=1),col=c(rev(colorsneg),colorspos),axes=F)
text(0,c(1:9)/10,round(seq.int(zmin,zmax,length.out = 11)[2:10]))
box(bty="o")


dev.off()
