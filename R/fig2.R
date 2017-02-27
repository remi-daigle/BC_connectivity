#### Figure 2 ####
all_summary <- all %>% group_by(year,pld) %>% summarize(inseadist=mean(mean_inseadist),
                                                        inseadistSD=sd(mean_inseadist),
                                                        nodes=mean(n_nodes),
                                                        nodesSD=sd(n_nodes))
#panel A

all_summary$climate <- "Present"
all_summary$climate[all_summary$year>2020] <- "Future"

as.numeric(coef(lm(inseadist~0+sqrt(pld),data=all_summary)))
coefs <- data.frame(all_summary %>% group_by(climate) %>% 
                        do(#Intercept=coef(lm(inseadist~sqrt(pld),data=.))[1],
                            slopes=coef(lm(inseadist~0+sqrt(pld),data=.))#[2]
                        ) %>% 
                        ungroup()
)
coefs$Intercept <- 0 #as.numeric(coefs$Intercept)
coefs$slopes <- as.numeric(coefs$slopes)

# add temperature corrected PLD according to O'COnnor et al. 2007
coefs <- rbind(coefs,coefs[1,])
coefs$climate[3] <- "Future PLD-corrected"

bestfitlines <- expand.grid(pld=unique(all_summary$pld),climate=coefs$climate,stringsAsFactors = FALSE)
bestfitlines <- left_join(bestfitlines,coefs,by="climate")

Tc=15
pldkey <- data.frame(T=seq(1.5,100,0.0001)) %>% 
    mutate(PLD=exp(3.17)*(T/Tc)^(-1.40-0.27*log(T/Tc)))

tempequi <- pldkey$T[sapply(bestfitlines$pld[bestfitlines$climate=="Future PLD-corrected"],function(x) which.min(abs(pldkey$PLD-x)))]+1

bestfitlines$pld[bestfitlines$climate=="Future PLD-corrected"] <- pldkey$PLD[sapply(tempequi,function(x) which.min(abs(pldkey$T-x)))]

bestfitlines$inseadist <- bestfitlines$Intercept+bestfitlines$slopes*sqrt(bestfitlines$pld)

# swap 'normal'  pld's so plotting is more straightforward
bestfitlines$pld[bestfitlines$climate=="Future PLD-corrected"] <- bestfitlines$pld[bestfitlines$climate=="Future"]

#### plot panel A ####
pA <- ggplot(all_summary,aes(x=pld,y=inseadist/1000,color=climate))+
    geom_point(size=3)+
    geom_smooth(method="lm",formula=y~0+sqrt(x))+
    geom_line(mapping=aes(x=pld,y=inseadist/1000),data=bestfitlines[bestfitlines$climate=="Future PLD-corrected",])+
    theme_classic()+ 
    theme(axis.line.x = element_line(colour = "grey50"),axis.line.y = element_line(colour = "grey50"))+
    labs(x="PLD (days)", y="Mean Dispersal Distance (km)")+
    scale_colour_manual(name = "Climate Scenario",values = c("#66c2a5", "#8da0cb", "#fc8d62"))+
    guides(colour=guide_legend(override.aes = list(
        fill = c("#66c2a5", "transparent", "#fc8d62"),
        shape = c(16,NA,16))))

#panel B
coef(lm(nodes~0+sqrt(pld),data=all_summary))
coefs <- data.frame(all_summary %>% group_by(climate) %>% 
                        do(#Intercept=coef(lm(nodes~sqrt(pld),data=.))[1],
                            slopes=coef(lm(nodes~0+sqrt(pld),data=.))#[2]
                        ) %>% 
                        ungroup()
)
coefs$Intercept <- 0 #as.numeric(coefs$Intercept)
coefs$slopes <- as.numeric(coefs$slopes)

# add temperature corrected PLD according to O'COnnor et al. 2007
coefs <- rbind(coefs,coefs[1,])
coefs$climate[3] <- "Future PLD-corrected"

bestfitlines <- expand.grid(pld=unique(all_summary$pld),climate=coefs$climate,stringsAsFactors = FALSE)
bestfitlines <- left_join(bestfitlines,coefs,by="climate")

tempequi <- pldkey$T[sapply(bestfitlines$pld[bestfitlines$climate=="Future PLD-corrected"],function(x) which.min(abs(pldkey$PLD-x)))]+1

bestfitlines$pld[bestfitlines$climate=="Future PLD-corrected"] <- pldkey$PLD[sapply(tempequi,function(x) which.min(abs(pldkey$T-x)))]

bestfitlines$nodes <- bestfitlines$Intercept+bestfitlines$slopes*sqrt(bestfitlines$pld)

# swap 'normal'  pld's so plotting is more straightforward
bestfitlines$pld[bestfitlines$climate=="Future PLD-corrected"] <- bestfitlines$pld[bestfitlines$climate=="Future"]

#### plot panel B ####

pB <- ggplot(all_summary,aes(x=pld,y=nodes,color=climate))+
    geom_point(size=3)+
    geom_smooth(method="lm",formula=y~0+sqrt(x))+
    geom_line(mapping=aes(x=pld,y=nodes),data=bestfitlines[bestfitlines$climate=="Future PLD-corrected",])+
    theme_classic()+ 
    theme(axis.line.x = element_line(colour = "grey50"),axis.line.y = element_line(colour = "grey50"))+
    labs(x="PLD (days)", y="Mean Connected Nodes")+
    scale_colour_manual(name = "Climate Scenario",values = c("#66c2a5", "#8da0cb", "#fc8d62"))+
    guides(colour=guide_legend(override.aes = list(
        fill = c("#66c2a5", "transparent", "#fc8d62"),
        shape = c(16,NA,16))))


png("./figures/fig2.png",width=18,height=18,units="cm",res=500)
grid.arrange(pA,pB)
dev.off()
