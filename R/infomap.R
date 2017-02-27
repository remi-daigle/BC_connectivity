#### IMPORTANT TO DO: ####
# install Infomap (http://www.mapequation.org/code.html#Installation)
# change path below to appropriate directory
infomap_wd <- "C:/Users/Remi-Work/Documents/Infomap/"
old_wd <- getwd()
setwd(infomap_wd)

for(t in c("Future","Present",c(1998:2007),c(2098:2107))){
    for(pld in seq(1,120,7)){
        if(is.na(as.numeric(t))){
            con_mat <- get_matrix_fun(con_mat_data,con_mat_data_legend,time=t,PLD=pld,FUN='mean')
        } else {
            con_mat <- get_matrix_fun(con_mat_data,con_mat_data_legend,year=t,PLD=pld,FUN='mean')
        }
        

        con_link <- expand.grid(seq_along(row.names(con_mat)),seq_along(row.names(con_mat)))
        names(con_link) <- c("source","settle")
        con_link$conn <- apply( con_link, 1, function(x,con_mat) con_mat[ x[1], x[2] ], con_mat=con_mat )
        write.table(con_link,paste0(infomap_wd,"G",t,"_pld_",pld,".net"),sep=" ",col.names=F,row.names=F)
        

        system(paste0(" ./Infomap G",t,"_pld_",pld,".net test --clu --bftree --tree --map -i 'link-list'"))
        # unlink(paste0(infomap_wd,"G",t,"_pld_",pld,".net"))
    }
}
setwd(old_wd)
