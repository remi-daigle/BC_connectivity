# uncomment to use, very time consuming
# #!/usr/bin/env Rscript
# require(data.table)
# require(dplyr)
# require(readr)
# 
# ### Choose PLD ###
# for(pld in seq(1,120,7)){
# 
#     #### load original positions ####
#     rlG <- read_csv("release_locations_Grid.txt", col_names=FALSE)
#     memory.limit(32000)
#     #### loop for each year #####
#     # this is where the raw data from figshare was decompressed
#     mypath='H:/MPA_particles/output'
#     year_dir <- list.dirs(mypath,recursive=F)
#     if(pld==1) year_dir <- year_dir[c(5:10)]
#     year_dir <- year_dir[c(23)]
#     for(mypath_y in year_dir){
#         type=substr(mypath_y,25,25)
#         print(paste(pld,substr(mypath_y,26,30)))
#         #### tabulate output ####
#         filenames <- list.files(path=mypath_y, pattern=glob2rx(paste0("*para    1",formatC(pld+1, width = 3, format = "d", flag = "0"),"*")), full.names=TRUE,recursive=T)
#         datalist <- lapply( filenames, read_csv, col_names = F )
#         dataset <- rbindlist(datalist)
#         setnames(dataset,names(dataset),c("long","lat","Z","Out","site"))
#         dataset$filename <- rep(filenames,do.call(rbind, lapply(datalist, function(x) dim(x)[1])))
#         
#         x <- matrix(unlist(sapply(dataset$filename,strsplit,"/")),nrow=length(dataset$filename),ncol=7,byrow=T)
#         dataset$type <- substr(x[,4],1,1)
#         dataset$year <- as.numeric(substr(x[,4],2,5))
#         dataset$rday <- as.numeric(x[,5])
#         dataset$bin <- as.numeric(x[,6])
#         dataset$time <- as.numeric(substr(x[,7],10,12))
#         rm(x)
# 
#         
#         #### link release to G output ####
#         if(type=="G"|type=="S"){
#             for(i in 1:max(dataset$bin)){
#                 x=(length(dataset$type[dataset$type=="G"&dataset$bin==i|dataset$type=="S"&dataset$bin==i])/length(rlG$X1[ceiling(rlG$X5/25)==i]))
#                 y <- dataset$type=="G"&dataset$bin==i|dataset$type=="S"&dataset$bin==i
#                 dataset$long0[y] <- rep(rlG$X1[ceiling(rlG$X5/25)==i],x)
#                 dataset$lat0[y] <- rep(rlG$X2[ceiling(rlG$X5/25)==i],x)
#                 dataset$Z0[y] <- rep(rlG$X3[ceiling(rlG$X5/25)==i],x)
#                 dataset$delay[y] <- rep(rlG$X4[ceiling(rlG$X5/25)==i],x)
#                 dataset$Site[y] <- rep(rlG$X5[ceiling(rlG$X5/25)==i],x)
#             }
#             write.csv(dataset,paste0(type,"_data_",dataset$year[1],"_pld_",pld,".csv"))
#                       
#         }
#         
#     }
# }