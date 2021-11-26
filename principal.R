#------------------preparativos----------------------

library(dplyr)

zonas <- c("cabeza","torso","piernas")
rois <- c(paste(zonas,"paciente",sep="_"),paste(zonas,"terapeuta",sep="_"))
bitacora <- read.csv("bitacora_mea.csv")
minutos_particion <- 5

#----------Crear las bases de roi para cada uno de los videos. Cortando las partes inicial y final ------------
# completa[[v]] es la base correspondiente al video v

completa <- list()

for (v in 1:4){
  video <- paste(bitacora$file[v],".txt",sep="")
  print(video)
  first_cut <- (bitacora$sampling_rate[v])*(bitacora$corte_inicial[v])
  last_cut <- (bitacora$sampling_rate[v])*(bitacora$corte_final[v])
  x <- arma_base(video,first_cut,last_cut)
  nombre <- substr(video,1,nchar(video)-4)
  path <- paste("bases_videos_csv/",nombre,".csv",sep = "")
  write.csv(x,path,row.names = FALSE)
  completa[[bitacora$file[v]]] <- x
}

#----------- En cada particion de cada video, revisar si pasa los umbrales del criterio, y en caso afirmativo 
# adjuntarlo a la lista de partes seleccionadas ---------------------------






rm(x,v,path,nombre,first_cut,last_cut,video)