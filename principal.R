library(dplyr)
rois <- c("cabeza_paciente","torso_paciente","piernas_paciente",
          "cabeza_terapeuta","torso_terapeuta","piernas_terapeuta")

bitacora <- read.csv("bitacora_mea.csv")

completa <- list()

for (v in 1){
  video <- bitacora$file[v]
  print(video)
  first_cut <- (bitacora$sampling_rate[v])*(bitacora$corte_inicial[v])
  last_cut <- (bitacora$sampling_rate[v])*(bitacora$corte_final[v])
  x <- arma_base(video,first_cut,last_cut)
  nombre <- substr(video,1,nchar(video)-4)
  path <- paste("bases_videos_csv/",nombre,".csv",sep = "")
  write.csv(x,path,row.names = FALSE)
  completa[[video]] <- x
}

rm(x,v,path,nombre,first_cut,last_cut)