source("funciones_para_MEA.R")
library(dplyr)
library(ggplot2)
library(knitr)

zonas <- c("cabeza","torso","piernas")
rois <- c(paste(zonas,"paciente",sep="_"),paste(zonas,"terapeuta",sep="_"))
bitacora <- read.csv("bitacora_sesiones_iniciales.csv",encoding = "UTF-8")
minutos_particion <- 3


completa <- list()

for (v in 1:length(bitacora$file)){
  video <- paste(bitacora$file[v],".txt",sep="")
  first_cut <- (bitacora$sampling_rate[v])*(bitacora$corte_inicial[v])
  last_cut <- (bitacora$sampling_rate[v])*(bitacora$corte_final[v])
  x <- arma_base(video,first_cut,last_cut)
  nombre <- substr(video,1,nchar(video)-4)
  path <- paste("bases_videos_csv/",nombre,".csv",sep = "")
  write.csv(x,path,row.names = FALSE)
  completa[[bitacora$file[v]]] <- x
}

lista_seleccionados<-data.frame(matrix(ncol = 12,nrow = 0))

for (video in names(completa)){
  print(video)
  temp <- crear_lista(partition_data(completa[[video]]),
                      nombre = video,umbral_max = 0.7,umbral_mean = 0.5)
  names(lista_seleccionados)<-names(temp)
  lista_seleccionados<-rbind(lista_seleccionados,temp)
}

lista_seleccionados<-cbind(id=1:dim(lista_seleccionados)[1],lista_seleccionados)
lista_seleccionados<-filter(lista_seleccionados,abs(lag_spearman_max)>0.025)

write.csv(lista_seleccionados,"lista_seleccionados.csv",row.names = FALSE)
