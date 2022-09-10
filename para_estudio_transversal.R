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

temp<-numeric()
for(j in names(completa)){
    print(j)
    temp<-c(temp,nrow(completa[[j]]))
}
print(length(temp))
bitacora$num_frames<-temp

write.csv(bitacora,"bitacora_aumentada.csv",row.names = FALSE)

# lista_seleccionados<-data.frame(matrix(ncol = 12,nrow = 0))
# 
# for (video in names(completa)){
#   print(video)
#   temp <- crear_lista(partition_data(completa[[video]]),
#                       nombre = video,umbral_max = 0.7,umbral_mean = 0.5,
#                       segundos = 60*minutos_particion)
#   names(lista_seleccionados)<-names(temp)
#   lista_seleccionados<-rbind(lista_seleccionados,temp)
# }
# 
# lista_seleccionados<-cbind(id=1:dim(lista_seleccionados)[1],lista_seleccionados)
# lista_seleccionados<-filter(lista_seleccionados,abs(lag_spearman_max)>0.025)
# 
# write.csv(lista_seleccionados,"lista_seleccionados.csv",row.names = FALSE)
# 

#segunda

d = 0.5
quantile = 0.85

lista_mov_altos <- data.frame(matrix(ncol = 15,nrow = 0))  

for (v in names(completa)){
  lista_mov_altos <- rbind(lista_mov_altos,
                           crea_lista_movs(partition_data(completa[[v]],minutos=d),
                                           nombre_video = v,q=quantile))    
}

lista_mov_altos<-filter(lista_mov_altos,abs(lag_spearman_max)>0.1)

write.csv(lista_mov_altos,"lista_mov_altos.csv",row.names = FALSE)

ids <- data.frame(id=1:nrow(lista_mov_altos))
lista_mov_altos<-cbind(ids,lista_mov_altos)

lista_mov_altos$se_mueve_mas = "-"

for (k in 1:nrow(lista_mov_altos)){
  if (lista_mov_altos$mov_paciente[k]>1.1*(lista_mov_altos$mov_terapeuta[k])){
    lista_mov_altos$se_mueve_mas[k]="paciente"
  }    
  if (lista_mov_altos$mov_paciente[k]< 0.9*(lista_mov_altos$mov_terapeuta[k])){
    lista_mov_altos$se_mueve_mas[k]="terapeuta"    
  }
}



rm(v,path,nombre,video,temp,j,k,d)