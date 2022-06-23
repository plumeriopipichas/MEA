source("~/Documentos/Chamba/MEA/funciones_para_MEA.R") #cargamos las funciones 
library(dplyr)

lista_mov_altos <- data.frame(matrix(ncol = 7,nrow = 0))  
colnames(lista_mov_altos) <- c("video","zona","minuto_inicio","minuto_final",
                        "mov_paciente","mov_terapeuta","mov_medio")

for (v in names(completa)){
    lista_mov_altos <- rbind(lista_mov_altos,
              crea_lista_movs(partition_data(completa[[v]],minutos=0.5),nombre_video = v,q=0.85))    
}

lista_mov_altos<-filter(lista_mov_altos,abs(lag_spearman_max)>0.1)
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

write.csv(lista_mov_altos,"lista_mov_altos.csv",row.names = FALSE)

rm(ids,v)