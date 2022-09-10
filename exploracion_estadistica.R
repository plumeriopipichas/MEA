bitacora <- read.csv("bitacora_aumentada.csv")
lista_mov_altos <- read.csv("lista_mov_altos.csv")

bitacora<-mutate(bitacora,segmentos_30_seg=round(num_frames/900,0))

segmentos <- list()
clases <- unique(bitacora$clasificacion)
for (clase in clases){
    print(clase)
    segmentos[[clase]]<-sum(filter(bitacora,clasificacion==clase)$segmentos_30_seg)   
}

summary_table <- data.frame(clase = names(segmentos), segmentos = as.numeric(segmentos))
