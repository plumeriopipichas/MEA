bitacora <- read.csv("bitacora_aumentada.csv")
lista_mov_altos <- read.csv("lista_mov_altos.csv")

bitacora<-mutate(bitacora,segmentos_30_seg=round(num_frames/900,0))

segmentos <- list()
cuantos_mva <- integer()
cuantos_lp <- integer()
cuantos_lt <- integer()
high_positive_corr <- integer()
high_negative_corr <- integer()

clases <- unique(bitacora$clasificacion)

for (clase in clases){
    print(clase)
    segmentos[[clase]]<-sum(filter(bitacora,clasificacion==clase)$segmentos_30_seg)
    x<-length(which(lista_mov_altos$clasificacion==clase))
    cuantos_mva <- c(cuantos_mva,x)
    x <- which(lista_mov_altos$lidera=='paciente')
    lista_mov_altos_lp <- lista_mov_altos[x, ]
    lista_mov_altos_lt <- lista_mov_altos[-x, ]
    cuantos_lp <- c(cuantos_lp,length(which(lista_mov_altos_lp$clasificacion==clase))) 
    cuantos_lt <- c(cuantos_lt,length(which(lista_mov_altos_lt$clasificacion==clase))) 
    
    x <- length(which(lista_mov_altos$clasificacion==clase & 
                        lista_mov_altos$spearman_max > 0.5))
    high_positive_corr <- c(high_positive_corr,x)
    x <- length(which(lista_mov_altos$clasificacion==clase & 
                        lista_mov_altos$spearman_min < -0.5))
    high_negative_corr <- c(high_negative_corr,x)
}

summary_table <- data.frame(clase = names(segmentos), 
                    segmentos_bitacora = as.numeric(segmentos),
                    segmentos_mov_altos=cuantos_mva,cuantos_lp,cuantos_lt,
                    high_positive_corr,high_negative_corr)


write.csv(summary_table,"resumen_T_C_MEA.csv",row.names = FALSE)

rm(cuantos_mva,cuantos_lp,cuantos_lt,segmentos,x)