#-----------------------------------------------------
# funcion arma_base toma un txt del mea, quita las columnas vacias y pone los nombres de los rois
# y recorta los renglones del principio y el final que no corresponden a la sesion. 

arma_base<-function(video,cut_above = 0,cut_below = 0){
    path <- paste("txts/",as.character(video),sep = "")
    x <- read.delim(path,header = FALSE, sep = " ")
    if (dim(x)[2]==1){
        x <- read.delim(path,header = FALSE, sep ="\t")   
    }
    x <-  select(x,1,2,3,5,6,7)
    t <- 1:dim(x)[1]
    x <- cbind(t,x)
    ca <- cut_above + 1
    cb <- dim(x)[1]-cut_below
    x <- x[ca:cb, ]
    names(x)<-c("tiempo",rois)
    return(x)
}

#-----------------------------------------------------
#   funcion partition_data para separar los datos del video en bloques de la longitud de tiempo dada
#   pasos debe ser entero positivo, de preferencia divisor del largo de los bloques

partition_data <- function(datos,minutos=5,sr=30,pasos=1){
  partes <- list()
  largo_bloques <- minutos*sr*60
  num_partes <- pasos*floor(nrow(datos)/largo_bloques)
  for (i in 1:num_partes){
        inicia <- (i-1)*floor(largo_bloques/pasos)+1
        termina <- inicia + largo_bloques -1
        partes[[i]] <- datos[inicia:termina, ]
  }
  return(partes)
}

#-----------------------------------------------------
# funcion generica para suavizar datos, promediandolos

suavizar <- function(listado,n=7){
  aux<-listado
  for (j in n:length(listado)){
    aux[j] <- mean(listado[(j-n+1):j],na.rm = TRUE)
  }
  for (j in 1:(n-1)){
    aux[j] <- mean(listado[j:(j+n-1)],na.rm = TRUE)
  }
  return(aux)
}

#----------------- funcion para detectar lapsos con valores altos. Dada una lista en que cada entrada es
# un vector de datos numericos, devuelve los indices que superan el cuantil q dado-------------

detecta_altos <- function(lista_datos,q=0.9){
      promedios <- numeric()
      for (i in 1:length(lista_datos)){
          promedios <- c(promedios,mean(lista_datos[[i]],na.rm=TRUE)) 
      }
      altos <- which(promedios>quantile(promedios,probs=q))
      return(altos)
}


#------------------------------
# dado una particion de uno de los videos, selecciona los que sean relevantes por tener una correlacion 
#cruzada alta,  ya sea en promedio o en maximo. Devuelve lista con informacion de partes seleccionadas. 

crear_lista <- function(video_partido, nombre=NA, sr=30, segundos = 300, 
                        umbral_max=0.7,umbral_mean=0.5,lag_mayor = 90){
    selectos <- data.frame(matrix(ncol = 10,nrow = 0))  
    colnames(selectos) <- c("video","zona","num_periodo","minuto_inicio","minuto_final","acf_max",
                            "acf_promedio","lag_acf_max","spearman_max","lag_spearman_max")

    for (i in 1:length(video_partido)){
          for (j in zonas){
              temp <- video_partido[[i]][ ,grep(j,names(video_partido[[i]]))]
              suave<-data.frame(suavizar(temp[ ,1],30),suavizar(temp[ ,2],30))
              names(suave)<-names(temp)
              temp<-suave
              rm(suave)
              if(sum(abs(temp[1]))>1000 && sum(abs(temp[2]))>1000){
                    #print(c(i,j))
                    cecefe <- ccf(temp[1],temp[2],plot = FALSE,lag.max = 150,method="pearson")  
                    cecefe_s <- ccf(rank(temp[1]),rank(temp[2]),plot = FALSE,lag.max = 150,method="spearman")  
                    if (max(abs(cecefe_s$acf)>umbral_max) | mean(cecefe_s$acf)>umbral_mean){
                        cecefe <- ccf(temp[1],temp[2],lag.max = 90,plot=FALSE)
                        cecefe_s<-ccf(rank(temp[1]),rank(temp[2]),lag.max = 150,plot=FALSE,method="spearman")
                        #print(c("maximo",max(cecefe$acf),"promedio",mean(cecefe$acf)))
                        #print(c("maximo spearman",max(cecefe_s$acf),"promedio spearman",mean(cecefe_s$acf)))
                        y<-cecefe$acf[ ,1,1]
                        x<-which(y==max(y))
                        y2<-cecefe_s$acf[ ,1,1]
                        x2<-which(y2==max(y2))
                        aux <- data.frame(matrix(ncol=ncol(selectos),nrow=1))
                        names(aux)<-names(selectos)
                        aux$video = nombre
                        aux$zona = j
                        aux$num_periodo <- i
                        aux$minuto_inicio <- round(video_partido[[i]]$tiempo[1]/(sr*60),1)
                        aux$minuto_final <- aux$minuto_inicio + 5
                        aux$acf_max <- max(abs(cecefe$acf))
                        aux$acf_promedio <- mean(cecefe$acf)
                        aux$lag_acf_max <- round(cecefe$lag[x,1,1]/30,2)
                        aux$spearman_max <- max(abs(cecefe_s$acf))
                        aux$lag_spearman_max <- round(cecefe_s$lag[x2,1,1]/30,2)
                        selectos<-rbind(selectos,aux)
                        rm(x,y,x2,y2)
                    }              
              }
              else{
                  #print(paste(nombre,"parte",i,"ignorada por inmovilidad"))
              }
          } 
    }
    rm(cecefe,cecefe_s)
    return(selectos)
} 

#-------------------Crear la lista de periodos de medio minuto en que, para la roi que se indique, 
#   hay mucho movimiento, tanto del paciente como del terapeuta *donde mucho es que sea mayor al cuantil dado*     

crea_lista_movs <- function(vp,q=0.71,nombre_video=NA,sr=30){
          selectos <-data.frame(matrix(ncol = 12,nrow = 0))  
          colnames(selectos) <- c("video","zona","minuto_inicio","minuto_final",
                                  "mov_paciente","mov_terapeuta","mov_medio","acf_max",
                                  "lag_acf_max","spearman_max","lag_spearman_max","lidera")
          
          for (k in zonas){
              temp<-list()
              temp1<-list()
              temp2<-list()
              for (i in 1:length(vp)){
                  temp[[i]] <- vp[[i]][ ,grep(k,names(vp[[i]]))]
                  temp1[[i]] <- as.numeric(temp[[i]][ ,grep("paciente",names(temp[[i]]))])
                  temp2[[i]] <- as.numeric(temp[[i]][ ,grep("terapeuta",names(temp[[i]]))])
              }
              aux1 <- detecta_altos(temp1,q)
              aux2 <- detecta_altos(temp2,q)
              Aux<-intersect(aux1,aux2)
              for (j in Aux){
                  print(c(j,"j","zona",k))
                  renglon <- data.frame(matrix(ncol=ncol(selectos),nrow=1))
                  names(renglon)<-names(selectos)
                  renglon$video <- nombre_video
                  renglon$zona <- k
                  renglon$minuto_inicio <- round(vp[[j]]$tiempo[1]/(60*sr),1)
                  renglon$minuto_final <- round(vp[[j]]$tiempo[nrow(vp[[j]])]/(60*sr),1)
                  renglon$mov_paciente <- mean(temp1[[j]])
                  renglon$mov_terapeuta <- mean(temp2[[j]])
                  renglon<-mutate(renglon,mov_medio=mean(mov_paciente,mov_terapeuta))
                  cecefe <- ccf(temp1[[j]],temp2[[j]],lag.max = 150,plot=FALSE)
                  cecefe_s <- ccf(rank(temp1[[j]]),rank(temp2[[j]]),lag.max = 150,plot=FALSE)
                  print(c("maximo",max(cecefe$acf),"promedio",mean(cecefe$acf)))
                  y<-cecefe$acf[ ,1,1]
                  x<-which(y==max(y))
                  y2<-cecefe_s$acf[ ,1,1]
                  x2<-which(y2==max(y2))
                  renglon$acf_max <- y[x]
                  renglon$lag_acf_max <- round(cecefe$lag[x,1,1]/sr,2)
                  renglon$spearman_max <- y2[x2]
                  renglon$lag_spearman_max <- round(cecefe$lag[x2,1,1]/sr,2)
                  renglon$lidera<-"-"
                  if (renglon$lag_acf_max>0.1 ){
                      renglon$lidera="terapeuta"
                  }
                  if (renglon$lag_acf_max< -0.1 ){
                     renglon$lidera="paciente"
                  }
                  selectos<-rbind(selectos,renglon)
              }
          }          
          return(selectos)
}        

#--------------------FUNCIONES PARA GENERAR GRAFICAS A PARTIR DE LA LISTA DE SELECCIONADOS -----------------

corta_lista <- function(id,ls=lista_seleccionados){
      x <- which(ls$id==id)
      aux <- partition_data(completa[[ ls$video[x] ]])[[ ls$num_periodo[x] ]]
      aux <- aux[ ,grep(ls$zona[x],names(aux))]
      aux$suave_paciente <- suavizar(aux[ ,1],30)
      aux$suave_terapeuta <- suavizar(aux[ ,2],30)
      aux$tiempo <- (1:dim(aux)[1]/(60*30))+ls$minuto_inicio[x]
      temp <- list()
      temp[[1]]<-aux
      temp[["video"]]<-paste("Video: ",ls$video[x])
      temp[["minutos"]]<-paste("De",as.character(ls$minuto_inicio[x]),"a",
                               as.character(ls$minuto_final[x]),"min.")
      
      return(temp)
}

comparativa_visual <- function(id,min_ini=0,min_fin=100,que_lista=lista_seleccionados){
      temp <- corta_lista(id,ls=que_lista)[[1]]
      temp2 <- corta_lista(id,ls=que_lista)
      g<-ggplot()+geom_line(data=filter(temp,tiempo>min_ini,tiempo<min_fin),aes(tiempo,suave_paciente),
          color='blue',size=0.5)+geom_line(data=filter(temp,tiempo>min_ini,tiempo<min_fin),
          aes(tiempo,suave_terapeuta),color='brown',size=0.5)+
          ggtitle(temp2[["video"]])+xlab("minuto en el video")+
          ylab("Azul: paciente. Rojo: terapeuta. (Promedios por segundo)")
      show(g)
      return(g)
      }

corr_cruzadas <- function(id,que_lista=lista_seleccionados){
      temp <- corta_lista(id,ls=que_lista)[[1]]
      cecefe <- ccf(temp$suave_paciente,temp$suave_terapeuta,plot = FALSE,lag.max = 57)
      aux <- data.frame(segundos=cecefe$lag[ ,1,1]/19,correlacion=cecefe$acf[,1,1])
      temp2 <- corta_lista(id,ls=que_lista)
      g <- ggplot(aux,aes(segundos,correlacion))+geom_col(color='orange',fill='purple')+
            ggtitle(paste(temp2[["video"]],temp2[["minutos"]],sep = "                 "))+
            xlab("lidera paciente       <--   segundos de desface   -->       lidera terapeuta")+
            ylab("correlaciones cruzadas")
      
      show(g)
      return(g)
}

#--------------------FUNCIONES PARA GENERAR GRAFICAS A PARTIR DE LA LISTA DE MOVIMIENTO ALTO -------
