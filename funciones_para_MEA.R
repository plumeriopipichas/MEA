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
#funcion partition_data para separar los datos del video en bloques de la longitud de tiempo dada

partition_data <- function(datos,minutos=5,sr=19){
  partes <- list()
  largo_bloques <- minutos*sr*60
  num_partes <- floor(nrow(datos)/largo_bloques)
  for (i in 1:num_partes){
    partes[[i]] <- datos[((i-1)*largo_bloques+1):(i*largo_bloques), ]
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


#-----------
# dado una particion de uno de los videos, selecciona los que sean relevantes por tener una correlacion cruzada alta, 
# ya sea en promedio o en maximo. Devuelve lista con informacion de partes seleccionadas. EN CONSTRUCCION. 


crear_lista <- function(video_partido, nombre=NA, sr=19, segundos = 300, 
                        umbral_max=0.7,umbral_mean=0.5,lag_mayor = 57){
    selectos <- prueba<-data.frame(matrix(ncol = 8,nrow = 0))  
    colnames(selectos) <- c("video","zona","num_periodo","minuto_inicio","minuto_final","acf_maxima",
                            "acf_promedio","lag_acf_max")

    for (i in 1:length(video_partido)){
          for (j in zonas){
              temp <- video_partido[[i]][ ,grep(j,names(video_partido[[i]]))]
              suave<-data.frame(suavizar(temp[ ,1],19),suavizar(temp[ ,2],19))
              names(suave)<-names(temp)
              temp<-suave
              rm(suave)
              if(sum(abs(temp[1]))>1000 && sum(abs(temp[2]))>1000){
                    print(c(i,j))
                    cecefe <- ccf(temp[1],temp[2],plot = FALSE,lag.max = 57)  
                    if (max(abs(cecefe$acf)>umbral_max) | mean(cecefe$acf)>umbral_mean){
                        cecefe <- ccf(temp[1],temp[2],lag.max = 57)
                        print(c("maximo",max(cecefe$acf),"promedio",mean(cecefe$acf)))
                        y<-cecefe$acf[ ,1,1]
                        x<-which(y==max(y))
                        aux <- data.frame(matrix(ncol=ncol(selectos),nrow=1))
                        names(aux)<-names(selectos)
                        aux$video = nombre
                        aux$zona = j
                        aux$num_periodo <- i
                        aux$minuto_inicio <- round(video_partido[[i]]$tiempo[1]/(sr*60),1)
                        aux$minuto_final <- aux$minuto_inicio + 5
                        aux$acf_maxima <- max(abs(cecefe$acf))
                        aux$acf_promedio <- mean(cecefe$acf)
                        aux$lag_acf_max <- cecefe$lag[x,1,1]/19
                        selectos<-rbind(selectos,aux)
                        rm(x,y)
                    }              
              }
              else{
                  print(paste(nombre,"parte",i,"ignorada por inmovilidad"))
              }
          } 
    }
    rm(cecefe)
    return(selectos)
} 


#--------------------FUNCIONES PARA GENERAR GRAFICAS A PARTIR DE LA LISTA DE SELECCIONADOS -----------------

corta_lista <- function(id){
      x <- which(lista_seleccionados$id==id)
      temp <- partition_data(completa[[ lista_seleccionados$video[x] ]])[[ lista_seleccionados$num_periodo[x] ]]
      temp <- temp[ ,grep(lista_seleccionados$zona[x],names(temp))]
      temp$suave_paciente <- suavizar(temp[ ,1],19)
      temp$suave_terapeuta <- suavizar(temp[ ,2],19)
      temp$tiempo <- (1:dim(temp)[1]/(60*19))+lista_seleccionados$minuto_inicio[x]
      return(temp)
}

comparativa_visual <- function(id,min_ini=0,min_fin=100){
      temp <- corta_lista(id)
      g<-ggplot()+geom_line(data=filter(temp,tiempo>min_ini,tiempo<min_fin),aes(tiempo,suave_paciente),
          color='blue',size=0.5)+geom_line(data=filter(temp,tiempo>min_ini,tiempo<min_fin),
          aes(tiempo,suave_terapeuta),color='brown',size=0.5)
      show(g)
}

