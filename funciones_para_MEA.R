#-----------------------------------------------------
# funcion arma_base toma un txt del mea, quita las columnas vacias y pone los nombres de los rois
# y recorta los renglones del principio y el final que no corresponden a la sesion. 

arma_base<-function(video,cut_above = 0,cut_below = 0){
  path <- paste("txts/",as.character(video),sep = "")
  x <- read.delim(path,header = FALSE, sep = " ")%>%
    select(1,2,3,5,6,7)
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


crear_lista <- function(video_partido, umbral_max=0.7,umbral_mean=0.5,lag_mayor = 57){
  for (i in 1:length(video_partido)){
      print (i)    
      for (j in zonas){
          print(j)
          temp <- video_partido[[i]][ ,grep(j,names(video_partido[[i]]))]
          cecefe <- ccf(temp[1],temp[2],plot = FALSE)
          if (max(abs(cecefe$acf))>umbral_max){
                print(c("maximo",max(cecefe$acf),"promedio",mean(cecefe$acf)))
                rm(cecefe)
          }      
    }
  }
} 
