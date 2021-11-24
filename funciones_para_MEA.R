
# funcion arma_base toma un txt del mea, quita las columnas vacias y pone los nombres de los rois
# y recorta los renglones del principio y el final que no corresponden a la sesion. 

arma_base<-function(video,cut_above = 0,cut_below = 0){
  path <- paste("txts/",as.character(video),sep = "")
  x <- read.delim(path,header = FALSE, sep = " ")%>%
    select(1,2,3,5,6,7)
  ca <- cut_above + 1
  cb <- dim(x)[1]-cut_below
  x <- x[ca:cb, ]
  t <- 1:dim(x)[1]
  x <- cbind(t,x)
  names(x)<-c("tiempo",rois)
  return(x)
}
