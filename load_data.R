#########################################################################################
#Objetivo: Cargar series temporales de NDVI de imagenes satelitales para calcular Xh y Xw
#########################################################################################

####librerias####
library(foreign)

####Configurar archivos de entrada####
#Configurar el espacio de trabajo donde esta los archivos dbf
#Ubicacion de la tabla con todos los datos
setwd("C:/Users/Administrador/Google Drive/Vazquez/dbf")

#Lista los archivos que son de formato DBF (*.dbf)
lista=list.files(pattern="*.dbf")

#Cargar cada archivo y correr calculos#
for (file in lista){
data <- read.dbf(file)
transformacion=10000 #Especificar un escalar seg?n el tipo de imagen
data=data/transformacion #_Generar los datos transformados_#

#Direccionar donde se encuentra el archivo de la funci?n de STD
source("C:/Users/Administrador/Google Drive/Vazquez/STL/calc_desc_ts.R") 

####Proceso para correr en paralelo####
#Especificar el numero de procesadores para realizar en paralelo
rm(res)
procesadores=3

#_Proceso para correr en paralelo_#
paral_desc_ts=function(i){
  divs=floor(dim(data)[1]/procesadores)
  ini=(i-1)*divs+1
  
  if(i == rangos){
    fin=dim(data)[1]
  }else{
    fin = ini + (divs-1)
  }

  return(calc_desc_ts(data[ini:fin,]))}

if(procesadores>dim(data)[1]){
  rangos = 0
} else {
  rangos=floor(dim(data)[1]/floor(dim(data)[1]/procesadores))
}
if (dim(data)[1]%%procesadores > 0){
  rangos=rangos+1
}

require(snowfall)
sfInit(parallel=T,cpus=procesadores)  
sfExport("data")
sfExport("calc_desc_ts")
sfExport("paral_desc_ts")
sfExport("procesadores")
sfExport("rangos")
res=sfLapply(1:rangos,paral_desc_ts)

sfStop()

res_xh=data.frame()
res_xw=data.frame()
for (i in seq(rangos)){
  res_xh=rbind(res_xh,c(res[[i]][,1:dim(data)[2]]))
  ini=dim(data)[2]+1
  res_xw=rbind(res_xw,c(res[[i]][,ini:dim(res[[1]])[2]]))
}

#Modificar los nombres de las columnas de los archivos de  salida 
colnames(res_xh)=paste("xh_",c(seq(dim(data)[2])),sep="")
colnames(res_xw)=paste("xw_",c(seq(dim(data)[2])),sep="")

#Guardar las tablas finales como archivos CSV
#Directorio de salida - Importante que los slash se inclinen a la derecha y estar entre comillas
dir_tables_output="C:/Users/Administrador/Google Drive/Vazquez/" 

write.csv(res_xh,paste(dir_tables_output,strsplit(basename(file),split=".dbf")[1],"_xh.csv",sep=""),row.names=F)
write.csv(res_xw,paste(dir_tables_output,strsplit(basename(file),split=".dbf")[1],"_xw.csv",sep=""),row.names=F)     

# Opcional: Gr?ficas Evoluci?n Xh,Xw y NDVI
# dir_img_output="D:/NOAA_IDRISI/data/proccessed/graficos"  
# 
# for (i in seq(dim(data)[1]))
# png(paste(dir_img_output,"grafico",file,"_",i,".png",sep=""), 2000, 2000)
# plot(t(data[i,]), main="Comportamiento NDVI, Xh y Xw", xlab="Dates", ylab="Components", ylim=c(-0.2, 1), type="l", col="blue")
# lines(1:dim(data)[2], t(res_xh[i,]), col="red")
# lines(1:dim(data)[2], t(res_xw[i,]), col="green")
# dev.off()
}