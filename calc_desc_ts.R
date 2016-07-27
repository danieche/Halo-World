##################################################################################################
#Objetivo: Aplicar algoritmo STD y calcular Xh y Xw de los datos cargados en el codigo load_data.R
##################################################################################################

calc_desc_ts=function(data){
  #Generar las matrices de salida de acuerdo al tamano de la matriz original
  final_xh=data
  final_xw=data  
  
  for (f in 1:dim(data)[1]){ #Extraer cada pixel de total de elementos detectados en la matriz de entreda
    
    cell_data <- t(data[f,]) #Hacer una transposición de la matrix para que los valores NDVI del pixel sea analizados por el algoritmo
    
    ##Configuración del algoritmo STD
    #Parametros dinámicos - Modificar según analisis o imagen 
    par1 <- 12 #Numero de periodos por ano
    
    #Parametros fijos
    par2 <- "periodic" 
    par3 <- 0 #s.degree
    par4 <- NULL
    par5 <- 1 #t.degree
    par6 <- ""
    par7 <- 1 #l.degree
    par8 <- FALSE
    
    #Datos
    nx <- dim(data)[2] #Numero de fechas a analizar
    print(nx)
    #Crear serie temporal de acuerdo a los parametros
    x <- ts(array(cell_data),frequency=par1)
    print(x)
    #Funcion 
    if (par6 != "") {
      m <- stl(x,s.window=par2, s.degree=par3, t.window=par4, t.degre=par5, l.window=par6, l.degree=par7, robust=par8)
    } else {
      m <- stl(x,s.window=par2, s.degree=par3, t.window=par4, t.degre=par5, l.degree=par7, robust=par8)
    }
    
    #Extracción de los componentes seasonal, trend, remainder
    seasonal=data.frame(m$time.series[,"seasonal"])
    trend=data.frame(m$time.series[,"trend"])
    remainder=data.frame(m$time.series[,"remainder"])
    
    #Calculos de variables intermedias para obtener Xh y Xw 
    #Dinamicas (Opcional modificar)
    ndvi_soil=0.1 #Especificar el valor según corresponda
    
    #Fijas (No modificar)
    xc_xt=data.frame(cell_data-trend)
    
    xcadj=1:nx
    for (i in 1:nx){
      xcadj[i]=max(xc_xt[i,],seasonal[i,])
    }
    
    max_anual_xcadj=1:nx
    min_anual_xcadj=1:nx
    for (i in 1:nx){
      e=floor((i-1)/par1)*par1+1
      max_anual_xcadj[i]=max(xcadj[e:(e+(par1-1))])
      min_anual_xcadj[i]=min(xcadj[e:(e+(par1-1))])
    }
    
    max_delta=1:nx
    max_delta[1]=seasonal[1,]+xcadj[1]
    for (i in 2:nx){
      max_delta[i]=max(seasonal[i,]+xcadj[i],seasonal[i-1,]+xcadj[i-1])
    }
    
    factor_max=1:nx
    for (i in 1:nx){
      factor_max[i]=(((0.99/(2-0.99))*max_anual_xcadj[i])+((1-0.99)/(2-0.99)*xcadj[i]))
    }
    
    xc_max_t=1:nx
    for (i in 1:nx){
      xc_max_t[i]=max(factor_max[i],xcadj[i])
    }
    
    factor_min=1:nx
    for (i in 1:nx){
      factor_min[i]=(((0.99/(2-0.99))*min_anual_xcadj[i])+((1-0.99)/(2-0.99)*xcadj[i]))
    }
    
    xc_min_t=1:nx
    for (i in 1:nx){
      xc_min_t[i]=min(factor_min[i],xcadj[i])
    }
    
    xa_t=1:nx
    for (i in 1:nx){
      xa_t[i]=xc_max_t[i]-xc_min_t[i]
    }
    
    s_t=1:nx
    for (i in 1:nx){
      s_t[i]=((xcadj[i]-xc_min_t[i])/(xa_t[i]))
    }
    
    xw_s=1:nx
    for (i in 1:nx){
      xw_s[i]=(1+(0.1*s_t[i]))*(trend[i,]-(0.5*xa_t[i]))-(0.1*s_t[i]*ndvi_soil) 
    }
    
    xh=1:nx
    for (i in 1:nx){
      xh[i]=s_t[i]*(((1+0.05)*xa_t[i])-(0.1*trend[i,]))+(0.015*s_t[i])
    }
    
    xw=1:nx
    for (i in 1:nx){
      xw[i]=(1+(0.1*s_t[i]))*(trend[i,]-(0.5*xa_t[i])-(ndvi_soil))
    }
    
    Xw_s_Xh=1:nx
    for (i in 1:nx){
      Xw_s_Xh[i]=xw_s[i]+xh[i]
    }
    
    #Generar el valor de cada pixel dentro de una tabla final por componente
    final_xh[f,]=t(array(xh))
    final_xw[f,]=t(array(xw))}
    
    #Modificar el nombre de las columnas para facilitar lectura en IDRISI
    
    
    return(cbind(final_xh,final_xw))}