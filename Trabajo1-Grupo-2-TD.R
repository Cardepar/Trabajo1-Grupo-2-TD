
# TRABAJO1-GRUPO-2-TD

# Lista de componentes del grupo

# - Carlos De Castilla Parrilla (Responsble)
# - Tamara Carmona Naranjo
# - Carlos Fernando Moreno León
# - José María Girol González
# - Marina Rivero Moreno


# Funciones Auxiliares
source("teoriadecision_funciones_incertidumbre.R") #estraídas de aquí

library(dplyr)


## Introducir nuestra matriz de valores:
crea.tablaX = function(vector_matporfilas,numalternativas=3,numestados=4) {
  
  X = matrix(vector_matporfilas,nrow=numalternativas,ncol=numestados,byrow=TRUE)
  colnames(X) = paste('e',1:numestados,sep='');
  rownames(X) = paste('d',1:numalternativas,sep='');
  return(X);
  
}

## Devolver el máximo de un vector:
which.max.general = function(vector) {
  maximo = max(vector);
  res = which(vector == maximo);
  return(res);
  
}

## Devolver el mínimo de un vector
which.min.general = function(vector) {
  minimo = min(vector);
  res = which(vector == minimo);
  return(res);
  
}

## Representar gráficamente el método de Hurwicz
dibuja.criterio.Hurwicz = function(tablaX,favorable=TRUE) {
  X = tablaX;
  Altmin = apply(X,MARGIN=1,min);
  Altmax = apply(X,MARGIN=1,max);
  valfa = seq(from=0,to=1,by=0.05);
  vHurwicz = rep(0,length(valfa));
  Alt_vHurwicz = rep(0,length(valfa));
  for (i in 1:length(valfa)) {
    alfab = valfa[i];  
    if (favorable) { 
      vAltH = alfab * Altmax + (1-alfab) * Altmin; 
      vHurwicz[i] = max(vAltH)
    } else {
      vAltH = alfab * Altmin + (1-alfab) * Altmax; 
      vHurwicz[i] = min(vAltH)      
    }
    
  }
  
  x0=0;x1=1;
  y0 = min(Altmin);
  y1 = max(Altmax);
  rg = y1-y0;
  y0=y0-0.1*rg;y1=y1+0.1*rg;
  plot(c(x0,x1), c(y0,y1), type = "n", xlab = "alpha",
       ylab = "Criterio Hurwicz"); 
  nn = length(Altmin);
  colores = rainbow(nn);
  abline(v=0);
  abline(v=1);
  if (favorable) {
    for (i in 1:nn) {
      aa = Altmin[i];
      bb = (Altmax[i] - Altmin[i]);
      abline(a=aa,b=bb,col=colores[i]);
    }
  } else {
    for (i in 1:nn) {
      aa = Altmax[i];
      bb = (Altmin[i] - Altmax[i]);
      abline(a=aa,b=bb,col=colores[i]);
    }        
  }
  lines(valfa,vHurwicz,col=rainbow(nn+1)[nn+1],lty=3,lwd=3)
  if (favorable) {
    legend("bottomright",legend=rownames(X),fill=colores,inset=0.05)
    title("Criterio de Hurwicz (favorable - línea discontinua)")
  } else {
    legend("topright",legend=rownames(X),fill=colores,inset=0.05)
    title("Criterio de Hurwicz (desfavorable - línea discontinua)")    
  }
  
}


## Aplicar todos los métodos de incertidumbre:
criterio.Todos = function(tablaX,alfa=0.3,favorable=TRUE) {
  
  cri01 = criterio.Wald(tablaX,favorable);
  cri02 = criterio.Optimista(tablaX,favorable);
  cri03 = criterio.Hurwicz(tablaX,alfa,favorable);
  cri04 = criterio.Savage(tablaX,favorable);
  cri05 = criterio.Laplace(tablaX,favorable);
  cri06 = criterio.PuntoIdeal(tablaX,favorable);
  
  numestados = ncol(tablaX)
  numalterna = nrow(tablaX)
  
  resultado = cbind(tablaX,cri01$ValorAlternativas,cri02$ValorAlternativas,
                    cri03$ValorAlternativas,cri04$ValorAlternativas,
                    cri05$ValorAlternativas,cri06$ValorAlternativas);
  
  decopt = c(rep(NA,numestados),cri01$AlternativaOptima[1],
             cri02$AlternativaOptima[1],cri03$AlternativaOptima[1],
             cri04$AlternativaOptima[1],cri05$AlternativaOptima[1],
             cri06$AlternativaOptima[1]);
  
  resultado = rbind(resultado,decopt);
  
  colnames(resultado)[numestados+1] = cri01$criterio;
  colnames(resultado)[numestados+2] = cri02$criterio;
  colnames(resultado)[numestados+3] = cri03$criterio;
  colnames(resultado)[numestados+4] = cri04$criterio;
  colnames(resultado)[numestados+5] = cri05$criterio;
  colnames(resultado)[numestados+6] = cri06$criterio;
  
  if (favorable) {
    rownames(resultado)[numalterna+1] = 'iAlt.Opt (fav.)';
  } else {
    rownames(resultado)[numalterna+1] = 'iAlt.Opt (Desfav.)';
  }
  
  ## nuevo
  resultado = as.data.frame(resultado)
  resultado = format(resultado,digits=4)
  decopt = c(rep('--',numestados),
             paste0(names(cri01$AlternativaOptima),collapse = ","),
             paste0(names(cri02$AlternativaOptima),collapse = ","),
             paste0(names(cri03$AlternativaOptima),collapse = ","),
             paste0(names(cri04$AlternativaOptima),collapse = ","),
             paste0(names(cri05$AlternativaOptima),collapse = ","),
             paste0(names(cri06$AlternativaOptima),collapse = ","));
  resultado[nrow(resultado),] = decopt
  ## fin nuevo
  
  return(resultado)
  
}


# Función Principal creada por el grupo:
intervalos.alfa= function(tablaX,favorable=TRUE) {
  
  alfa=seq(0,1,by=0.01) # Introducimos un conjunto de alfas que nos servirán
  # para saber cuándo cambia la alternativa óptima. Fijamos un valor de 0.01,
  # el cual indica cada cuánto se quiere que exista alfa.
  X = tablaX
  
  if(favorable){ #en el caso de que sea favorable
    Altmin = apply(X,MARGIN=1,min)
    Altmax= apply(X,MARGIN=1,max)
    
    # Como ya no tenemos un alfa, sino varios, debemos crear un bucle que
    # trabaje con todos los alfa. Como, además, Altmin y Altmax son vecto-
    # res, tenemos que crear listas que nos devuelvan, para cada elemento
    # de dichos vectores, cuáles son las alternativas asociadas a los
    # alfas.
    
    AltH=list()
    Hurwicz=list()
    Alt_Hurwicz=list()
    
    # Creamos el bucle:
    
    for(i in 1:length(alfa)){
      AltH[[i]] = alfa[i] * Altmax + (1-alfa[i]) * Altmin
      Hurwicz[[i]] = max(AltH[[i]])
      Alt_Hurwicz[[i]] = which.max.general(AltH[[i]])
    }
    
    metodo = 'favorable'
    
  } else { #en caso de que no sea favorable
    Altmin = apply(X,MARGIN=1,min)
    Altmax= apply(X,MARGIN=1,max)
    
    AltH=list()
    Hurwicz=list()
    Alt_Hurwicz=list()
    
    for(i in 1:length(alfa)){
      AltH[[i]] = (1-alfa[i]) * Altmax + alfa[i] * Altmin 
      Hurwicz[[i]] = min(AltH[[i]])
      Alt_Hurwicz[[i]] = which.min.general(AltH[[i]])
    }
    
    metodo = 'desfavorable'
    
  }
  
  resultados = list();
  resultados$metodo = metodo;
  resultados$ValorAlternativa = unlist(Hurwicz); # Valores que toma cada
  # alfa en su alternativa óptima.
  resultados$alfa = alfa; # Alfas usados.
  resultados$AlternativaOptima = unlist(Alt_Hurwicz);
  resultados$Solucion = unlist(Alt_Hurwicz);
  names(resultados$Solucion)=alfa;
  # distinct(as.data.frame(resultados,alfa));
  prueba=cbind.data.frame(alfa,resultados$Solucion[1:length(alfa)]);
  colnames(prueba)<-c("Alfa", "Solución");
  rownames(prueba %>% distinct(Solución));
  alternativas= prueba %>% distinct(Solución);
  resultados$intervalos = alternativas
  return(resultados)
}


# Ejemplo (favorable):

datos=c(2160,360,
        720,720,
        3480,480)
matriz=crea.tablaX(datos,3,2)

intervalos.alfa(matriz,T)
# Valores de Alfa a partir de los cuales cambiamos de alternativa.
dibuja.criterio.Hurwicz(matriz,T) # Comprobamos.

# Ejemplo (desfavorable):

datos=c(2160,360,
        720,720,
        3480,480)
matriz=crea.tablaX(datos,3,2)

intervalos.alfa(matriz,FALSE)
# Valores de Alfa a partir de los cuales cambiamos de alternativa.
dibuja.criterio.Hurwicz(matriz,FALSE) # Comprobamos.



# Problemas realizados por cada integrante del grupo


### EJEMPLO 1


### EJEMPLO 2


### EJEMPLO 3


### EJEMPLO 4 (José María)

# El dueño de una empresa constructora tiene ciertas dudas sobre si debe 
# construir un almacén pequeño, mediano o grande. El tipo de demanda que se 
# puede presentar es: baja, media o alta, con probabilidades estimadas de 0.2, 
# 0.5 y 0.3, respectivamente. Ha de elegir que alternativa es la óptima ya que 
# no le interesaría perder dinero ni mucho menos. Las condiciones son las 
# siguientes:
# 
# - Instalando un almacén pequeño se esperaría ganar un valor neto de sólo 15000 
# euros si la demanda es baja. Si el tipo de demanda es medio, se espera que la 
# instalación pequeña gane 70000 euros. Si la demanda es alta, cabría esperar 
# que la instalación pequeña ganara 78000 euros. 
# 
# - Con un almacén de tamaño mediano se esperaría una pérdida estimada en 30000 
# euros si la demanda es baja, y una ganancia de 150000 euros si la demanda es 
# media. Si la demanda es alta, cabría esperar que la instalación de tamaño 
# mediano ganara un valor neto de 170000 euros.
# 
# - Si el constructor optase por construir un almacén grande y la demanda 
# resultara ser alta, se esperaría que las ganancias ascendieran a 220000 euros. 
# Si la demanda resultara ser de magnitud medio para la instalación grande, se 
# esperaría que el valor fuera igual a 135000 euros; finalmente si la demanda 
# fuera baja, cabría esperar que la instalación perdiera 65000 euros.

# Luego tenemos la siguiente tabla de valores del problema:
#   
# ALT. \\ ESTADOS | DEMANDA BAJA | DEMANDA MEDIA | DEMANDA ALTA |
# ---------------------------------------------------------------
# ALMACÉN PEQUEÑO |     15000    |      70000    |      78000   |
# ALMACÉN MEDIANO |    -30000    |     150000    |     170000   |
# ALMACÉN GRANDE  |    -65000    |     135000    |     220000   |


### EJEMPLO 5

# Marina está barajando la posibilidad de darse de alta en una plataforma para ver
# series y películas. Para ver cual es la que más se adapta a sus necesidades se 
# ha informado y ha encontrado la siguiente información:
#   
# -	Con la plataforma 1 podría ver todo el contenido que quiera (series, 
# películas, etc). Esta plataforma tiene un precio de 20€ al mes. Si queremos 
# añadirle una pantalla más, es decir, poder ver el contenido en dos dispositivos 
# diferentes al mismo tiempo debemos pagar 5€ más al mes.
# 
# -	Con la plataforma 2 tendría acceso a solo a series o películas por 12 euros. 
# Si queremos tener todo el contenido debería pagar 10€ más cada mes. La 
# ampliación a dos pantallas cuesta 3,5€ al mes.
# 
# -	Con la plataforma 3 dispondríamos de series y películas en dos dispositivos 
# por 23€ al mes.
# 
# ¿Qué le aconsejaría si su objetivo es minimizar los costes?
