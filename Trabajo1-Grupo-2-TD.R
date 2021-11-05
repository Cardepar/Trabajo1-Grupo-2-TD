
# Trabajo1-Grupo-2-TD

# La lista de componentes del grupo

# - Carlos De Castilla Parrilla (Responsble)
# - Tamara Carmona Naranjo
# - Carlos Fernando Moreno León
# - José María Girol González
# -

library(dplyr)

# Funciones Auxiliares

crea.tablaX = function(vector_matporfilas,numalternativas=3,numestados=4) {
  
  X = matrix(vector_matporfilas,nrow=numalternativas,ncol=numestados,byrow=TRUE)
  colnames(X) = paste('e',1:numestados,sep='');
  rownames(X) = paste('d',1:numalternativas,sep='');
  return(X);
  
}


which.max.general = function(vector) {
  maximo = max(vector);
  res = which(vector == maximo);
  return(res);
  
}


which.min.general = function(vector) {
  minimo = min(vector);
  res = which(vector == minimo);
  return(res);
  
}


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



# Función Principal (creada por el grupo)
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







# Ejercicio de Carlos De Castilla

# Unos amigos quieren pasar la noche en el casino y jugarse parte de sus
# ahorros en alguno de los distintos juegos que se ofrecen en las mesas
# del salón quieren decidir en cuál es mejor jugar. 
# Los distintos juegos son: El Bingo, El Blackjack, El Póker y La Ruleta.
# En cada juego (alternativa) existen 5 estados de la naturaleza:
# Muy mala suerte, Mala suerte, Suerte neutra, Buena suerte y Muy buena suerte.
# A continuación represento la tabla de valores (ganancias en €):
# Juego: (Suerte) // Muy mala // Mala // Neutra // Buena // Muy Buena //    
# Bingo:          //   -40    // -30  //   -10  //   10  //     30    //
# Blackjack:      //   -40    // -20  //   -10  //   20  //     40    //
# Póker:          //   -120   // -60  //   -30  //   50  //     150   //
# Ruleta:         //   -140   // -90  //   -10  //   60  //     170   //

datos=c(-40,-30,-10,10,30,
        -40,-20,-10,20,40,
        -120,-60,-30,50,150,
        -140,-90,-10,60,170)
matriz=crea.tablaX(datos,4,5)

intervalos.alfa(matriz,T)
# Valores de Alfa a partir de los cuales cambiamos de alternativa.
# El Alfa mide nuestra valentía siendo 0 muy pesimista y 1 muy optimista.
# Si nuestro alfa va de [0,0.01) la mejor alternativa es el Bingo.
# Si nuestro alfa está entre [0.01,0.44) lo mejor es jugar al Blackjack.
# Si nuestro alfa va de [0.44,0.52) la mejor decisión es jugar al Póker.
# Y si nuestro alfa va de [0.52,1] el mejor plan es jugar a la Ruleta.
dibuja.criterio.Hurwicz(matriz,T) # Comprobamos.




