
# Trabajo1-Grupo-2-TD

# La lista de componentes del grupo

# - Carlos De Castilla Parrilla (Responsble)
# - Tamara Carmona Naranjo
# - Carlos Fernando Moreno León
# -
# -
  
  
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



# Función Principal
intervalos.alfa= function(tablaX,p,favorable=TRUE) {
  
  alfa=seq(0,1,by=p) # Introducimos un conjunto de alfas que nos servirán
  # para saber cuándo cambia la alternativa óptima. A menor p, mayor pre-
  # cisión.
  X = tablaX
  
  if(favorable){
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
    
  } else {
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
  return(resultados)
}


# Ejemplo (favorable):

datos=c(2160,360,
        720,720,
        3480,480)
matriz=crea.tablaX(datos,3,2)

intervalos.alfa(matriz,0.1,T)
dibuja.criterio.Hurwicz(matriz,T) # Comprobamos.

# Ejemplo (desfavorable):

datos=c(2160,360,
        720,720,
        3480,480)
matriz=crea.tablaX(datos,3,2)

intervalos.alfa(matriz,0.1,FALSE)
dibuja.criterio.Hurwicz(matriz,FALSE) # Comprobamos.

