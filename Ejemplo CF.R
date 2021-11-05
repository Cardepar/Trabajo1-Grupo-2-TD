library(dplyr)
source("teoriadecision_funciones_incertidumbre.R")

intervalos.alfa=function(tablaX,favorable=TRUE) {
  
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

# Una persona decide regalarle a su pareja por su cumpleaños un viaje juntos, para el año que viene.
# Para ello, puede decidir buscar ya mismo el alojamiento, o esperar unos meses. Si lo busca ahora, aun-
# que sea algo precipitado, y puede que no encuentre muchos resultados, podría encontrar un piso barato.
# No obstante, si espera unos meses, puede encontrar más opciones de alojamiento, pero, probablemente,
# más caros. Se plantea, entonces, la siguiente tabla de ganancias:

#              ---------------------------------------------------------------------------
# ------------|  Más caro de lo esperado  |  Más barato de lo esperado  |   Lo esperado   |
# Reservar ya |            -75            |             60              |        30       |
# Esperar     |           -100            |            100              |        10       |
# ----------------------------------------------------------------------------------------

datos=c(-75,60,30,
        -100,100,10)
tabla=crea.tablaX(datos,numalternativas = 2,numestados = 3)

### Criterio de Wald.

Wald=criterio.Wald(tabla)
Wald$AlternativaOptima # Según el criterio de Wald, la decisión óptima es reservar ya.

### Criterio optimista.

Optimista=criterio.Optimista(tabla)
Optimista$AlternativaOptima # Según el criterio optimista, es mejor reservar más tarde.

### Criterio de Hurwicz.

Hurwicz=criterio.Hurwicz.General(tabla,alfa=0.5)
Hurwicz$AlternativaOptima # Según el criterio de Hurwicz, debemos esperar para reservar.

### Criterio de Savage.

Savage=criterio.Savage(tabla)
Savage$AlternativaOptima # El criterio de Savage también nos recomienda esperar.

### Criterio de Laplace.

Laplace=criterio.Laplace(tabla)
Laplace$AlternativaOptima # Laplace nos indica que lo mejor es reservar ya.

### Criterio del punto ideal.

Ideal=criterio.PuntoIdeal(tabla)
Ideal$AlternativaOptima # El criterio del punto ideal nos indica que es mejor esperar.

# Apliquemos la función creada.

intervalos.alfa(tabla) # Es decir, si alfa se encontrara en el intervalo (0,0'39), la mejor alterna-
# tiva sería la 1, y si alfa fuera mayor a 0'39 (como es el caso), la mejor alternativa sería la 2.

# Comprobemos los resultados de la función representando el criterio de Hurwicz.

dibuja.criterio.Hurwicz(tabla)