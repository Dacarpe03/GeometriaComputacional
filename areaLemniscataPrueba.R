#FunciÃ³n que dibuja una lemniscata en funciÃ³n del ancho de su hoja
dibujarLemniscata <- function(){
  
  #Calculamos las coordenadas de los puntos de la lemniscata para dibujarlos despuÃ©s
  t <- seq(0, pi/2, length=50) #ParÃ¡metro t de la curva
  x <- (cos(t))/(1+sin(t)^2)  #Coordenadas x de la curva
  y <- x*sin(t) #Coordenadas y de la curva
  
  #Calculamos los focos para pintarlos despuÃ©s
  foco1 = c(-1/sqrt(2), 0) #Foco izquierdo
  foco2 = c(1/sqrt(2), 0) #Foco derecho
  
  #Dibujamos la lemniscata
  plot(x, y, type='l', col='blue')
  #Dibujamos sus focos
  points(foco1[1], foco1[2], type='b', pch=19, col='red')
  points(foco2[1], foco2[2], type='b', pch=19, col='red')
  
}#end dibujarLemniscata


#FunciÃ³n que dibuja un punto
dibujarPuntoTriangulacion <- function(punto){
  points(punto[1], punto[2], type='o', pch=18, col='green')
}#fin dibujarPuntoTriangulacion

#FunciÃ³n que dibuja un punto
dibujarPunto <- function(punto){
  points(punto[1], punto[2], type='o', pch=4, col='green')
}#fin dibujarPuntoTriangulacion

#FunciÃ³n que dibuja un segmento dados dos puntos
dibujarSegmento <- function(punto1, punto2){
  segments(punto1[1], punto1[2], punto2[1], punto2[2], col="#AE5CEE")
}#fin dibujarSegmento

#FunciÃ³n que dibuja una linea dado un vector y un punto
dibujarLinea <- function(punto, vector){
  x <- seq(0, 1, length=10)
  a <- punto[1] + vector[1]*x
  y <- seq(0, 1, length=10)
  b <- punto[2] + vector[2]*y
  lines(a, b, type='l')
}

#FunciÃ³n que calcula el mÃ³dulo de un vector
modulo <- function(vector){
  module <- sqrt(vector[1]^2 + vector[2]^2)
  return(module)
}#fin dibujarLemniscata


#FunciÃ³n que calcula la mediatriz de un segmento dados dos puntos
calcularMediatriz <- function(punto1, punto2){
  mediatriz <- (1/2)*c(punto1[1]+punto2[1], punto1[2]+punto2[2])
}#fin calcularMediatriz


#FunciÃ³n que calcula el vector de la bisectriz de un segmento dados dos puntos
calcularVectorDirector <- function(punto1, punto2){
  #Calculamos el vector director del segmento
  vector <- calcularVector(punto1, punto2)
  
  #Calculamos el perpendicular
  vectorDirector <- c(-vector[2], vector[1])
}#fin calcularVectorDirector


#FunciÃ³n que calcula un vector dados dos puntos
calcularVector <- function(punto1, punto2){
  vector <- c(punto2[1]-punto1[1], punto2[2]-punto1[2])
}

#FunciÃ³n que calcula la desviaciÃ³n de un punto a la lemniscata
calcularError <- function(punto, foco1, foco2, d){
  vectorPF1 <- calcularVector(punto, foco1)
  vectorPF2 <- calcularVector(punto, foco2)
  
  mod1 <- modulo(vectorPF1)
  mod2 <- modulo(vectorPF2)
  
  error <- abs(d^2 - mod1*mod2)
  error
}#fin calcularError


#FunciÃ³n que aproxima la coordY dada una x y un vector paralelo al eje Y
buscarCoordY <- function(punto, vector, foco1, foco2, errorAproximacion){
  
  d <- 1/sqrt(2)
  error <- 1
  
  
  abajo <- 0
  arriba <- 1
  
  coordX <- punto[1]
  
  foco1 <- c(-1/sqrt(2), 0)
  foco2 <- c(1/sqrt(2), 0)
  
  while(error > errorAproximacion){
    medio <- (abajo+arriba)/2
    
    puntoArriba <- c(coordX, arriba)
    puntoAbajo <- c(coordX, abajo)
    puntoMedio <- c(coordX, medio)
    
    errorArriba <- calcularError(puntoArriba, foco1, foco2, d)
    errorAbajo <- calcularError(puntoAbajo, foco1, foco2, d)
    errorMedio <- calcularError(puntoMedio, foco1, foco2, d)
    
    if(errorArriba <= errorAbajo){
      abajo <- medio
      if(errorArriba <= errorMedio){
        error <- errorArriba
      }else{
        error <- errorMedio
      }
    }else{
      arriba <- medio
      if(errorAbajo <= errorMedio){
        error <- errorAbajo
      }else{
        error <- errorMedio
      }
    }
    print(error)
  }
  medio
}#fin buscarCoordY


#FunciÃ³n que aproxima el punto de intersecciÃ³n de la lemniscata con la bisectriz de un segmento
calcularSigPunto <- function(punto1, punto2){
  
  #Primero calculamos la recta bisectriz del segmento punto1 punto2 
  mediatriz <- calcularMediatriz(punto1, punto2) #Calculamos mediatriz
  vectorDirector <- calcularVectorDirector(punto1, punto2) #Calculamos el vector director de la bisectriz
  dibujarLinea(mediatriz, vectorDirector) #Dibujamos la bisectriz
  
  #Ahora calculamos los focos de la lemniscata
  foco1 = c(-1/sqrt(2), 0) #Foco izquierdo
  foco2 = c(1/sqrt(2), 0) #Foco derecho
  
  #Definimos el error de aproximaciÃ³n que aceptamos en la bÃºsqueda de la intersecciÃ³n
  errorAproximacion = 0.00001
  
  #Hay dos casos extremos en los que la bisectriz puede ser paralela al eje x o al eje y
  if(vectorDirector[1] == 0){ # El vector es paralelo al eje y
    coordY <- buscarCoordY(mediatriz, vectorDirector, foco1, foco2, errorAproximacion)
    sigPunto <- c(mediatriz[1], coordY)
    print("Pinto")
    dibujarPunto(sigPunto)
  }else if(vectorDirector[2] == 0){ #El vector es paralelo al eje x
    print("NO")
  }else{ #El vector no es paralelo a ningÃºn eje
    print("NO")
  }
}#fin calcularSigPunto




#FunciÃ³n recursiva que triagula la lemniscata con d=1/sqrt(2)
calcularAreaLemniscata <- function(profundidadMaxima, profundidadActual, puntoIzq, puntoDer, area){
  
  print(area)
  
  #Primero dibujamos el segmento que une los dos puntos de este paso de recursiÃ³n
  dibujarSegmento(puntoIzq, puntoDer)
  
  print(paste0("Profundidad actual: ", profundidadActual))
  
  if(profundidadActual < profundidadMaxima){ #Comprobamos si hemos llegado a la profundidad buscada
  
    #Calculamos el siguiente punto de triangulaciÃ³n
    puntoTriangulacion <- calcularSigPunto(puntoIzq, puntoDer)
    #puntoTriangulacion <- calcularPuntoTriangulacion()
    mediatriz <- calcularMediatriz(puntoIzq, puntoDer)
    area <- area + 1
    
    #Calculamos la siguiente profundidad
    sigProfundidad = profundidadActual+1
    print("A la izquierda")
    area <- calcularAreaLemniscata(profundidadMaxima, sigProfundidad, puntoIzq, mediatriz, area) #Parte Izquierda
    print("A la derecha")
    area <- calcularAreaLemniscata(profundidadMaxima, sigProfundidad, mediatriz, puntoDer, area) #Parte Derecha
    
  }else{
    
    area <- area+1
    print("Llego al fondo y subimos de profundidad")
  }
  
  area
}#fin calcularAreaLemniscata


#FunciÃ³n main que contiene todo el proceso de resoluciÃ³n
main <- function(){
  #Definimos la "profundidad" del algoritmo de triangulaciÃ³n
  profundidadMaxima = 3;
  profundidadInicial = 0;
  
  #Dibujamos la lemniscata
  dibujarLemniscata()
  
  #Puntos iniciales del algoritmo
  origen <- c(0,0)
  t <- 0
  extremoDerLemniscata <- c(cos(t)/(1+sin(t)^2),cos(t)*sin(t)/(1+sin(t)^2)) #Coordenadas y de la curva)
  
  #Los dibujamos
  dibujarPuntoTriangulacion(origen)
  dibujarPuntoTriangulacion(extremoDerLemniscata)
  
  #Calculamos el Ã¡rea
  print(calcularAreaLemniscata(profundidadMaxima, 0, origen, extremoDerLemniscata, 0))

}#fin main


#Lanzamos el programa
#FunciÃ³n que aproxima el punto de intersecciÃ³n de la lemniscata con la bisectriz de un segmento
main()
