#Autores: 
        #Lucas Gómez Torres
        #Israel Peñalver Sánchez
        #Daniel Carmona Pedrajas
#PRÁCTICA 1: Calcular el área de la lemniscata mediante triangulación

#FUNCIONES

#Función que dibuja una lemniscata en función del ancho de su hoja
dibujarLemniscata <- function(ancho){
  t <- seq(0, 2*pi, length=500) #Parámetro t de la curva
  
  #Calculamos las coordenadas de los puntos de la lemniscata para dibujarlos después
  x <- ancho*(cos(t))/(1+sin(t)^2)  #Coordenadas x de la curva
  y <- x*sin(t) #Coordenadas y de la curva
  
  #Calculamos los focos para pintarlos después
  foco1 = ancho*c(-1/sqrt(2), 0) #Foco izquierdo
  foco2 = ancho*c(1/sqrt(2), 0) #Foco derecho
  
  #Dibujamos la lemniscata
  plot(x, y, type='l', col='blue')
  #Dibujamos sus focos
  points(foco1[1], foco1[2], type='b', pch=19, col='red')
  points(foco2[1], foco2[2], type='b', pch=19, col='red')
}#fin dibujarLemniscata

#Función que dibuja un array de puntos
dibujarPuntos <- function(puntos){
  #Calculamos el número de puntos que tenemos
  numPuntos <- calcularNumPuntos(puntos)
  
  #Pintamos cada punto
  for(i in 1:numPuntos){
    points(puntos[i,1], puntos[i,2], type='o', pch=18, col='green')
  }
}#fin dibujarPuntos

#Función que une los puntos discretizados
dibujarTriangulacion <- function(puntos){
  #Calculamos el número de puntos que tenemos
  numPuntos <- calcularNumPuntos(puntos)
  
  #Unimos todos los puntos con el (0,0)
  for(i in 1:numPuntos){
    segments(0, 0, puntos[i,1], puntos[i,2], col="#AE5CEE")
  }
  
  segments(puntos[numPuntos,1], puntos[numPuntos,2], 0, 0, col="#AE5CEE")
  
  #Unimos cada punto con sus adyacentes
  for(i in 1:(numPuntos-1)){
    segments(puntos[i,1], puntos[i,2], puntos[i+1,1], puntos[i+1,2], col="#AE5CEE")
  }
}#fin dibujarTriangulacion

#Calcula el número de puntos dado array bidimensional con 2 columnas de coordenadas, una para la x y otra para la y
calcularNumPuntos <- function(puntos){
  #Tenemos un array bidimensional de n coordenadas x y n coordenadas y por lo que tenemos una longitud 2n
  numPuntos <- length(puntos)/2 #Como tenemos 2n coordenadas tenemos n puntos
}#fin calcularNumPuntos

#Función que calcula n puntos discretizados de la lemniscata en el primer cuadrante (t in (0, pi/2))
calcularPuntosLemniscata <- function(n, ancho){
    t <- seq(0, 2*pi, length=(n+1)) #Parámetro t de la curva. Ahora sólo del primer cuadrante
    #Pongo n+1 porque el último elemento de esta división es el pi/2 que me dará el punto (0,0), este punto se eliminará más adelante
    #Como ese punto se elimina y quiero n puntos en total, hago la división en n+1
    
    #Calculamos las coordenadas de los puntos de la lemniscata para dibujarlos después
    x <- ancho*(cos(t))/(1+sin(t)^2)  #Coordenadas x de la curva
    y <- x*sin(t) #Coordenadas y de la curva
    
    #Quitamos el (0,0)
    x <- x[-c(n+1)]
    y <- y[-c(n+1)]
    
    #Guardamos las coordenadas de los puntos en un array y se devuelven
    puntos = array(c(x, y), dim=c(length(x),2))
}#fin calcularPuntosLemniscata

#Función para calcular vectores uniendo el (0,0) con los puntos discretizados
calcularArea <- function(puntos){
  #Calculamos el número de puntos
  numPuntos <- calcularNumPuntos(puntos)
  area <- 0
  for(i in 1:(numPuntos-1)){
    area <- area + area_triangle(c(0,0), c(puntos[i,1], puntos[i,2]), c(puntos[i+1,1],puntos[i+1,2]))
  }
  return (area)
    
}#fin calcularVectores

area_triangle<-function(A, B, C){
  return(abs(det(matrix(c(A-B,A-C), nrow = 2, ncol=2)))/2)
}

#Función main que contiene todo el proceso de resolución
main <- function(){
  #Definimos un número de puntos para triangular
  numPuntos <- 100000000
  #Definimos un ancho
  ancho <- 1
  #Calculamos los puntos discretizados
  puntosDiscretizados <- calcularPuntosLemniscata(numPuntos, ancho)
  #GRÁFICOS
  #Dibujamos la lemniscata
  #dibujarLemniscata(ancho)
  #Dibujamos los puntos
  #dibujarPuntos(puntosDiscretizados)
  #Dibujamos la triangulación
  #dibujarTriangulacion(puntosDiscretizados)
  area <- calcularArea(puntosDiscretizados)
  print(area)
}#fin main

#Lanzamos el programa
main()


