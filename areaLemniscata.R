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
}#end dibujarLemniscata

#Función que calcula n puntos discretizados de la lemniscata en el primer cuadrante (t in (0, pi/2))
calcularPuntosLemniscata<-function(n, ancho){
    t <- seq(0, pi/2, length=n) #Parámetro t de la curva. Ahora sólo del primer cuadrante
    #Calculamos las coordenadas de los puntos de la lemniscata para dibujarlos después
    x <- ancho*(cos(t))/(1+sin(t)^2)  #Coordenadas x de la curva
    y <- x*sin(t) #Coordenadas y de la curva
    
    #Guardamos las coordenadas de los puntos en un array
    puntos = array(c(x, y), dim=c(length(x),2))
    
    #Los pintamos
    for(i in 1:length(x)){
      points(puntos[i,1], puntos[i,2], type='o', pch=18, col='green')
    }
}#end calcularPuntosLemniscata

#Función main que contiene todo el proceso de resolución
main <- function(){
  ancho <- 20
  numPuntos <- 20
  dibujarLemniscata(ancho)
  calcularPuntosLemniscata(numPuntos, ancho)
}#end main

#Lanzamos el programa
main()

