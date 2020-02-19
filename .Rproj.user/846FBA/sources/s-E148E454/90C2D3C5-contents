
#Función que dibuja una lemniscata en función del ancho de su hoja
dibujarLemniscata <- function(ancho){
  t <- seq(0, pi/2, length=50) #Parámetro t de la curva
  
  #Calculamos las coordenadas de los puntos de la lemniscata para dibujarlos después
  x <- ancho*(cos(t))/(1+sin(t)^2)  #Coordenadas x de la curva
  y <- x*sin(t) #Coordenadas y de la curva
  
  print(t)
  print(x)
  print(y)
  #Calculamos los focos para pintarlos después
  foco1 = ancho*c(-1/sqrt(2), 0) #Foco izquierdo
  foco2 = ancho*c(1/sqrt(2), 0) #Foco derecho
  
  #Dibujamos la lemniscata
  plot(x, y, type='l', col='blue')
  #Dibujamos sus focos
  points(foco1[1], foco1[2], type='b', pch=19, col='red')
  points(foco2[1], foco2[2], type='b', pch=19, col='red')
}#end dibujarLemniscata

dibujarLemniscata(1)
x <- 0.5
points(0.5, 0.5*sin(pi/4), type='b')


