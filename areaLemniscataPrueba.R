
#Función que dibuja una lemniscata en función del ancho de su hoja
dibujarLemniscata <- function(ancho){
  t <- seq(0, pi/2, length=50) #Parámetro t de la curva
  
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

modulo <- function(vector){
  sqrt(vector[1]^2 + vector[2]^2)
}

dibujarLemniscata(1)
points(0.5, 0.5*sin(pi/4), type='b')
foco1 = c(-1/sqrt(2), 0) #Foco izquierdo
foco2 = c(1/sqrt(2), 0) #Foco derecho
p = c(1,0)
points(p[1], p[2], type='b', pch=19, col='red')
v1 = p-foco1
v2 = p-foco2
mod1 = modulo(v1)
mod2 = modulo(v2)
mod3 = modulo(foco1)
print(mod3^2)
print(mod1 * mod2)