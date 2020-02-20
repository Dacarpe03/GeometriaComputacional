#Función que dibuja una lemniscata en función del ancho de su hoja
dibujarLemniscata <- function(){
  
  #Calculamos las coordenadas de los puntos de la lemniscata para dibujarlos después
  t <- seq(0, pi/2, length=50) #Parámetro t de la curva
  x <- (cos(t))/(1+sin(t)^2)  #Coordenadas x de la curva
  y <- x*sin(t) #Coordenadas y de la curva
  
  #Calculamos los focos para pintarlos después
  foco1 = c(-1/sqrt(2), 0) #Foco izquierdo
  foco2 = c(1/sqrt(2), 0) #Foco derecho
  
  #Dibujamos la lemniscata
  plot(x, y, type='l', col='blue')
  #Dibujamos sus focos
  points(foco1[1], foco1[2], type='b', pch=19, col='red')
  points(foco2[1], foco2[2], type='b', pch=19, col='red')
  
}#end dibujarLemniscata


#Función que dibuja un punto
dibujarPuntoTriangulacion <- function(punto){
  points(punto[1], punto[2], type='o', pch=18, col='green')
}#fin dibujarPuntoTriangulacion


#Función que calcula el módulo de un vector
modulo <- function(vector){
  sqrt(vector[1]^2 + vector[2]^2)
}#fin dibujarLemniscata


#Función recursiva que triagula la lemniscata
calcularAreaLemniscata <- function(profundidadMaxima, profundidadActual, puntoIzq, puntoDer){
  
  #Primero comprobamos si hemos llegado a la profundidad
  print(profundidadActual)
  if(profundidadActual < profundidadMaxima){
    #Calculamos la siguiente profundidad
    sigProfundidad = profundidadActual+1
    calcularAreaLemniscata(profundidadMaxima, sigProfundidad, puntoIzq, puntoDer)
    calcularAreaLemniscata(profundidadMaxima, sigProfundidad, puntoIzq, puntoDer)
  }else{
    print("Llego al fondo")
  }
  
}#fin calcularAreaLemniscata


#Función main que contiene todo el proceso de resolución
main <- function(){
  #Definimos la "profundidad" del algoritmo de triangulación
  profundidadMaxima = 2;
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
  
  #Calculamos el área
  calcularAreaLemniscata(profundidadMaxima, 0, origen, extremoDerLemniscata)

}#fin main


#Lanzamos el programa
main()