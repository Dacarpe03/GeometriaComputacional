

#Función que pinta puntos
dibujarPuntos <- function(puntos){
  #Calculamos el número de puntos que tenemos
  plot(puntos[,1], puntos[,2], pch=18, col='purple')
}

#Calcula el número de puntos dado array bidimensional con 2 columnas de coordenadas, una para la x y otra para la y
calcularNumPuntos <- function(puntos){
  #Tenemos un array bidimensional de n coordenadas x y n coordenadas y por lo que tenemos una longitud 2n
  numPuntos <- length(puntos)/2 #Como tenemos 2n coordenadas tenemos n puntos
}#fin calcularNumPuntos


#Función que calcula el área de un triángulo dados sus tres vértices por fuerza bruta
estaALaDerecha <- function(A, B, C){
  return (det(matrix(c(A-B,A-C), nrow = 2, ncol=2)))
}#fin areaTriangulo

#Nos dan 3 o 4 puntos y devolvemos la envolvente conexa ordenada en sentido antihorario
envolventeConexaBase <- function(puntos){
  
  numPuntos <- calcularNumPuntos(puntos)
  envolvente <- matrix(c(0,0), ncol=2, byrow=T)
  
  for (i in 1:numPuntos-1){
    puntoA <- puntos[i,]
    puntoB <- punto[i+1,]
  } 
}

main <- function(){
  puntos = matrix( c(4,5 ,1,2, 1,8, 2,6), ncol=2, byrow=T)
  dibujarPuntos(puntos)
  print(puntos)
  puntos[1,] <- c(3,4)
  print(puntos)
  envolventeConexaBase(puntos)
}

main()