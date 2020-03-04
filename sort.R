#Bubblesort recursivo

#Función que devuelve TRUE si el vector de dos posiciones que recibe está ordenado
larger = function(pair)
{
  if(pair[1] > pair[2]) return(TRUE)
  else return(FALSE)
}#fin larger

#Función que da la vuelta a un vector de dos posiciones si no está ordenado
swapIfLarger <- function(pair){
  if(larger(pair))
  {
    return(rev(pair))
  }
  else
  {
    return(pair)
  }
}#fin swapIfLarger

#Función que ordena un vector
swapPass = function(vec){
  #Si un elemento es mayor que su adyacente por la derecha lo intercambia
  for(i in seq(1, length(vec)-1))
  {
    vec[i:(i+1)] = swapIfLarger(vec[i:(i+1)])
  }
  return(vec)
}

#Función del método bubblesort implemetado de forma recursiva
bubblesortRecursivo = function(vec){
  new_vec = swapPass(vec)
  #Si el vector está ordenado lo devolvemos
  if(isTRUE(all.equal(vec, new_vec)))
  {
    return(new_vec)
  }
  #Si no, volvemos a intentar que queden ordenados todos los elementos del vector
  else {
    return(bubblesortRecursivo(new_vec))
  }
}


#Bubblesort iterativo
bubblesort<-function(arr){
  n <- length(arr)
  #Por cada elemento del array
  for(i in 1:(n-1)){
    #vamos comparando por pares los elementos e intercambiadolos de posicion si el primero es mayor que el segundo
    for(j in 1:(n-i)){
      if(arr[j] > arr[j+1]){
        temp<-arr[j]
        arr[j]<-arr[j+1]
        arr[j+1]<-temp
      }
    }
  }
  return(arr)
}#fin bubblesort

#Inserción
insertionsort<-function(arr){
  n <- length(arr)
  #Comenzamos la ordenación desde el segundo elemento
  i <- 2
  while(i <= n){
    j <- i
    #Buscamos cual es la posición del elemento i del vector
    while(j > 1){
      #Recorremos desde la posición actual hasta la primera
      #buscando un elemento menor que el elemento que estamos moviendo 
      if(arr[j-1] > arr[j]){
        temp<-arr[j]
        arr[j]<-arr[j-1]
        arr[j-1]<-temp
      }
      j <- j - 1
    }
    #Avanzamos en el vector para coger el siguiente elemento a colocar
    i <- i + 1
  }
  return(arr)
}

#Selección
selectionsort<-function(arr){
  n <- length(arr)
  #Para cada posición del array
  for(i in 1:(n-1)){
    #Calculamos cuál es el mínimo desde la posición i hasta el final del vector
    min<-i
    for(j in (i+1):n){
      if(arr[j] < arr[min]){
        min = j
      }
    }
    #Insertamos en la posición i el mínimo que hemos encontrado en esta iteración
    if(min != i){
      temp<-arr[i]
      arr[i]<-arr[min]
      arr[min]<-temp
    }
  }
  
  return(arr)
}#fin Selección

dibujarGraficos <- function(tiempos){
  
  print(tiempos)
  casos <- (length(tiempos)/5)-1
  print(casos)
  longitudes <- rep(0,casos)
  bubbleRecursivo <- rep(0, casos)
  bubbleIterativo <- rep(0, casos)
  insercion <- rep(0, casos)
  seleccion <- rep(0, casos)
  
  for(i in 1:casos){
    longitudes[i] = tiempos[i+1,1]
    bubbleRecursivo[i] = tiempos[i+1,2]
    bubbleIterativo[i] = tiempos[i+1,3]
    insercion[i] = tiempos[i+1,4]
    seleccion[i] = tiempos[i+1,5]
  }
  
  plot(longitudes, bubbleRecursivo, type="o", col="blue", pch="o", lty=1, ylim=c(0,20), ylab="Tiempo(s)", xlab="Elementos del vector")
  
  points(longitudes, bubbleIterativo, col="red", pch="*")
  lines(longitudes, bubbleIterativo, col="red",lty=2)
  
  points(longitudes, insercion, col="dark red",pch="+")
  lines(longitudes, insercion, col="dark red", lty=3)
  
  points(longitudes, seleccion, col="green",pch="+")
  lines(longitudes, seleccion, col="green", lty=4)
  
  legend(x=0,y=20,legend=c("BubbleSort Recursivo", "BubbleSort Iterativo", "Insertion sort", "Selection sort"),
         fill=c("blue", "red", "dark red", "green") ,cex=0.8, text.font=4, bg='grey')
}


main <- function(){
  #Distintas longitudes de vectores
  longitudes <- c(10, 100, 200, 400, 1000, 2500, 5000, 7500, 10000, 12000, 15000)
  tabla <- matrix(data=1:5, nrow=1)
  for(i in 1:length(longitudes)){
    print("-----")
    l <- longitudes[i]
    print(longitudes[i])
    arrayDesordenado <- round(runif(longitudes[i], 0, 100))
    if(l > 1000){
      br <- c(100,100,100)
    }else{
      br <- system.time(bubblesortRecursivo(arrayDesordenado))
      print("Bubblesort Recursivo")
      print(br)
    }
    b <- system.time(bubblesort(arrayDesordenado))
    print("Bubblesort Iterativo")
    print(b)
    i <- system.time(insertionsort(arrayDesordenado))
    print("Insertionsort")
    print(i)
    s <- system.time(selectionsort(arrayDesordenado))
    print("Selectionsort")
    print(s)
    tabla <- rbind(tabla, c(l, br[3], b[3], i[3], s[3]))
  }
  
  dibujarGraficos(tabla)
}

main()
