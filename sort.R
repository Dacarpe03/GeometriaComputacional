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

### Mergesort ###

mergesort<-function(x){
  n<-length(x)
  #Si hemos llegado al caso base (la longitud del vector es 1)
  if(n <= 1){
    #Lo devolvemos
    return(x)
  }#Si no
  #Dividimos el vector en dos mitades
  medio<-round(n/2)
  left<-x[1:medio]
  right<-x[(medio+1):n]
  #Hacemos mergesort en cada mitad
  left<-mergesort(left)
  right<-mergesort(right)
  #Si las mitades han quedado ordenadas las juntamos automáticamente
  if(left[length(left)] <= right[1])
    return(c(left, right))
  #Si no están ordenadas las juntamos ordenadas
  res<-merge(left, right)
  return(res)
}

#Función que junta dos vectores ordenados en uno también ordenado
merge<-function(left, right){
  result<-c(0)
  #Mientras que haya elementos en alguna de las dos mitades
  while((length(left) > 0) && (length(right) > 0)){
    #Si el elemento de la izquierda es mayor que el de la derecha, lo metemos al vector final
    if(left[1] <= right[1]){
      result<-c(result, left[1])
      left<-left[-1]
    }else{#Si no, lo hacemos al revés
      result<-c(result, right[1])
      right<-right[-1]
    }
  }
  
  #Metemos el vector que queda después del bucle
  if(length(left) > 0){
    result<-c(result, left)
  }
  
  if(length(right) > 0){
    result<-c(result, right)
  }
  #Devolvemos el resultado
  return(result[-1])
}

### QuickSort ###
quicksort<-function(x, low, high){
  if(low < high){
    #Hacemos una primera partición de la lista
    list_particion<-particion(x, low, high)
    #Obtenemos nuestro pivote, que será el punto por el que dividiremos el vector
    p<-list_particion[['p']]
    x<-list_particion[['x']]
    
    #Ordenamos la parte derecha del pivote
    x<-quicksort(x, low, p-1)
    #Ordenamos la parte izquierda del pivote
    x<-quicksort(x, p+1, high)
  }
  return(x)
}

particion<-function(x, low, high){
  #Cogemos un pivote
  pivot <- x[high]
  i <- low
  #Ponemos a la izquierda del pivote todos los elementos menores a él
  for(j in low:high){
    if(x[j] < pivot){
      #Si un elemento es menor que el pivote, lo pasamos a la izquierda
      x<-swap(x, i, j)
      i<-i+1
    }
  }
  x<-swap(x, i, high)
  return(list('x'=x, 'p'=i))
}

#Intercambia de posición los elementos en las posiciones i Y j
swap<-function(x, i, j){
  temp<-x[i]
  x[i]<-x[j]
  x[j]<-temp
  return(x)
}

# Función que crea el heap dado un array
heapbuilding<-function(arr){
  n <- length(arr)
  
  # Nos copiamos el array en un heap
  heap <- arr
  
  # Recorremos el array
  for(j in n:1){
    # Vamos insertando los elementos del array en el heap
    heap<-modifyheap(heap, j)
  }
  return(heap)
}

# Función de reorganización del heap al eliminar o meter un elemento
modifyheap<-function(heap, root_i){
  n <- length(heap)
  
  # Flag para indicar cuando acabar el while
  flag <- TRUE
  
  while(((root_i * 2) <= n) & flag){
    # Calculamos cuales serian los iondices de los hijos
    left_i <- root_i * 2
    right_i <- root_i * 2 + 1
    
    # Ponemos el flag false para parar el while en caso de que este el heap bien construido
    flag <- FALSE
    
    # Cogemos a los hijos del root actual
    son <- c(heap[left_i],heap[right_i])
    son <- son[!is.na(son)]
    
    # Buscamos los hijos en el heap y nos quedamos el menor
    min_ind = which.min(son)
    
    # Si el menor hijo es mayor que el root los intercambiamos y seguimos en el while para que se recoloquen el resto
    # También cambiamos de root al hijo menor
    if(heap[root_i] > son[min_ind]){
      flag <- TRUE
      
      heap_ind <- c(left_i,right_i)[min_ind]
      
      tmp <- heap[heap_ind]
      heap[heap_ind] <- heap[root_i]
      heap[root_i] <- tmp
      
      root_i <- heap_ind
    }
  }
  return(heap)
}

# Funcion que dado un heap lo devulve ordenado
heapsortutil<-function(heap){
  # Inicializamos la ordenacion
  sorted <- NULL
  
  n <- length(heap)
  # recorremos el heap entero
  while(n > 0){
    # Inseramos el primer  elemento del heap
    sorted <- c(sorted, heap[1])
    
    # Sacamos el elemento del heap
    n <- length(heap)
    heap[1] <- heap[n]
    heap <- heap[1:(n-1)]
    
    # Reorganizamos el heap  (siempre raíz el primer elemento)
    heap<-modifyheap(heap, root_i = 1)
    
    n <- n - 1
  }
  
  return(sorted)
}

heapsort<-function(arr){
  # Insertamos todos los elementos en el heap
  heap<-heapbuilding(arr)
  
  # Devolvemos el " heap ordenado "
  return(heapsortutil(heap))
}

dibujarGraficos <- function(tiempos){
  
  print(tiempos)
  casos <- (length(tiempos)/8)-1
  print(casos)
  longitudes <- rep(0,casos)
  bubbleRecursivo <- rep(0, casos)
  bubbleIterativo <- rep(0, casos)
  insercion <- rep(0, casos)
  seleccion <- rep(0, casos)
  mergesort <- rep(0, casos)
  quicksort <- rep(0, casos)
  heapsort <- rep(0, casos)
  
  for(i in 1:casos){
    longitudes[i] = tiempos[i+1,1]
    bubbleRecursivo[i] = tiempos[i+1,2]
    bubbleIterativo[i] = tiempos[i+1,3]
    insercion[i] = tiempos[i+1,4]
    seleccion[i] = tiempos[i+1,5]
    mergesort[i] = tiempos[i+1,6]
    quicksort[i] = tiempos[i+1,7]
    heapsort[i] = tiempos[i+1,8]
  }
  
  plot(longitudes, bubbleRecursivo, type="o", col="blue", pch="o", lty=1, ylim =c(0,40), ylab="Tiempo(s)", xlab="Elementos del vector")
  
  points(longitudes, bubbleIterativo, col="red", pch="*")
  lines(longitudes, bubbleIterativo, col="red",lty=2)
  
  points(longitudes, insercion, col="dark red",pch="+")
  lines(longitudes, insercion, col="dark red", lty=3)
  
  points(longitudes, seleccion, col="green",pch="+")
  lines(longitudes, seleccion, col="green", lty=4)
  
  points(longitudes, mergesort, col="orange",pch="+")
  lines(longitudes, mergesort, col="orange", lty=3)
  
  points(longitudes, quicksort, col="black",pch="+")
  lines(longitudes, quicksort, col="black", lty=3)
  
  points(longitudes, heapsort, col="purple",pch="+")
  lines(longitudes, heapsort, col="purple", lty=3)
  
  legend(x=0, y=40, legend=c("BubbleSort Recursivo", "BubbleSort Iterativo", "Insertion sort", "Selection sort", "Mergesort", "Quicksort", "Heapsort"),
         fill=c("blue", "red", "dark red", "green", "orange", "black", "purple") ,cex=0.5, text.font=4, bg='grey')
}


main <- function(){
  #Distintas longitudes de vectores
  longitudes <- c(10, 100, 200, 400, 500, 1000, 2500, 5000, 7500, 10000, 12500)
  tabla <- matrix(data=1:8, nrow=1)
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
    
    print("Mergesort")
    m <- system.time(mergesort(arrayDesordenado))
    print(m)
    
    print("Quicksort")
    q <- system.time(quicksort(arrayDesordenado, 1, l))
    print(q)
    
    print("Heapsort")
    h <- system.time(mergesort(arrayDesordenado))
    print(h)
    
    tabla <- rbind(tabla, c(l, br[3], b[3], i[3], s[3], m[3], q[3], h[3]))
  }
  
  dibujarGraficos(tabla)
}

main()
