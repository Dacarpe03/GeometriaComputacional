### Mergesort ###

mergesort<-function(x){
  n<-length(x)
  if(n <= 1){
    return(x)
  }
  medio<-round(n/2)
  left<-x[1:medio]
  right<-x[(medio+1):n]
  left<-mergesort(left)
  right<-mergesort(right)
  if(left[length(left)] <= right[1])
    return(c(left, right))
  res<-merge(left, right)
  return(res)
}

merge<-function(left, right){
  result<-c(0)
  while((length(left) > 0) && (length(right) > 0)){
    if(left[1] <= right[1]){
      result<-c(result, left[1])
      left<-left[-1]
    }else{
      result<-c(result, right[1])
      right<-right[-1]
    }
  }
  if(length(left) > 0){
    result<-c(result, left)
  }
  if(length(right) > 0){
    result<-c(result, right)
  }
  return(result[-1])
}

### QuickSort ###

quicksort<-function(x, low, high){
  if(low < high){
    list_particion<-particion(x, low, high)
    p<-list_particion[['p']]
    x<-list_particion[['x']]
    x<-quicksort(x, low, p-1)
    x<-quicksort(x, p+1, high)
  }
  return(x)
}

particion<-function(x, low, high){
  pivot <- x[high]
  i <- low
  for(j in low:high){
    if(x[j] < pivot){
      x<-swap(x, i, j)
      i<-i+1
    }
  }
  x<-swap(x, i, high)
  return(list('x'=x, 'p'=i))
}

swap<-function(x, i, j){
  temp<-x[i]
  x[i]<-x[j]
  x[j]<-temp
  return(x)
}

# Funcion que crea el heap dado un array
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

# Funcion de reorganizacion del heap al eliminar o meter un elemento
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
    # Tambien cambiamos de root al hijo menor
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
    
    # Reorganizamos el heap  (siempre raiz el primer elemento)
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

### Medir tiempos ###

medir_tiempos<-function(lengths){
  tiempos_mergesort<-c(0)
  tiempos_quicksort<-c(0)
  tiempos_heapsort <-c(0)
  for(l in lengths){
    min<-(-(l*10))
    max<-(l*10)
    
    print(paste('Generando vector de ',l,' enteros entre ', min ,' y ', max)) 
    x<-round(runif(l, min, max))
    print(paste('Generado'))
    
    print(paste('Midiendo tiempo mergesort'))
    time <- system.time(mergesort(x))[3]
    print(paste('Tiempo medido mergesort: ', time, 's'))
    tiempos_mergesort<-c(tiempos_mergesort, time)
    
    print(paste('Midiendo tiempo quicksort'))
    time <- system.time(quicksort(x, 1, l))[3]
    print(paste('Tiempo medido quicksort: ', time, 's'))
    tiempos_quicksort<-c(tiempos_quicksort, time)
    
    print(paste('Midiendo tiempo heapsort'))
    time <- system.time(heapsort(x))[3]
    print(paste('Tiempo medido heapsort: ', time, 's'))
    tiempos_heapsort<-c(tiempos_heapsort, time)
    
    print('#########################################')
  }
  return(list("mergesort"=tiempos_mergesort[-1], 
              "quicksort"=tiempos_quicksort[-1],
              "heapsort"=tiempos_heapsort[-1]))
}