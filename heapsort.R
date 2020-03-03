# Funcion que crea el heap dado un array
heapbuilding<-function(arr){
  n <- length(arr)
  
  # Nos copiamos el array en un heap
  heap <- arr
  
  # Recorremos el array
  for(j in n:1){
    # Vamos insetando los elementos del array en el heap
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