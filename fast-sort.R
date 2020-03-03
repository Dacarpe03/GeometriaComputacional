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

### Medir tiempos ###

medir_tiempos<-function(lengths){
  tiempos_mergesort<-c(0)
  tiempos_quicksort<-c(0)
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
    
    print('#########################################')
  }
  return(list("mergesort"=tiempos_mergesort[-1], 
              "quicksort"=tiempos_quicksort[-1]))
}