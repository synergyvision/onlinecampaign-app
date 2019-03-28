cate<- function(datos){
  
  y <- lapply(datos, class)
  t<- unlist(y, use.names = TRUE)
  
  d <- which((t == 'character') ||(t == 'factor') || (t == 'logical') )
  
  if (length(d)>0){
    j<- datos[,-d]
  }else{j<- datos}
  
  
  
  tam<- NULL
  
  
  for (i in 1:length(j)){
    
    p <- as.numeric(j[[i]])
    k <- summary(as.factor(p))
    tam[i]<- length(k)
    
  }
  
  w<- which(tam<= 15)
  jj<- j[,-w]
  return(jj)
  
}
