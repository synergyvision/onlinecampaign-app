



RS <- function(data1,nombre){
  
  
  posi <- which(colnames(data1) == nombre)
  
  colnames(data1)[posi] <- 'dependiente'
  modelo <- lm(dependiente ~. , data=data1)
  
  
  return(list(modelo, summary(modelo)))
  
}




