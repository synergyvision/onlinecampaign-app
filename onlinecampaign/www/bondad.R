bondad <- function(data){
  
  
  Pval <- NULL
  
  for (i in 1:length(data)) {
    
    prueba <- shapiro.test(data[[i]])
    Pval[i] <- prueba[[2]]
    
  }
  
  return(Pval)
  
}
bondad(iris[-5])
