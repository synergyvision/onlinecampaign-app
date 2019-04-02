bondad <- function(data){
  
  data <- as.data.frame(data)
  data <- apply(data, 2, as.numeric)
  Pval <- NULL
  
  for (i in 1:ncol(data)) {
    
    prueba <- shapiro.test(data[,i])
    Pval[i] <- round(prueba[[2]],5)
    
  }
  
  return(Pval)
  
}


