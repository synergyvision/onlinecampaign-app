glmmulti <- function(data3,nombre,familia,enlace){
  
  data3 <- as.data.frame(data3)
  
  data3 <- apply(data3, 2, as.numeric)
  
  data3 <- as.data.frame(data3)
  
  
  pos <- which(colnames(data3) == nombre)
  
  colnames(data3)[pos] <- 'dependiente'
  
  modelo <- glm(dependiente ~. , family = familia(link = enlace), data = data3)
  
  return(list(modelo, summary(modelo)))
}