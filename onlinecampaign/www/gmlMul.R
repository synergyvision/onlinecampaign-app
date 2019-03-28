glmmulti <- function(data3,nombre,familia,enlace){
  
  
  pos <- which(colnames(data3) == nombre)
  
  colnames(data3)[pos] <- 'dependiente'
  
  modelo <- glm(dependiente ~. , family = familia(link = enlace), data = data3)
  
  return(list(modelo, summary(modelo)))
}