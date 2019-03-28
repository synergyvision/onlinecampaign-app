gra <- function(modelo){
  
  par(mfrow=c(2,2)) 
  
  plot(modelo)
  
}

res <- function(modelo){
  
  q<- summary(modelo)
  
  
  e<- as.data.frame(q$coefficients)
  nom <-c('Coeficiente', 'Desviacion Estandar', 'Test de Hipotesis', 'P-Valor')
  colnames(e) <- nom
  row.names(e)[1] <- 'Pto de corte eje Y'
  
  a<- c(q$sigma,q$r.squared,q$adj.r.squared,q$fstatistic)
  b<- c('Desviacion estandar del error', 'R-Cuadrado', 'R-Cuadrado ajustado','Estadistico F', 'Num. de variables independientes', 'Grados de libertad')
  k<- as.data.frame(b)
  k1<- as.data.frame(a)
  k2<- cbind(k,k1)
  colnames(k2)<- c('','')
 
  
  return(list(e,k2))
  
}
modelo <- lm(Sepal.Length~.,data=iris)
res(modelo)[1]
