resuglm <- function(modelo){
  
  q<- summary(modelo)
  
  e<- as.data.frame(q$coefficients)
  nom <-c('Coeficiente', 'Desviacion Estandar', 'Test de Hipotesis', 'P-Valor')
  colnames(e) <- nom
  
  a<- c(q$null.deviance,q$df.null,q$deviance,q$df.residual,q$aic,q$dispersion,q$iter)
  b<- c('Desviacion nula','Grados de libertad de la DN','Desviacion residual',
        'Grados de libertad dela DR','AIC','Parametro de dispersion','Num. de iteraciones de Fisher') 
  k<- as.data.frame(b)
  k1<- as.data.frame(a)
  k2<- cbind(k,k1)
  colnames(k2)<- c('','')
  
  return(list(e,k2))
  
}



