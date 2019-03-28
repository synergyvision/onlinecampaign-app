correl <- function(data,name){
  
  co <- as.data.frame(cor(data))
  
  po<-which(name==colnames(co))
  
  return(co[,po])
  
}






