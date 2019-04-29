shinyServer(function(input, output, session) {
  
  
###### Seccion datos  
  
  #### Aqui se cargan los datos
  
  data<-reactive({
    infile<-input$cargardatos
    if(is.null(infile)){
  
  return()
}
else{
  D<-as.data.frame(read_excel(infile$datapath))
  D<-na.omit(D)
  return(D)
}
})
########## Se muestran los datos
output$datos1<-renderDataTable({
  return(data())
},options=list(scrollX = TRUE,scrollY=300,searching=FALSE))



### Se actualiza la seccion de donde se seleccionan los datos
observe({
  vars<-names(data())
  updateCheckboxGroupInput(session, 'columnas', choices = vars)
})
###########################


############# Seccion ACP


####### Subseccion metodos

############## datos a usar para el metodo

z<-reactive({
  d<-data()[,c(input$columnas)]
  for(i in 1:ncol(d)) {
    d[,i]<-as.numeric(d[,i])
  }
  na.omit(d)
})


#### Matriz de correlacion
output$datos2<-renderTable({
  z<-na.omit(z())
  z1<-cor(z())
  return(z1)
},rownames = TRUE,digits = 4)

#### Matriz de covarianza
output$datos3<-renderTable({
  z<-na.omit(z())
  z1<-cov(z())
  return(z1)
},rownames = TRUE,digits = 4)


#### Se calculan las componentes principales con la funcion prcomp

pr<-reactive({
  if(is.null(input$metodo1)){
    NULL
  }
  else if(input$metodo1=="Matriz de Correlación"){
    prcomp(z(),scale. = T)
  } else if(input$metodo1=="Matriz de Covarianza"){
    prcomp(z(),scale. = F)
  }
})

### donde se muestra la cantidad de varianza que muestra cada componente

output$imagen1<-renderPlot({
  if(is.null(pr())){
    return()
  }
  else {
    m<-summary(pr())
    m1<-m$importance
    PVE<-m1[2,]
    PVE<-unname(PVE)
    PVE<-PVE*100
    
    comp<-c('Comp')
    nombres<-c()
    for(i in 1:ncol(z())){
      nombres[i]<-paste0(comp,sep=' ',i)
    }
    
    prueba<-data.frame(nombres,PVE)
    prueba$nombres<-factor(prueba$nombres,levels = prueba$nombres) #esto evita que el comp 10 aparezca en comp 2 y ordena los datos como quiero que aparezca en el plot
    
    ggplot(prueba,mapping=aes(x=nombres,y=PVE))+geom_bar(stat = 'identity',fill='darkblue')+geom_text(stat='identity',aes(label=scales::percent(PVE/100)),vjust=-0.3)+scale_y_continuous(labels = function(x) paste0(x, "%"))+xlab('Componentes principales')+ylab('Porcentaje de varianza')
  }
})


## Donde se muestra la varianza acumulada de las componentes

output$imagen2<-renderPlot({
  if(is.null(pr())){
    return()
  }
  else {
    d1<-summary(pr())
    d2<-d1$importance
    d3<-d2[3,]
    d4<-unname(d3)
    d4<-d4*100
    
    comp<-c('Comp')
    nombres<-c()
    for(i in 1:ncol(z())){
      nombres[i]<-paste0(comp,sep=' ',i)
    }
    
    prueba<-data.frame(nombres,d4)
    prueba$nombres<-factor(prueba$nombres,levels = prueba$nombres)
    
    ggplot(prueba,mapping=aes(x=nombres,y=d4,group=1))+geom_point(colour="blue")+
      xlab("Componentes principales")+ylab("Varianzas Acumuladas")+
      scale_y_continuous(labels = function(x) paste0(x,"%"))+stat_summary(geom = "line",col="blue")
    
  }
})





output$slid<-renderUI({
  sliderInput( inputId = "num",
               label = "Seleccionar el número de Componentes con
               los que desee quedarse:", value = 1, min = 1,
               max = ncol(z()), step = 1
  )
})

r<-reactive({
  as.matrix(pr()$rotation[,1:input$num])
})


w<-reactive({
  w1<-c()
  for(i in 1:ncol(r())){
    w1[i]<-rownames(r())[which.max(abs(r()[,i]))]
  }
  
  w2<-w1
  r1<-r()
  for(i in 1:length(w2)){
    while(duplicated(w2)[i]==TRUE){
      r1<-r1[-which(rownames(r1)==w2[i]),]
      w2[i]<-names(which.max(abs(r1[,i])))
    }
  }
  w2
})

v<-reactive({ 
  v1<-c()
  for (i in 1:length(w())){
    v1[i]<-r()[which(names(r()[,i])==w()[i]),i]
  }
  z1<-c()
  z1<-setNames(v1,w())
  z1
})

output$variable<-renderPrint({
  
  print(v())
})

cp<-reactive({
  cp1<-as.data.frame(pr()$x)
  cp1
})

output$imagen3<-renderPlot({
  ggplot(cp(),aes(x=cp()$PC1,y=cp()$PC2))+geom_point(col="darkblue")+xlab("PC1")+ylab("PC2")
})

output$imagen4<-renderPlot({
  ggplot(cp(),aes(x=cp()$PC2,y=cp()$PC1))+geom_point(col="darkblue")+xlab("PC2")+ylab("PC1")
})

output$colun<-renderUI({
  checkboxGroupInput(inputId = 'columnasgrupos',
                     label = 'Elegir las variables para la formación de grupos',
                     choices = c(input$columnas))
})

datascale<-reactive({
  d1<-z()[,c(input$columnasgrupos)]
  d2<-na.omit(d1)
  d3<-scale(d2)
  return(d3)
})

numbergroups<-reactive({
  wss<-c()
  wss[1]<-(nrow(datascale())-1)*sum(apply(datascale(),2,var))
  for(i in 2:15){
    wss[i]<-sum(kmeans(datascale(),center=i,nstart = 50)$withinss)
  }
  wss
})

output$elbow<-renderPlot({
  if(is.null(input$columnasgrupos)){
    return
  }
  else{
    ggplot(mapping = aes(x=1:15,y=numbergroups()))+geom_line(colour='darkblue')+geom_point(colour='darkblue',size=3)+xlab('Número de Clusters')+ylab('Varianza total inter-cluster')+scale_x_continuous(breaks = 1:15)
  }
})


datapam<-reactive({
  pam(datascale(),k=input$cantidadgrupos,stand = TRUE)
})

output$clusplot<-renderPlot({
  fviz_cluster(datapam(),ggtheme = theme_minimal(),ellipse.type = "euclid",star.plot=TRUE,stand = TRUE)
})

output$silueta<-renderPlot({
  fviz_silhouette(datapam(),ggtheme= theme_classic())
  
})

output$comp1<-renderPlot({
  ggplot(mapping=aes(x=cp()$PC1,y=cp()$PC2))+geom_point(aes(x=cp()$PC1,y=cp()$PC2,colour=palette()[datapam()$clustering]),show.legend = FALSE)+xlab("PC1")+ylab("PC2")
})

output$comp2<-renderPlot({
  ggplot(mapping=aes(x=cp()$PC2,y=cp()$PC1))+geom_point(aes(x=cp()$PC2,y=cp()$PC1,colour=palette()[datapam()$clustering]),show.legend = FALSE)+xlab("PC2")+ylab("PC1")
})


output$elecciongrupos<-renderUI({
  Grupo<-c('Grupo')
  grupos<-c()
  for(i in 1:input$cantidadgrupos){
    grupos[i]<-paste0(Grupo,sep=' ',i)
  }
  selectInput(inputId = "elecciongrupos2",
              label="Escoja el grupo a resumir",
              choices = setNames(1:input$cantidadgrupos,grupos),
              selected = NULL, 
              width = NULL)
})

clusterpam<-reactive({
  #cluster<-c('Cluster')
  # for(i in 1:input$cantidadgrupos){ 
  #   nam <- paste(cluster, i, sep = "")
  #   assign(nam,clust_list_pam[[i]])
  # }
  clust_list_pam<-lapply(sort(unique(datapam()$clustering)),function(x)data()[which(datapam()$clustering==x),])
  return(clust_list_pam[[as.numeric(input$elecciongrupos2)]])
})

clusterpamnumeric<-reactive({
  clust_list_pamnumeric<-lapply(sort(unique(datapam()$clustering)),function(x)z()[which(datapam()$clustering==x),])
  return(clust_list_pamnumeric[[as.numeric(input$elecciongrupos2)]])
})

output$holaprueba<-renderTable({
  h1<-do.call(cbind,lapply(clusterpamnumeric(),summary))
  h1<-round(h1,2)
  return(h1)
},rownames = TRUE)

output$ngruposss<-renderTable({
  m<-table(datapam()$clustering)
  m<-as.data.frame(m)
  colnames(m)<-c('Número del grupo','Tamaño del grupo')
  return(m)
},colnames = TRUE)

output$grupoelegido<-renderDataTable({
  return(clusterpam())
},options=list(scrollX = TRUE,scrollY=300,searching=FALSE))

output$colun1<-renderUI({
  selectInput(inputId = 'columnasgrupos1',
              label = 'Elegir la variable para el análisis de series temporales',
              choices = c(input$columnas))
  
})

output$gr1<-renderUI({
  Grupo<-c('Grupo')
  grupos<-c()
  for(i in 1:input$cantidadgrupos){
    grupos[i]<-paste0(Grupo,sep=' ',i)
  }
  selectInput(inputId = "elecciongrupos21",
              label="Escoja el grupo a estudiar",
              choices = setNames(1:input$cantidadgrupos,grupos),
              selected = NULL, 
              width = NULL)
})

clusterpam1<-reactive({
  
  clust_list_pam<-lapply(sort(unique(datapam()$clustering)),function(x)z()[which(datapam()$clustering==x),])
  return(clust_list_pam[[as.numeric(input$elecciongrupos21)]])
})

output$ser<-renderPlot({
  d<-input$columnasgrupos1
  # ggplot(clusterpam1(),aes(y=clusterpam1()[,d],x=seq(1,length(clusterpam1()[,d]))))+geom_line(col="darkblue")+xlab("")+ylab(d)
  ggtsdisplay(clusterpam1()[,d],main=d)
})

serie<-reactive({
  d<-input$columnasgrupos1
  s<-ts(clusterpam1()[,d],frequency = 7)
})

s1<-reactive({
  decompose(serie())
})

output$descom<-renderPlot({
  plot(s1())
})

f<-reactive({
  auto.arima(s1()$random)
})

output$modelo<-renderPrint({
  return(f())
})


output$fijado<-renderPlot({
  d<-input$columnasgrupos1
  z<-f()$fitted+s1()$trend+s1()$seasonal
  # 
  # plot(serie(),type="l",col="blue")
  # lines(z,col="green")
  
  ggplot(serie(),aes(y=serie(),x=seq(1,length(serie()))))+geom_line(col="black")+geom_line(aes(y=z),col="blue")+xlab("")
})

output$resi<-renderPlot({
  ggtsdisplay(f()$residuals,main="Residuales")
})

output$qq<-renderPlot({
  # qqnorm(f()$residuals,col="blue")
  # qqline(f()$residuals)
  ggplot(f()$residuals,aes(sample=f()$residuals))+stat_qq(color="blue")+stat_qq_line()
})

output$nrodepred<-renderUI({
  sliderInput( inputId = "num2",
               label = "Seleccionar la cantidad de datos a predecir:", value = 1, min = 1,
               max = nrow(clusterpam1()), step = 1
  )
})

output$predic<-renderPlot({
  arima.f1<-forecast(f(),h=input$num2)
  plot(arima.f1,col="black")
})
############---------------------------parte del server de glm

dat12 <- reactive({
  
  if(input$dat=="Originales"){
    
    return(data())
  }else if(input$dat=="Cluster"){
    return(clusterpamglm())
  }else{return(NULL)}
  
})







output$ngruposssglm<-renderTable({
  m<-table(datapam()$clustering)
  m<-as.data.frame(m)
  colnames(m)<-c('Número del grupo','Tamaño del grupo')
  return(m)
},colnames = TRUE)


vars<-reactive({1:input$cantidadgrupos})
observe({
  
  updateSelectInput(session, 'select', choices = vars())
})


clusterpamglm<-reactive({
  #cluster<-c('Cluster')
  # for(i in 1:input$cantidadgrupos){ 
  #   nam <- paste(cluster, i, sep = "")
  #   assign(nam,clust_list_pam[[i]])
  # }
  clust_list_pam<-lapply(sort(unique(datapam()$clustering)),function(x)data()[which(datapam()$clustering==x),])
  return(clust_list_pam[[as.numeric(input$select)]])
})

output$datMod <- renderDataTable({
  
  
  dat12()
  
},options=list(scrollX = TRUE,scrollY=300,searching=FALSE))



outVar3 = reactive({
  
  nombres <- colnames(dat12())
  
  
  
  nombres
})  


observe({
  updateSelectInput(session, "columns4",
                    choices = outVar3()
  )}) 



output$Histograma2 <- renderPlotly({ 
  posi <- which(colnames(dat12())== input$columns4 )
  
  
  
  if(is.factor(dat12()[,posi])){
    ggplot(dat12(),aes(x=dat12()[,posi]))+ geom_bar(position=position_dodge(), fill = "#FF3466")
    
    
  }else{
    
    ggplot(data=dat12(), aes(as.numeric(dat12()[,posi]))) + 
      geom_histogram( 
        col="red", 
        fill="green", 
        alpha = .2)
    
  }
  
  
  
})

source("www/categoria.R")

datosSC2<- reactive({
  
  cate(dat12())
  
  
})

source("www/bondad.R")

pvalExp1 <- reactive({
  
  bondad(datosSC2())
  
  
  
})



output$datatable12 <-renderDataTable({
  s <- pvalExp1()
  
  m <- as.data.frame(matrix(s,ncol = length(colnames(datosSC2()))))
  colnames(m) <- colnames(datosSC2())
  m
  
},options = list(scrollX=T,scrollY=300))


varMod2 <- reactive({
  d<- outVar3()
  e<- which(d == input$columns4)
  f<- d[-e]
  f
  
})

observe( updateCheckboxGroupInput(session,"selec2",  choices = varMod2() ))

source("www/links.R")

observe({
  updateRadioButtons(session, "enlace",label = "Enlace",
                     if(input$selec3==1){ choices =Linkbinomial}
                     else if(input$selec3==2){choices = Linkgaussian}
                     else if(input$selec3==3){choices = LinkGamma}
                     else if(input$selec3==4){choices = Linkinverse.gausian}
                     else if(input$selec3==5){choices = Linkpoisson}
                     else if(input$selec3==6){choices = Linkquasi}
                     else if(input$selec3==7){choices = Linkquasibinomial}
                     else if(input$selec3==8){choices = Linkquasipoisson}
                     
                     
                     
  )}) 


source("www/gmlMul.R")

modelo2 <- reactive({
  
  if(length(input$selec2)==0){"Debe seleccionar variables"}else{
    if(input$selec3==1){       glmmulti(dat12()[c(input$columns4,input$selec2)],input$columns4,binomial,input$enlace)}
    else if(input$selec3==2){glmmulti(dat12()[c(input$columns4,input$selec2)],input$columns4,gaussian,input$enlace)}
    else if(input$selec3==3){glmmulti(dat12()[c(input$columns4,input$selec2)],input$columns4,Gamma,input$enlace)}
    else if(input$selec3==4){glmmulti(dat12()[c(input$columns4,input$selec2)],input$columns4,inverse.gaussian,input$enlace)}
    else if(input$selec3==5){glmmulti(dat12()[c(input$columns4,input$selec2)],input$columns4,poisson,input$enlace)}
    else if(input$selec3==6){glmmulti(dat12()[c(input$columns4,input$selec2)],input$columns4,quasi,input$enlace)}
    else if(input$selec3==7){glmmulti(dat12()[c(input$columns4,input$selec2)],input$columns4,quasibinomial,input$enlace)}
    else if(input$selec3==8){glmmulti(dat12()[c(input$columns4,input$selec2)],input$columns4,quasipoisson,input$enlace)}
  }
  
})

source("www/sumGlm.R")

output$grafi2 <- renderPlot({ 
  gra(modelo2()[[1]])
})


resGLM <- reactive({
  
  resuglm(modelo2()[[1]])
  
})

output$summar2 <- renderDataTable({
  
  resGLM()[[2]]
  
})


output$coeficien2 <- renderDataTable({
  
  resGLM()[[1]]
  
})


####----------------------------------Fin del server de glm


  
})