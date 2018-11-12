#Aplicación para la agrupación de palabras claves usadas en el Sistema adWords
#Modelos ARIMA.
#Estadística Multivariada.
#ACP
#Regresion lineal.

ensure_version <- function(pkg, ver = "0.0") {
  if (system.file(package = pkg)  == "" || packageVersion(pkg) < ver)
    install.packages(pkg)
}

ensure_version("shiny", "1.1.0")
ensure_version("readxl", "1.1.0")
ensure_version("shinydashboard", "0.7.0")
ensure_version("ggplot2", "3.0.0")
ensure_version("cluster", "2.0.6")
ensure_version("factoextra", "1.0.5")

library(shiny)
library(shinydashboard)
library('readxl')
library("ggplot2")
library('cluster')
library("factoextra")



ui <- dashboardPage( skin = "purple",
                     dashboardHeader(title=tags$img(src="img/vision.png", width=100)),
                     dashboardSidebar(
                       sidebarMenu(
                         h4("Campaña Online",style="text-align:center"),
                         menuItem("Introducción",tabName = 'intro',icon = icon("align-justify")),
                         menuItem("Datos",tabName = 'datos',icon = icon("folder-open")),
                         menuItem("ACP",tabName = 'acp',icon = icon("calculator")),
                         menuItem("Agrupación",tabName = 'agrup',icon = icon("th",lib = "glyphicon")),
                         menuItem("Resumen",tabName = 'resu',icon = icon("signal",lib = "glyphicon")),
                         menuItem("Series Temporales",tabName = 'series',icon = icon("chart-area",lib = "font-awesome"))
                         
                       )
                     ),
                     dashboardBody(
                       tabItems(
                         tabItem(tabName = 'datos',
                                 fluidRow(box(width = 3,
                                              title = 'Cargar Datos',
                                              fileInput(inputId = 'cargardatos',
                                                        label = "Seleccionar archivo:",
                                                        buttonLabel = "Buscar...",
                                                        placeholder = "Aun no seleccionas el archivo..."),
                                              tags$hr(),
                                              p('Cargar datos en formato .xlsx'),
                                              
                                              checkboxGroupInput(inputId = 'columnas',
                                                                 label = 'Seleccione de lo siguiente las columnas numéricas que desee usar para los análisis posteriores',
                                                                 choices = ''
                                              )
                                 ),
                                 box(width = 9,
                                     h3('Datos:'),
                                     dataTableOutput('datos1')))
                         ),
                         tabItem(tabName = "acp",
                                 fluidRow(tabBox(width = 12,
                                        title = "",id="tab1",
                                        tabPanel(h4("Métodos"),
                                                 fluidRow(column(width=3,fluidRow(column(width=12,radioButtons(inputId = "metodo1",label = "Elegir método",selected = '',
                                                                                      choices = c("Matriz de Correlación","Matriz de Covarianza")
                                                 ))),fluidRow(box(title = "Observación",width = 12, solidHeader = TRUE,status = "primary",'La matriz de correlación se usa cuando los datos no son dimensionalmente homogéneos o el orden de magnitud de las variables no es el mismo, en cambio, la matriz de covarianza se usa cuando los valores son medios similares',collapsible = TRUE,collapsed =TRUE))
                                                 ),
                                                 conditionalPanel(condition = "input.metodo1=='Matriz de Correlación'",column(width = 9,h3("Matriz de Correlación"),div(style='overflow-x: scroll',
                                                                                                                                                                        tableOutput("datos2")
                                                 ))
                                                 ),
                                                 conditionalPanel(condition = "input.metodo1=='Matriz de Covarianza'",column(width = 9,h3("Matriz de Covarianza"),div(style='overflow-x: scroll',
                                                                                                                                                                      tableOutput("datos3")
                                                 ))
                                                 )
                                                 ),
                                                 fluidRow(
                                                   column(width=6,h3("Porcentaje de varianza por componente principal"),plotOutput("imagen1")),
                                                   column(width=6,h3("Porcentaje de varianza Acumulada por cada componente"),plotOutput("imagen2"))
                                                 )
                                        ),
                                        tabPanel(h4("Variables Representativas"),fluidRow(box(solidHeader = TRUE,width=12,uiOutput("slid"))),
                                                 fluidRow(box(verbatimTextOutput("variable"),title = "Variables Representativas de las Componentes Seleccionadas",width = 12,status = "primary")),
                                                 fluidRow(box(title = "Observación",width = 8,solidHeader = TRUE,status = "primary","Se recomienda usar el número de componenentes que representen un 80% de variabilidad de los datos. El porcentaje de variabilidad de las componentes principales lo observamos anteriormente.",collapsible = TRUE,collapsed =TRUE),
                                                          box(background = 'purple',width=4,div(img(src="img/vision.png", width=200),style="text-align: center;")))),

                                        tabPanel(h4("Proyecciones"),fluidRow(column(width = 5,h3('Componente 1 Vs. Componente 2'),plotOutput('imagen3')),column(width = 5,h3('Componente 2 Vs. Componente 1'),plotOutput('imagen4'))))))
                         ),
                         tabItem(tabName = 'agrup',
                                 fluidRow(tabBox(width = 12,
                                                 title = '',id='tab2',
                                                 tabPanel(h4('Elección de grupos'),fluidRow(column(width=3,fluidRow(box(width = 12,title = 'Variables Númericas',uiOutput('colun')))
                                                                                                   ),
                                                                                            column(width = 4,h3("Diagrama de Codo"),plotOutput("elbow")),
                                                                                            column(width = 4,sliderInput(inputId = 'cantidadgrupos',label = 'Elija la cantidad de grupos a formar:',min = 2,max = 15,value = 2),
                                                                                                   fluidRow(h4('Agrupación'),plotOutput('clusplot')))),
                                                          fluidRow(box(width = 3,title = 'Observación',solidHeader = TRUE,status = "primary","Se recomienda usar las variables representativas que se consideran en el Análisis de Componentes Principales",collapsible = TRUE,collapsed =TRUE),box(width = 4,title = 'Observación',solidHeader = TRUE,status = "primary","El diagrama de codo recomienda el número apropiado de grupos a usar para la agrupación",collapsible = TRUE,collapsed =TRUE))),
                                                 tabPanel(h4('Resultados'),fluidRow(column(width = 4,h3('Método de la Silueta'),plotOutput('silueta')),
                                                                                    column(width = 4,h3('Componente 1 Vs. Componente 2'),plotOutput('comp1')),
                                                                                    column(width = 4,h3('Componente 2 Vs. Componente 1'),plotOutput('comp2'))))))),
                         tabItem(tabName = 'resu',
                                 fluidRow(
                                   box(width = 6,uiOutput('elecciongrupos'),
                                       title = 'Resumen de los grupos resultantes',status = 'primary',solidHeader = TRUE,
                                       verbatimTextOutput('holaprueba')),
                                   box(width = 6,h3('Datos del grupo seleccionado:'),dataTableOutput('grupoelegido')))),
                         tabItem(tabName = 'series',
                                 fluidRow(tabBox(width = 12,
                                                 title = '',id='tab3',
                                                 tabPanel(h4('Elección de Variables'),fluidRow(column(width=3,fluidRow(box(width = 12,title = 'Variables Númericas',uiOutput('colun1'))),
                                                               fluidRow(box(width = 12,title = "Grupo a analizar",uiOutput("gr1")))                                       
                                                 )),fluidRow(column(width = 9,h3("Serie Temporal",plotOutput("ser"))))))))
                         )

                       )
                     )

server <- function(input, output,session) {
  
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
  
  output$datos1<-renderDataTable({
    return(data())
  },options=list(scrollX = TRUE,scrollY=300,searching=FALSE))
  
  observe({
    vars<-names(data())
    updateCheckboxGroupInput(session, 'columnas', choices = vars)
  })
  
  z<-reactive({
    d<-data()[,c(input$columnas)]
    for(i in 1:ncol(d)) {
      d[,i]<-as.numeric(d[,i])
    }
    na.omit(d)
  })

  
  
  output$datos2<-renderTable({
    z<-na.omit(z())
    z1<-cor(z())
    return(z1)
  },rownames = TRUE,digits = 4)
  
  output$datos3<-renderTable({
    z<-na.omit(z())
    z1<-cov(z())
    return(z1)
  },rownames = TRUE,digits = 4)
  
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
  ggplot(mapping = aes(x=1:15,y=numbergroups()))+geom_line(colour='darkblue')+geom_point(colour='darkblue',size=3)+xlab('Número de Clusters')+ylab('Varianza total inter-cluster')+scale_x_continuous(breaks = 1:15)
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

output$holaprueba<-renderPrint({
  summary(clusterpam())
})

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
  
  clust_list_pam<-lapply(sort(unique(datapam()$clustering)),function(x)data()[which(datapam()$clustering==x),])
  return(clust_list_pam[[as.numeric(input$elecciongrupos21)]])
})

output$ser<-renderPlot({
  d<-input$columnasgrupos1
  ggplot(clusterpam1(),aes(y=clusterpam1()[,d],x=seq(1,length(clusterpam1()[,d]))))+geom_line(col="darkgreen")+xlab("")+ylab("Impresiones")
})



}


shinyApp(ui, server)