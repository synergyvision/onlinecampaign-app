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

library(shiny)
library(shinydashboard)
library('readxl')
library("ggplot2")

ui <- dashboardPage( skin = "purple",
                     dashboardHeader(title=tags$img(src="img/vision.png", width=100)),
                     dashboardSidebar(
                       sidebarMenu(
                         h4("Campaña Online",style="text-align:center"),
                         menuItem("Introducción",tabName = 'intro',icon = icon("align-justify")),
                         menuItem("Datos",tabName = 'datos',icon = icon("folder-open")),
                         menuItem("ACP",tabName = 'acp',icon = icon("calculator")),
                         menuItem("Agrupación",tabName = 'agrup',icon = icon("th",lib = "glyphicon")),
                         menuItem("Resumen",tabName = 'resu',icon = icon("signal",lib = "glyphicon"))
                         
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
                                                 fluidRow(column(width=3,radioButtons(inputId = "metodo1",label = "Elegir método",selected = '',
                                                                                      choices = c("Matriz de Correlación","Matriz de Covarianza")
                                                 )
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
                                                   column(width=6,h3("Varianza Acumulada por cada componente"),plotOutput("imagen2"))
                                                 )
                                        ),
                                        tabPanel(h4("Variables Representativas"),fluidRow(box(solidHeader = TRUE,width=12,uiOutput("slid")))),
                                        tabPanel(h4("Proyecciones"),"Hola3")))
                         )
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
    if(input$metodo1=="Matriz de Correlación"){
    prcomp(z(),scale. = T)
  } else if(input$metodo1=="Matriz de Covarianza"){
    prcomp(z(),scale. = F)
  }
  })
  
  output$imagen2<-renderPlot({
    d1<-summary(pr())
    d2<-d1$importance
    d3<-d2[3,]
    d4<-unname(d3)
    d4<-d4*100
    
    
    ggplot(mapping=aes(x=1:ncol(z()),y=d4))+geom_line(colour='blue')+geom_point(colour="blue")+
      xlab("Componentes principales")+ylab("Varianzas Acumuladas")+
      scale_y_continuous(labels = function(x) paste0(x,"%"))+
      scale_x_continuous(breaks = 1:ncol(z()))
    
  })
  
  
  output$slid<-renderUI({
    sliderInput( inputId = "num",
                 label = "Seleccionar el número de Componentes con
                 los que desee quedarse:", value = 1, min = 1,
                 max = ncol(z()), step = 1
    )
  })
  

}

shinyApp(ui, server)