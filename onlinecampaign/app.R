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


library(shiny)
library(shinydashboard)
library('readxl')

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
                                 tabBox(width = 12,
                                        title = "",id="tab1",
                                        tabPanel(h4("Métodos"),
                                                 fluidRow(column(width=3,radioButtons(inputId = "metodo1",label = "Elegir método",selected = '',
                                                                                      choices = c("Matriz de Correlación","Matriz de Covarianza")
                                                 )
                                                 ),
                                                 conditionalPanel(condition = "input.metodo1=='Matriz de Correlación'",column(width = 9,h3("Matriz de Correlación"),tableOutput("datos2"))
                                                 ),
                                                 conditionalPanel(condition = "input.metodo1=='Matriz de Covarianza'",column(width = 9,h3("Matriz de Covarianza"),tableOutput("datos3"))
                                                 )
                                                 ),
                                                 fluidRow(
                                                   column(width=6,h3("Porcentaje de varianza por componente principal"),plotOutput("imagen1")),
                                                   column(width=6,h3("Varianza Acumulada por cada componente"),plotOutput("imagen2"))
                                                 )
                                        ),
                                        tabPanel(h4("Variables Representativas"),"Hola2"),
                                        tabPanel(h4("Proyecciones"),"Hola3")))
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
    d
  })

  
  output$datos2<-renderTable({
    z<-na.omit(z())
    z1<-cor(z())
    return(z1)
  },rownames = TRUE,digits = 4)
  
  output$datos3<-renderTable({
    return(head(z()))
  })
  
}

shinyApp(ui, server)