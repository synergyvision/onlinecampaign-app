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
                                 box(width = 3,
                                     title = 'Cargar Datos',
                                     fileInput(inputId = 'cargardatos',
                                               label = "Seleccionar archivo:",
                                               buttonLabel = "Buscar...",
                                               placeholder = "Aun no seleccionas el archivo..."),
                                     tags$hr(),
                                     p('Cargar datos en formato .xlsx')
                                 ),
                                 box(width = 9,
                                     h3('Datos:'),
                                     dataTableOutput('datos1'))
                         )
                       )
                     )
)

server <- function(input, output) {
  
  data<-reactive({
    infile<-input$cargardatos
    if(is.null(infile)){
      return()
    }
    else{
      D<-as.data.frame(read_excel(infile$datapath))
      D<-na.omit(D)
    }
  })
  
  output$datos1<-renderDataTable({
    return(data())
  },options=list(scrollX = TRUE,scrollY=300,searching=FALSE))
  
}

shinyApp(ui, server)