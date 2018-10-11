#Aplicación para la agrupación de palabras claves usadas en el Sistema adWords
#Modelos ARIMA.
#Estadística Multivariada.
#ACP
#Regresion lineal.

library(shiny)
library(shinydashboard)

ui <- dashboardPage( skin = "purple",
                     dashboardHeader(title=tags$img(src="img/vision.png", width=100)),
                     dashboardSidebar(
                       sidebarMenu(
                         h4("Campaña Online",style="text-align:center"),
                         menuItem("Introducción",icon = icon("align-justify")),
                         menuItem("Datos",icon = icon("folder-open")),
                         menuItem("ACP",icon = icon("calculator")),
                         menuItem("Agrupación",icon = icon("chart-pie",lib = "font-awesome")),
                         menuItem("Resumen",icon = icon("chart-bar",lib = "font-awesome"))
                         
                       )
                     ),
                     dashboardBody()
)

server <- function(input, output) { }

shinyApp(ui, server)