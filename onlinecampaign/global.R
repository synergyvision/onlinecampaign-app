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
ensure_version("forecast", "8.4")


library(shiny)
library(shinydashboard)
library('readxl')
library("ggplot2")
library('cluster')
library("factoextra")
library("forecast")
library(plotly)


