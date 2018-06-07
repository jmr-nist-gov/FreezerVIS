if (require(odbc) == FALSE) {
  install.packages("odbc")
}
if (require(DBI) == FALSE) {
  install.packages("DBI")
}
if (require(tidyverse) == FALSE) {
  install.packages("tidyverse")
}
if (require(shiny) == FALSE) {
  install.packages("shiny")
}
if (require(shinyjs) == FALSE) {
  install.packages("shinyjs")
}
if (require(plotly) == FALSE) {
  install.packages("plotly")
}
if (require(shinythemes) == FALSE) {
  install.packages("shinythemes")
}
library(odbc)
library(DBI)
suppressPackageStartupMessages(library(tidyverse))
library(shiny)
library(shinyjs)
library(plotly)
library(shinythemes)