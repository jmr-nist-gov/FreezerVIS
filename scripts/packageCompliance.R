if (require(odbc) == FALSE) {
  install.packages("odbc")
  library(odbc)
}
if (require(DBI) == FALSE) {
  install.packages("DBI")
  library(DBI)
}
if (require(tidyverse) == FALSE) {
  install.packages("tidyverse")
  suppressPackageStartupMessages(library(tidyverse))
}
if (require(shiny) == FALSE) {
  install.packages("shiny")
  library(shiny)
}
if (require(shinyjs) == FALSE) {
  install.packages("shinyjs")
  library(shinyjs)
}
if (require(plotly) == FALSE) {
  install.packages("plotly")
  library(plotly)
}