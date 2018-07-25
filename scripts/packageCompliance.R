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
if (require(ggthemes) == FALSE) {
  install.packages("ggthemes")
}
if (require(shinythemes) == FALSE) {
  install.packages("shinythemes")
}
if (require(DT) == FALSE) {
  install.packages("DT")
}
if (require(scales) == FALSE) {
  install.packages("scales")
}
if (require(yaml) == FALSE) {
  install.packages("yaml")
}
library(odbc)
library(DBI)
suppressPackageStartupMessages(library(tidyverse))
library(shiny)
library(shinyjs)
library(plotly)
library(shinythemes)
library(ggthemes)
library(DT)
library(scales)