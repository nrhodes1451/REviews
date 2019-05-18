library(shiny)
library(shinydashboard)
library(shinyjs)
library(R6)
library(lubridate)
library(plotly)
library(broom)
library(lmtest)
library(car)
library(tseries)
library(zoo)
library(rhandsontable)
library(tidyverse)

toproper <- function(x){
  x %>% strsplit(" ") %>% lapply(function(str){
    lapply(str, function(s){
      if(nchar(s)<4 && tolower(s)!= "for" && tolower(s) != "kit" &&
         tolower(s) != "1st" && tolower(s) != "2nd") {
        return(toupper(s))}
      else{
        return(paste0(toupper(substr(s, 1, 1)), tolower(substring(s, 2))))
      }
    }) %>% paste(collapse=" ")
  }) %>% unlist
}

global_options <- list(
  color = 'primary',
  charts = list(
    start_date = '2013-12-30',
    font = list(family = "sans-serif", size = 12),
    colors = list(
      purple='rgb(111,87,152)',
      blue='rgb(47,195,199)',
      grey="rgb(100,100,100")
  )
)

global <- list(
  "model_data" = readRDS("data/demo.RDS")
)

# Decomp objects
source("decomp.R")