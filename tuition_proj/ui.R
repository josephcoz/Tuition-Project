# ui.R

# still not clear if these only need to be in server.R file...
library(shiny)
library(jsonlite)
library(shinyjs)
library(plotly)

# read data into vector (not dataframe, as is traditional)
schools <- scan(file = "./schools.csv", what = character(), sep=",")

ui <- fluidPage(
  #useShinyjs(),

  # school input box element
  selectInput('schoolname', 'School', c(Choose='', schools), selectize=TRUE),

  
  plotlyOutput("schoolplot", width = "80%"),
  fluidRow(
    column(2, uiOutput("toggleforecast", inline = TRUE)),
    column(2, uiOutput("toggletable", inline = TRUE))
  ),
  br(),
  br(),
  DT::dataTableOutput("table", width = "80%")
  
)


