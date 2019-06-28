# ui.R

# still not clear if these only need to be in server.R file...
# library(shiny)
# library(jsonlite)
library(shinyjs)
library(ggplot2)

# read data into vector (not dataframe, as is traditional)
schools <- scan(file = "./schools.csv", what = character(), sep=",")

ui <- fluidPage(
  #useShinyjs(),

  # school input box element
  selectInput('schoolname', 'School', c(Choose='', schools), selectize=TRUE),

  
  plotOutput("schoolplot", width = "100%"),
  uiOutput("choices", inline = TRUE)
  
  
)


