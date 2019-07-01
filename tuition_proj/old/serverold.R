library(shiny)
library(jsonlite)
library(shinyjs)
library(ggplot2)
library(reshape2)
library(plotly)

server <- function(input, output, session) {
  
  # maintain school search box
  output$out4 <- renderPrint(input$schoolname)

  # react when school name is changed
  observeEvent(input$schoolname, {
    # tryCatch prevents initial error when program attempts to read in data
    #  before a school has been selected
    tryCatch({
      
      # initialize vector of checkbox input choices
      choices <- NULL
      
      # construct path to data source for selected school
      schooldatapath <- paste0("./data/",input$schoolname, ".json")
    
      # use jsonlite to read json data in path
      schooldata <<- fromJSON(schooldatapath)
      
      # check if a category exists for a certain school:
      #  if the category exists: 
      #   change it to numeric AND
      #   add it to the checkbox input choices
      if (!is.null(schooldata$tuition_and_fees_in_state) && !is.na(schooldata$tuition_and_fees_in_state)) {
        schooldata$tuition_and_fees_in_state <- as.numeric(schooldata$tuition_and_fees_in_state)
        choices <- c(choices, "In State" = "tuition_and_fees_in_state")
      } 
      if (!is.null(schooldata$tuition_and_fees_out_of_state) && !is.na(schooldata$tuition_and_fees_out_of_state)) {
        schooldata$tuition_and_fees_out_of_state <- as.numeric(schooldata$tuition_and_fees_out_of_state)
        choices <- c(choices, "Out of State" = "tuition_and_fees_out_of_state")
      } 
      if (!is.null(schooldata$tuition_fees_room_and_board_in_state) && !is.na(schooldata$tuition_fees_room_and_board_in_state)) {
        schooldata$tuition_fees_room_and_board_in_state <- as.numeric(schooldata$tuition_fees_room_and_board_in_state)
        choices <- c(choices, "In State + Room & Board" = "tuition_fees_room_and_board_in_state")
      } 
      if (!is.null(schooldata$tuition_fees_room_and_board_out_of_state) && !is.na(schooldata$tuition_fees_room_and_board_out_of_state)) {
        schooldata$tuition_fees_room_and_board_out_of_state <- as.numeric(schooldata$tuition_fees_room_and_board_out_of_state)
        choices <- c(choices, "Out of State + Room & Board" = "tuition_fees_room_and_board_out_of_state")
      } 
      if (!is.null(schooldata$room_and_board)) {
        schooldata$room_and_board <- as.numeric(schooldata$room_and_board)
        choices <- c(choices, "Room & Board" = "room_and_board")
      }
      # convert year to an integer
      schooldata$year_pub <- as.integer(schooldata$year_pub)
      
      # automatically output empty plot when a school is selected
      output$schoolplot <- renderPlot({
        emptyplot <<- ggplot(data = schooldata, aes(x = year_pub)) + labs(x = "Year", y = "Cost")
        print(emptyplot)
      })
      
    }, error = function(e) { # copied from https://stackoverflow.com/questions/19918985/r-plot-only-text#19966875
      output$schoolplot <- renderPlotly({
      # this outputs the paste() text to an empty plot
      par(mar = c(0,0,0,0))
      plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
      text(x = 0.34, y = 0.9, paste("Search the name of a U.S. college to show its plot"), 
           cex = 1.3, col = "black", font=1, adj=0.5)})
    })
      
    # renders the checkbox group depending on what data we have on the selected school
    output$choices <- renderUI({
      checkboxGroupInput("choices", NULL, choices = choices, inline = TRUE)
    })
    
    # when checkboxes are changed, plot changes
    observeEvent(input$choices, {
      # format data to be used with ggplot
      plotdataraw <- schooldata[,c("year_pub", input$choices)]
      plotdata <- melt(plotdataraw, id.var = "year_pub")
      
      # create vector for legend labels
      legend <- input$choices
      legend <- ifelse(legend == "tuition_and_fees_in_state", "In State", legend)
      legend <- ifelse(legend == "tuition_and_fees_out_of_state", "Out of State", legend)
      legend <- ifelse(legend == "tuition_fees_room_and_board_in_state", "In State + Room and Board", legend)
      legend <- ifelse(legend == "tuition_fees_room_and_board_out_of_state", "Out of State + Room and Board", legend)
      legend <- ifelse(legend == "room_and_board", "Room & Board", legend)
      
      # output plot depending on checkboxes
      # FIX: SOOO THANKS TO PLOTLY I CAN TOGGLE TRACES IN THE ACTUAL PLOT AND WON'T
      # NEED CHECKBOXES AT THE BOTTOM...INTERESTING, MAY NEED TO LEARN PLOTLY
      output$schoolplot <- renderPlot({
        if (!is.null(input$choices)) {
          plot <- ggplot(data = plotdata, aes(x=year_pub, y=value, col=variable)) +
          geom_line() + labs(x = "Year", y = "Cost") + scale_color_hue(label = legend) + theme(legend.title = element_blank())
          print(plot)
        } else {
          # if no checkboxes selected, output empty plot
          print(emptyplot)
        }
      })
    })
  })
}