library(shiny)
library(jsonlite)
library(shinyjs)
library(ggplot2)
library(reshape2)
library(plotly)

useShinyjs()

server <- function(input, output, session) {
  
  # maintain school search box
  output$out4 <- renderPrint(input$schoolname)
  
  # FIX: Y U NO WORK????
  # output$initial <- renderUI({renderText({"Select a school to view its plot"})})
  
  # react when school name is changed
  observeEvent(input$schoolname, {
    # remove 'select a school' instruction
    # output$initial <- NULL
    # remove any table from a previous selection
    output$table <- NULL
    # tryCatch prevents initial error when program attempts to read in data
    #  before a school has been selected
    tryCatch({
      
      # construct path to data source for selected school
      schooldatapath <- paste0("./data/",input$schoolname, ".json")
    
      # use jsonlite to read json data in path
      schooldata <<- fromJSON(schooldatapath)
      
      # convert to numeric since all are str in json
      schooldata$tuition_and_fees_in_state <- as.numeric(schooldata$tuition_and_fees_in_state)
      schooldata$tuition_and_fees_out_of_state <- as.numeric(schooldata$tuition_and_fees_out_of_state)
      schooldata$tuition_fees_room_and_board_in_state <- as.numeric(schooldata$tuition_fees_room_and_board_in_state)
      schooldata$tuition_fees_room_and_board_out_of_state <- as.numeric(schooldata$tuition_fees_room_and_board_out_of_state)
      schooldata$room_and_board <- as.numeric(schooldata$room_and_board)
      schooldata$year_pub <- as.integer(schooldata$year_pub)
      
      # if all categories are the same, consolidate into one category
      if (identical(schooldata$tuition_and_fees_in_state, schooldata$tuition_and_fees_out_of_state) && 
          identical(schooldata$tuition_fees_room_and_board_in_state, schooldata$tuition_fees_room_and_board_out_of_state)) {
        # if tuition & room and board categories are the same, consolidate into two
        if (identical(schooldata$tuition_and_fees_in_state,schooldata$tuition_fees_room_and_board_in_state) && 
            identical(schooldata$tuition_and_fees_out_of_state, schooldata$tuition_fees_room_and_board_out_of_state)) {
          # create new classification
          schooldata$tuition <- schooldata$tuition_and_fees_in_state
          # columns to keep in plot
          selectvector <- c("year_pub", "tuition")
          schooldata <<- schooldata[, names(schooldata) %in% selectvector]
          namelist <<- c("year_pub" = "Year", "tuition" = "Tuition")
        } else {
          # create new classifications
          schooldata$tuition <- schooldata$tuition_and_fees_in_state
          schooldata$tuition_room_and_board <- schooldata$tuition_fees_room_and_board_in_state
          # columns to keep in plot
          selectvector <- c("year_pub", "tuition", "tuition_room_and_board", "room_and_board")
          schooldata <<- schooldata[, names(schooldata) %in% selectvector]
          namelist <<- c("year_pub" = "Year", "room_and_board" = "Room and Board", 
                        "tuition" = "Tuition", "tuition_room_and_board" = "Tuition + Room and Board")
        }
      } else {
        schooldata <<- schooldata[,c("year_pub", "tuition_and_fees_in_state", "tuition_and_fees_out_of_state",
                                    "room_and_board", "tuition_fees_room_and_board_in_state", 
                                    "tuition_fees_room_and_board_out_of_state")]
        
        namelist <<- c("year_pub" = "Year", "tuition_and_fees_in_state" = "In State", 
                      "tuition_and_fees_out_of_state" = "Out of State", "room_and_board" = "Room and Board", 
                      "tuition_fees_room_and_board_in_state" = "In State + Room and Board",
                      "tuition_fees_room_and_board_out_of_state" = "Out of State + Room and Board")
      } 
      
      # output school data in plot when school is selected
      output$schoolplot <- renderPlotly({
        if (!is.null(schooldata$tuition)) {
          if (!is.null(schooldata$tuition_room_and_board)) {
            p <- plot_ly(schooldata, x = ~year_pub) %>%
              add_trace(y = ~tuition, name = "Tuition", type = "scatter", mode = "lines+markers") %>%
              add_trace(y = ~tuition_room_and_board, name = "Tuition + Room & Board", type = "scatter", mode = "lines+markers") %>%
              add_trace(y = ~room_and_board, name = "Room and Board", type = "scatter", mode = "lines+markers") %>%
              layout(legend = list(x = 0.05, y = 0.95)) %>%
              layout(xaxis = list(title = "Years"), yaxis = list(title = "Cost"))
            print(p)
          } else {
            p <- plot_ly(schooldata, x = ~year_pub) %>%
              add_trace(y = ~tuition, name = "Tuition", type = "scatter", mode = "lines+markers") %>%
              layout(legend = list(x = 0.05, y = 0.95)) %>%
              layout(xaxis = list(title = "Years"), yaxis = list(title = "Cost"))
            print(p)
          } 
        } else {
          
          p <- plot_ly(schooldata, x = ~year_pub) %>%
            add_trace(y = ~tuition_and_fees_in_state, name = "In State", type = "scatter", mode = "lines+markers") %>%
            add_trace(y = ~tuition_and_fees_out_of_state, name = "Out of State", type = "scatter", mode = "lines+markers") %>%
            add_trace(y = ~tuition_fees_room_and_board_in_state, name = "In State + Room and Board", type = "scatter", mode = "lines+markers") %>%
            add_trace(y = ~tuition_fees_room_and_board_out_of_state, name = "Out of State + Room and Board", type = "scatter", mode = "lines+markers") %>%
            add_trace(y = ~room_and_board, name = "Room and Board", type = "scatter", mode = "lines+markers") %>%
            layout(legend = list(x = 0.05, y = 0.95)) %>%
            layout(xaxis = list(title = "Years"), yaxis = list(title = "Cost"))
          print(p)
        }
        
      })
      
    }, error = function(e) {
      print("something went wrong")
    })
    
    if (input$schoolname != "") {
      output$toggletable <- renderUI({actionButton("toggletable", "Show Table")})
    } 
    
  })
  
  observeEvent(input$toggletable, {
    output$toggletable <- renderUI({actionButton("hidetable", "Hide Table")})
    colnames(schooldata) <- namelist
    print(namelist)
    print(schooldata)
    output$table <- DT::renderDataTable(DT::datatable(schooldata, options = list(lengthMenu = c(5, 10, 25), 
                                                                                pageLength = 25,
                                                                                autoWidth = TRUE,
                                                                                columnDefs = list(list(width = '600px', targets = c(1:length(namelist))))
                                                                                )))
  })
  
  observeEvent(input$hidetable, {
    output$toggletable <- renderUI({actionButton("toggletable", "Show Table")})
    output$table <- NULL
  })
  
}