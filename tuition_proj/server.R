library(shiny)
library(jsonlite)
library(shinyjs)
library(reshape2)
library(plotly)
library(forecast)
library(tseries)
library(shinyWidgets)

useShinyjs()

# forecast function
createforecastdf <- function(var, latestyear = 2018, numyears = 5) {
  model <- auto.arima(var)
  # summary(model)
  
  pred <- forecast(model, numyears)
  predvals <- as.numeric(pred$mean)
  predll80 <- as.numeric(pred$lower[,1])
  predul80 <- as.numeric(pred$upper[,1])
  predll95 <- as.numeric(pred$lower[,2])
  predul95 <- as.numeric(pred$upper[,2])
  
  predyears <- (latestyear + 1):(latestyear + numyears)
  preddf <- as.data.frame(cbind(predyears, predvals, predll80, predul80, predll95, predul95))
  return(preddf)
}

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
      
    }, error = function(e) {
      print("something went wrong")
    })
    
    # if the schooldata object exists, move on
    if (exists("schooldata") & (input$schoolname != "")) {
      
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
    }
      # output school data in plot when school is selected
      if (input$schoolname != "") {
      
        if (!is.null(schooldata$tuition)) {
          if (!is.null(schooldata$tuition_room_and_board)) {
            p <- plot_ly(schooldata, x = ~year_pub) %>%
              add_trace(y = ~tuition, name = "Tuition", type = "scatter", mode = "lines+markers") %>%
              add_trace(y = ~tuition_room_and_board, name = "Tuition + Room & Board", type = "scatter", mode = "lines+markers") %>%
              add_trace(y = ~room_and_board, name = "Room and Board", type = "scatter", mode = "lines+markers") %>%
              layout(xaxis = list(title = "Years"), yaxis = list(title = "Cost"), legend = list(orientation = 'h',
                                                                                                xanchor = "left",
                                                                                                y = -0.25))
            # this variable will be the same across all forecasts
            lastyear <- max(schooldata$year_pub)
            
            # use forecast function to create forecast dfs
            tuitdf <- createforecastdf(schooldata$tuition, latestyear = lastyear)
            # FIX: ROOM AND BOARD HAS NAs, WON'T WORK RIGHT NOW
            # rbdf <- createforecastdf(schooldata$room_and_board, lastyear)
            
            # construct forecast plot
            q <- p %>%
              add_ribbons(x = tuitdf$predyears, ymin = tuitdf$predll80, ymax = tuitdf$predul80, 
                          color = I("gray80"), legendgroup = "isf", showlegend = FALSE) %>%
              add_ribbons(x = tuitdf$predyears, ymin = tuitdf$predll95, ymax = tuitdf$predul95, 
                          color = I("gray95"), legendgroup = "isf", showlegend = FALSE) %>%
              add_trace(x = tuitdf$predyears, y = tuitdf$predvals, name = "Tuition Forecast", type = "scatter", 
                        mode = "lines+markers", line = list(dash = "dash"), legendgroup = "isf") #%>%
              # add_ribbons(rbdf, x = ~predyears, ymin = ~predll80, ymax = ~predul80, color = I("gray80"), 
              #             legendgroup = "isf", showlegend = FALSE) %>%
              # add_ribbons(rbdf, x = ~predyears, ymin = ~predll95, ymax = ~predul95, color = I("gray95"),
              #             legendgroup = "isf", showlegend = FALSE) %>%
              # add_trace(rbdf, x = ~predyears, y = ~predvals, name = "Room & Board Forecast", type = "scatter", 
              #           mode = "lines+markers", line = list(dash = "dash"), legendgroup = "isf")
            
        } else {
            p <- plot_ly(schooldata, x = ~year_pub) %>%
              add_trace(y = ~tuition, name = "Tuition", type = "scatter", mode = "lines+markers") %>%
              layout(xaxis = list(title = "Years"), yaxis = list(title = "Cost"), legend = list(orientation = 'h',
                                                                                                xanchor = "left",
                                                                                                y = -0.25))
            # this variable will be the same across all forecasts
            lastyear <- max(schooldata$year_pub)
            
            # use forecast function to create forecast df
            tuitdf <- createforecastdf(schooldata$tuition, latestyear = lastyear)
            print(tuitdf)
            
            # construct forecast plot
            q <- p %>%
              add_ribbons(x = tuitdf$predyears, ymin = tuitdf$predll80, ymax = tuitdf$predul80, 
                          color = I("gray80"), legendgroup = "isf", showlegend = FALSE) %>%
              add_ribbons(x = tuitdf$predyears, ymin = tuitdf$predll95, ymax = tuitdf$predul95, 
                          color = I("gray95"), legendgroup = "isf", showlegend = FALSE) %>%
              add_trace(x = tuitdf$predyears, y = tuitdf$predvals, name = "Tuition Forecast", type = "scatter", 
                        mode = "lines+markers", line = list(dash = "dash"), legendgroup = "isf")
            
          } 
        } else {
          p <- plot_ly(schooldata, x = ~year_pub) %>%
            add_trace(y = ~tuition_and_fees_in_state, name = "In State", type = "scatter", mode = "lines+markers") %>%
            add_trace(y = ~tuition_and_fees_out_of_state, name = "Out of State", type = "scatter", mode = "lines+markers") %>%
            add_trace(y = ~tuition_fees_room_and_board_in_state, name = "In State + Room and Board", type = "scatter", mode = "lines+markers") %>%
            add_trace(y = ~tuition_fees_room_and_board_out_of_state, name = "Out of State + Room and Board", type = "scatter", mode = "lines+markers") %>%
            add_trace(y = ~room_and_board, name = "Room and Board", type = "scatter", mode = "lines+markers") %>%
            layout(xaxis = list(title = "Years"), yaxis = list(title = "Cost"), legend = list(orientation = 'h',
                                                                                              xanchor = "left",
                                                                                              y = -0.25))
          # this variable will be the same across all forecasts
          lastyear <- max(schooldata$year_pub)
          
          # use forecast function to create forecast dfs
          tuitisdf <- createforecastdf(schooldata$tuition_and_fees_in_state, latestyear = lastyear)
          tuitoosdf <- createforecastdf(schooldata$tuition_and_fees_out_of_state, latestyear = lastyear)
          # FIX: ROOM AND BOARD HAS NAs, WON'T WORK RIGHT NOW
          # rbdf <- createforecastdf(schooldata$room_and_board, lastyear)
          
          # construct forecast plot
          q <- p %>%
            add_ribbons(x = tuitisdf$predyears, ymin = tuitisdf$predll80, ymax = tuitisdf$predul80, 
                        color = I("gray80"), legendgroup = "isf", showlegend = FALSE) %>%
            add_ribbons(x = tuitisdf$predyears, ymin = tuitisdf$predll95, ymax = tuitisdf$predul95, 
                        color = I("gray95"), legendgroup = "isf", showlegend = FALSE) %>%
            add_trace(x = tuitisdf$predyears, y = tuitisdf$predvals, name = "Tuition Forecast", type = "scatter", 
                      mode = "lines+markers", line = list(dash = "dash"), legendgroup = "isf") %>%
            add_ribbons(x = tuitoosdf$predyears, ymin = tuitoosdf$predll80, ymax = tuitoosdf$predul80, 
                        color = I("gray80"), legendgroup = "isf", showlegend = FALSE) %>%
            add_ribbons(x = tuitoosdf$predyears, ymin = tuitoosdf$predll95, ymax = tuitoosdf$predul95, 
                        color = I("gray95"), legendgroup = "isf", showlegend = FALSE) %>%
            add_trace(x = tuitoosdf$predyears, y = tuitoosdf$predvals, name = "Tuition Forecast", type = "scatter", 
                      mode = "lines+markers", line = list(dash = "dash"), legendgroup = "isf") #%>%
            # add_ribbons(rbdf, x = ~predyears, ymin = ~predll80, ymax = ~predul80, color = I("gray80"), 
            #             legendgroup = "isf", showlegend = FALSE) %>%
            # add_ribbons(rbdf, x = ~predyears, ymin = ~predll95, ymax = ~predul95, color = I("gray95"),
            #             legendgroup = "isf", showlegend = FALSE) %>%
            # add_trace(rbdf, x = ~predyears, y = ~predvals, name = "Room & Board Forecast", type = "scatter", 
            #           mode = "lines+markers", line = list(dash = "dash"), legendgroup = "isf")
        }
      
      output$toggleforecast <- renderUI({materialSwitch("schoolforecast", "Forecast", value = FALSE, status = "success")})
      output$toggletable <- renderUI({materialSwitch("schooltable", "Table", value = FALSE, status = "primary")})
      
      print(input$schoolforecast)
      
      observeEvent(input$schoolforecast, {
        if (!is.null(input$schoolforecast)) {
          if (input$schoolforecast) {
            output$schoolplot <- renderPlotly({q}) 
          } else {
            output$schoolplot <- renderPlotly({p})  
          }
        } else {
          output$schoolplot <- renderPlotly({p})
        }
      })
      # end if statement where schoolname != ""
      }
  # end reactive when school is selected
  })
  
  observeEvent(input$schooltable, {
    # output$toggletable <- renderUI({actionButton("hidetable", "Hide Table")})
    if (input$schooltable) {
      colnames(schooldata) <- namelist
      # print(namelist)
      # print(schooldata)
      output$table <- DT::renderDataTable(DT::datatable(schooldata, options = list(lengthMenu = c(5, 10, 25), 
                                                                                  pageLength = 25,
                                                                                  autoWidth = TRUE,
                                                                                  columnDefs = list(list(width = '600px', targets = c(1:length(namelist))))
                                                                                  )))
    } else {
      output$table <- NULL
    }
  })
  
  # observeEvent(input$hidetable, {
  #   output$toggletable <- renderUI({actionButton("toggletable", "Show Table")})
  #   output$table <- NULL
  # })

}
