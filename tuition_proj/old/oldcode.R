# old ugly code for future reference


# render plot of selected school
# output$schoolplot <- renderPlot({
#   
#   # data-reading code throws error if no school is selected--
#   # tryCatch outputs a nice message to the user instead of full error
#   tryCatch({
# 
#     # construct path to data source for selected school
#     schooldatapath <- paste0("./data/",input$schoolname, ".json")
#     
#     # use jsonlite to read json data in path
#     schooldata <- fromJSON(schooldatapath)
#     
#     # plot year vs out-of-state tuition (will include other things later)
#     plot(schooldata$year_pub, schooldata$tuition_and_fees_out_of_state, type = "l",
#          main = input$schoolname, xlab = "Year", ylab = "Out of State Tuition")
#     points(schooldata$year_pub, schooldata$tuition_and_fees_out_of_state, pch = 19)
#     
#    },  # if above code throws error, run below code: 
#    error=function(e) { # copied from https://stackoverflow.com/questions/19918985/r-plot-only-text#19966875
#     
#     # this outputs the paste() text to an empty plot
#     par(mar = c(0,0,0,0))
#     plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
#     text(x = 0.34, y = 0.9, paste("Search the name of a U.S. college to show its plot"), 
#          cex = 1.3, col = "black", font=1, adj=0.5)}
#   )
# })

# fluidRow(
#   column(1, disabled(tags$input(type="checkbox", id="instate"))),
#   column(6, tags$label(`for`="instate", "In State"))
# ),
# fluidRow(
#   column(1, disabled(tags$input(type="checkbox", id="outofstate"))),
#   column(6, tags$label(`for`="outofstate", "Out of State"))
# ),
# fluidRow(
#   column(1, disabled(tags$input(type="checkbox", id="instaterb"))),
#   column(6, tags$label(`for`="instaterb", "In State + Room & Board"))
# ),
# fluidRow(
#   column(1, disabled(tags$input(type="checkbox", id="outofstaterb"))),
#   column(6, tags$label(`for`="outofstaterb", "Out of State + Room & Board"))
# ),
# fluidRow(
#   column(1, disabled(tags$input(type="checkbox",id="rb"))),
#   column(6, tags$label(`for`="rb", "Room & Board"))
# )
# fluidRow(
#   column(1, disabled(tags$input(type="checkbox", id="instate"))),
#   column(2, tags$label(`for`="instate", "In State")),
#   column(1, disabled(tags$input(type="checkbox", id="outofstate"))),
#   column(1, disabled(tags$input(type="checkbox", id="instaterb"))),
#   column(1, disabled(tags$input(type="checkbox", id="outofstaterb"))),
#   column(1, disabled(tags$input(type="checkbox",id="rb")))
# )