# test prediction
library(jsonlite)
library(forecast)
library(tseries)
library(plotly)

schooldata <- fromJSON("/cloud/project/tuition_proj/data/Brigham Young University.json")
schooldata$year_pub <- as.integer(schooldata$year_pub)
ts <- as.numeric(schooldata$tuition_and_fees_in_state)

model <- auto.arima(ts)
summary(model)

predint <- 5
pred <- forecast(model, predint)
predvals <- as.numeric(pred$mean)
predll80 <- as.numeric(pred$lower[,1])
predul80 <- as.numeric(pred$upper[,1])
predll95 <- as.numeric(pred$lower[,2])
predul95 <- as.numeric(pred$upper[,2])

lastyear <- max(schooldata$year_pub)
predyears <- (lastyear + 1):(lastyear + predint)
preddf <- as.data.frame(cbind(predyears, predvals, predll80, predul80, predll95, predul95))

p <- plot_ly(schooldata, x = ~year_pub) %>% 
  add_trace(y = ~tuition_and_fees_in_state, name = "In State", type = "scatter", mode = "lines+markers") %>%
  add_ribbons(preddf, x = predyears, ymin = predll80, ymax = predul80, color = I("gray80"), 
              legendgroup = "isf", showlegend = FALSE) %>%
  add_ribbons(preddf, x = predyears, ymin = predll95, ymax = predul95, color = I("gray95"),
              legendgroup = "isf", showlegend = FALSE) %>%
  add_trace(preddf, x = predyears, y = predvals, name = "In State Forecast", type = "scatter", 
            mode = "lines+markers", line = list(dash = "dash"), legendgroup = "isf")

p
