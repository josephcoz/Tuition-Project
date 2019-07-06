library(jsonlite)

school <- scan(file = "/cloud/project/tuition_proj/schools.csv", what = character(), sep=",")

sector <- NULL

state <- NULL

for (i in schools) {
  schoolpath <- paste0("/cloud/project/tuition_proj/data/", i, ".json")
  data <- fromJSON(schoolpath)
  data$sector <- as.numeric(data$sector)
  
  sector <- c(sector, data$sector[1])
  state <- c(state, data$state[1])
}

schoolsdata <- cbind(school, sector, state)
schoolsdata <- as.data.frame(schoolsdata)

