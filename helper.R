url <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"
url <- url(url,"rb")
covidData <- read_csv(url)
close(url)

for (state in unique(covidData$state)){
  data <- covidData[covidData$state == state,]
  data$cases_pre <- shift(data$cases, n = 1, fill = NA, type = "lag")
  data$deaths_pre <- shift(data$deaths, n = 1, fill = NA, type = "lag")
  data$cases_daily <- data$cases - data$cases_pre
  data$deaths_daily <- data$deaths - data$deaths_pre
  if(state == unique(covidData$state)[1]) {
    final <- data
  } else {
    final <- bind_rows(final, data)
  }
}
covidData <- final
#Add US total
total <- covidData %>%
  group_by(date) %>%
  summarize(cases = sum(cases, na.rm = TRUE), 
            deaths = sum(deaths, na.rm = TRUE), 
            cases_daily = sum(cases_daily, na.rm = TRUE), 
            deaths_daily = sum(deaths_daily, na.rm = TRUE))

total$state <- "US Total"
covidData <- covidData %>%
  bind_rows(total)

covidData <- covidData %>% mutate(state = as.factor(state))
covidData$state <- relevel(covidData$state,"US Total")
#Make the label more friendly
covidData <- dplyr::rename(covidData,`cases accumulative` = cases, `deaths accumulative` = deaths,
                           `cases daily` = cases_daily, `deaths daily` = deaths_daily)