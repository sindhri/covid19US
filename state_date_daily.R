library(shiny)
library(datasets)
library(readr)
library(ggplot2)
library(data.table)
library(dplyr)
library(ggiraph)


# Data pre-processing ----
# Tweak the "am" variable to have nicer factor labels -- since this
# doesn't rely on any user inputs, we can do this once at startup
# and then use the value throughout the lifetime of the app

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


# Define UI for miles per gallon app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel(h1("Covid-19 data per state")),
  
  helpText("Data source:", (a("NYtimes Covid-19 data at Github",
                              href="https://github.com/nytimes/covid-19-data",
                              target="_blank"))),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(width = 3,

      # Input: Selector for variable to plot against mpg ----
      selectInput("state_name", "State:",
                  sort(unique(covidData$state)), selected = "Connecticut"),
      
      # Input: Checkbox for whether outliers should be included ----
    
      radioButtons("type_of_data", "Plot type:",
                  c("Accumulative" = "accumulative",
                    "Daily" = "daily")),

      checkboxInput("logscale", "Use Log Scale", FALSE)
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(width = 9,

      plotOutput("casePlot"),
      br(),
      plotOutput("deathPlot")

    )
  )
)

# Define server logic to plot various variables against mpg ----
server <- function(input, output) {


  # Compute the formula text ----
  # This is in a reactive expression since it is shared by the
  # output$caption and output$mpgPlot functions
  formulaText <- reactive({
    paste(input$type_of_data,"~ date")
  })
  
  # Return the formula text for printing as a caption ----
  #output$caption <- renderText({
  #  formulaText()
  #})
  
  # Generate a plot of the requested variable against mpg ----
  # and only exclude outliers if requested
  
  #question, how to make the title and ylab easier to understand without renaming the dataframe columns
  
  output$casePlot <- renderPlot({

      p <- ggplot(data = covidData[covidData$state == input$state_name,], na.rm = TRUE) + 
        aes_string(x = "date", y = paste("`cases ",input$type_of_data,"`", sep = ""), na.rm = TRUE) + 
        geom_line(color = "#75AADB", size = 1.5)+
        theme(panel.background = element_rect(fill = "white", colour = "grey50"), 
              text = element_text(size=20),
              plot.title = element_text(hjust = 0.5)) + 
        labs(title = paste(input$state_name, "cases", input$type_of_data)) 
    
      if (input$logscale) {
        p + scale_y_continuous(trans = 'log10')
      } 
      else {
        p
      }
  
      })
  
  output$deathPlot <- renderPlot({
    
    p <- ggplot(data = covidData[covidData$state == input$state_name,], na.rm = TRUE) + 
      aes_string(x = "date", y = paste("`deaths ",input$type_of_data,"`", sep = ""), na.rm = TRUE) + 
      geom_line(color = "#75AADB", size = 1.5)+
      theme(panel.background = element_rect(fill = "white", colour = "grey50"), 
            text = element_text(size=20),
            plot.title = element_text(hjust = 0.5)) + 
      labs(title = paste(input$state_name, "deaths",input$type_of_data))
    
    if (input$logscale) {
      p + scale_y_continuous(trans = 'log10')
    } 
    else {
      p
    }
    
  })
  

#  output$info <- renderText({
#    xy_str <- function(e) {
#      if(is.null(e)) return("NULL\n")
#      paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
#    }
#    xy_range_str <- function(e) {
#      if(is.null(e)) return("NULL\n")
#      paste0("xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1), 
#             " ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1))
#    }
    
#    paste0(
#      "click: ", xy_str(input$plot_click),
#      "dblclick: ", xy_str(input$plot_dblclick),
#      "hover: ", xy_str(input$plot_hover),
#      "brush: ", xy_range_str(input$plot_brush)
#    )
#  })
  
}

# Create Shiny app ----
shinyApp(ui, server)