# Load packages ----
library(shiny)
library(maps)
library(mapproj)
library(dplyr)
# Load data ----
source("datasource.R")
# User interface ----
ui <- fluidPage(
  titlePanel("Covid-19 study"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Create heat maps with 
        covid-19 research data from clinic.gov"),
      
      selectInput("var", 
                  label = "Choose a year to display",
                  choices = c("2019", "2020", "2021"),
                  selected = "2020"),
      
     
      
      radioButtons("dist", "Data Type:",
                   c("Total study enrollment",
                     "Total number of trials")),
      
      
      
      ),
    
    mainPanel(plotOutput("map"))
  )
)

# Server logic ----
server <- function(input, output) {
  output$map <- renderPlot({
    data <- switch(input$dist,
                   "Total study enrollment" = cleanDF,
                   "Total number of trials" = cleanDFTrial)
    
    data <- switch(input$var,
                   "2019" = data %>% filter(`Start Date` == "2019"),
                   "2020" = data %>% filter(`Start Date` == "2020"),
                   "2021" = data %>% filter(`Start Date` == "2021"))
    
    
    mapDF = joinCountryData2Map(data, joinCode = "ISO3", nameJoinColumn = "Locations")
    par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
    mapParams = mapCountryData(mapDF, 
                               nameColumnToPlot = "y", 
                               catMethod = "logFixedWidth", 
                               missingCountryCol = gray(0.8),
                               mapTitle = ""
                               )
  })
}

# Run app ----
shinyApp(ui, server)