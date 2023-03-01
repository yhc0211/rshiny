# Load packages ----
library(shiny)
library(shinythemes)
library(tidyverse)
library(dplyr)
library(readr)

# Load data
icu_cohort <- readRDS("icu_cohort.rds")

#select demographics, lab measurements, vitals
icu_cohort <- icu_cohort %>% 
  select(c(5,6,9,10,11,17:23,27:41))
icu_cohort$thirty_day_mort <- as.character(icu_cohort$thirty_day_mort)


# Define UI
ui <- fluidPage(
  theme = shinytheme("journal"),
  
  titlePanel("ICU cohort Summary Statistics"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Select a variable you want"),
      varSelectInput("variable",
                     "variable:",
                     icu_cohort,
                     selected = "gender"),
      hr(),
      helpText("Data : MIMIC_IV")
    ),
    # Show a bar plot or box plot and summary stat
    mainPanel(
      
      verbatimTextOutput("Summary"),
      plotOutput("Plot")
      
      
    )
  )
)

# Define server function
server <- function(input, output){
  
  var <- reactive({
    var_select <- icu_cohort %>%
      select(input$variable)
  })
  
  output$Summary <- renderPrint({
    if (summary(var())[2] == "Class :character  "){
      #if it's character then we use table to summarize
      table(var())
    }else{
      #if it's continuous data then we use summary()
      summary(var())
    }
  })
  
  output$Plot <- renderPlot({
    if (summary(var())[2] == "Class :character  "){
      ggplot(data = icu_cohort) + 
        geom_bar(mapping = aes_string(x = input$variable, 
                                      fill = input$variable))
      
    }else {
      ## revise
      ggplot(data = icu_cohort) + 
        geom_boxplot(mapping = aes_string(x = input$variable),
                     outlier.shape = NA) +
        coord_cartesian(xlim = c(boxplot.stats(var()[,1])[[1]][1],
                                 boxplot.stats(var()[,1])[[1]][5]))
    }
    
  })
  
  

  
  
}

# Create Shiny object
shinyApp(ui = ui, server = server)
