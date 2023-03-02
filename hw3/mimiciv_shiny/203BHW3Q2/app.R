#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(tidyverse)
library(DBI)
library(bigrquery)

# Load the data
setwd("~/Desktop/203b-hw/hw3/mimiciv_shiny")
cohort <- readRDS("icu_cohort.rds")


# Define UI for application that draws a histogram
ui <- fluidPage(
  
# Application title
titlePanel("Mimic Cohort Data Summary"),
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput("demovar", 
                  label = "Demographics Variable",
                  
                  choices = c("insurance",
                              "language",
                              "ethnicity",
                              "marital_status",
                              "gender",
                              "thirty_day_mort",
                              "age"),
                  selected = "age"),
      
      sliderInput("bins",
                  "Number of bins:",
                  min = 1, max = 200, value = 200),
      
      selectInput("itemid",
                  label = "Lab Measurements and Vitals",
                  
                  choices = c("Bicarbonate", 
                              "Chloride",
                              "Creatinine", 
                              "Glucose",
                              "Potassium", 
                              "Sodium",
                              "Hematocrit",  
                              "White_Blood_Cells", 
                              "Respiratory_Rate",
                              "Heart_Rate", 
                              "Non_Invasive_Blood_Pressure_systolic",
                              "Non_Invasive_Blood_Pressure_mean", 
                              "Temperature_Fahrenheit"),
                  selected = "Bicarbonate"), ),
    
  mainPanel(
      plotOutput("demo_plot"),
      verbatimTextOutput("demo_summary"),
      plotOutput("lab_plot"),
      verbatimTextOutput("lab_summary"))))


# Define server logic required to draw a histogram
server <- function(input, output) {
  demo <- reactive({
    choosedemo <- switch(input$demovar,
                       "insurance" = cohort$insurance,
                       "language" = cohort$language,
                       "ethnicity" = cohort$ethnicity,
                       "marital_status" = cohort$marital_status,
                       "gender" = cohort$gender,
                       "thirty_day_mort" = cohort$thirty_day_mort,
                       "age" = cohort$age)})
  
#plot demo
output$demo_plot <- renderPlot({
    if (input$demovar == "age"){
      cohort %>%
        ggplot() +
        geom_boxplot(mapping = aes(y = demo())) +
        labs(x = input$choosedemo)
    }
    else{
      cohort %>%
        ggplot() +
        geom_bar(mapping = aes(x = demo())) +
        labs(x = input$choosedemo)
    }
  })
  
# numeric summary
output$demo_summary <- renderPrint({
    if (input$demovar == "age"){
      #if it's continuous data then do summary()
      summary(demo())
    }
    else{
      #if it's character summarize in table
      table(demo())
    }
  })
  
  
  
labvital <- reactive({
    chooseitem <- switch(input$itemid,
                        "Bicarbonate" = cohort$bicarbonate, 
                        "Chloride" = cohort$chloride,
                        "Creatinine" = cohort$creatinine, 
                        "Glucose" = cohort$glucose,
                        "Potassium" = cohort$potassium, 
                        "Sodium" = cohort$sodium,
                        "Hematocrit" = cohort$hematocrit,
                        "White_Blood_Cells" = cohort$n_wb_cells, 
                        "Respiratory_Rate" = cohort$Respiratory_Rate,
                        "Heart_Rate" = cohort$Heart_Rate, 
                        "Non_Invasive_Blood_Pressure_systolic" = 
                          cohort$Non_Invasive_Blood_Pressure_systolic,
                        "Non_Invasive_Blood_Pressure_mean" = 
                          cohort$Non_Invasive_Blood_Pressure_mean, 
                        "Temperature_Fahrenheit" = 
                          cohort$Temperature_Fahrenheit)})
  
# numeric summary
output$lab_summary <- renderPrint({
    summary(labvital())
  })
  
# plot labvital
output$lab_plot <- renderPlot({
    cohort %>%
      ggplot() +
      geom_histogram(mapping = aes(x = labvital()), bins = input$bins) +
      labs(x = input$chooseitem)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
