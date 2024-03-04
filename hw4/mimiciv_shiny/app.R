library(shiny)
library(dplyr)
library(ggplot2)
library(gtsummary)
library(readr)
library(arrow)
library(memuse)
library(pryr)
library(R.utils)
library(tidyverse)
library(styler)
#getwd()
mimic_icu_cohort <- read_rds("mimic_icu_cohort.rds")

ui <- fluidPage(
  tabsetPanel(
    tabPanel("Patient characteristics",
             sidebarLayout(
               sidebarPanel(
                 selectInput("variable", "Variable of interest",
                            choices = c("First care unit" = "first_careunit", 
                                        "Last care unit" = "last_careunit",
                                        "admission_type",
                                        "admission_location",
                                        "discharge_location", 
                                        "insurance", "language",
                                        "marital_status", "race", 
                                        "hospital_expire_flag", 
                                        "gender",
                                        "Lab Events" = c("Sodium", "Potassium")) ),
                 actionButton("update", "Update"),
                 checkboxInput("remove", 
                            "Remove outliers in IQR method for measurements?")
               ),
               mainPanel(
                 plotOutput("summaryPlot"),
                 tableOutput("summaryTable")
               )
             )),
    tabPanel("Patient's ADT and ICU stay information",
             sidebarLayout(
               sidebarPanel(
                 helpText("Select a patient."),
                 textInput("patientID", "Patient ID"),
                 actionButton("lookup", "Lookup")
               ),
               mainPanel(
                 tableOutput("patientDetails")
               )
             ))
  )
)

server <- function(input, output) {
  observeEvent(input$update, {
    variable <- input$variable
    if(input$remove == FALSE) {
    output$summaryPlot <- renderPlot({
      mimic_icu_cohort %>%
        ggplot(aes_string(y = variable)) +
        geom_bar() +
        theme_minimal()
    })} else {
      output$summaryPlot <- renderPlot({
        mimic_icu_cohort %>%
          ggplot(aes_string(x = variable)) +
          geom_boxplot(notch=TRUE) +
          theme_minimal()
      })
    }
    output$summaryTable <- renderTable({
      mimic_icu_cohort %>%
        group_by(!!sym(variable)) %>%
        summarise(Count = n()) %>%
        arrange(desc(Count))
     })
  })
  
  observeEvent(input$lookup, {
    patientID <- input$patientID
    output$patientDetails <- renderPlot({
      mimic_icu_cohort %>%
        filter(subject_id == patientID) %>%
        ggplot()
    })
  })
}

shinyApp(ui, server)
