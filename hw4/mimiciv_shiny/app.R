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

#data <- mimic_icu_cohort %>%
 # select(Potassium, Sodium) %>%
  #pivot_longer(cols = c("Potassium", "Sodium"), 
   #            names_to = "variable", values_to = "value")

#data %>%
 # ggplot(aes(x = value, y = variable)) +
 #  geom_boxplot(notch=TRUE) +
 # theme_minimal()

ui <- fluidPage(
  tabsetPanel(
    tabPanel("Patient characteristics",
             sidebarLayout(
               sidebarPanel(
                 selectInput("variable", "Variable of interest",
                            choices = c("First care unit" = "first_careunit", 
                                        "Last care unit" = "last_careunit",
                                        "Admission type"  =  "admission_type",
                                    "Admission location" = "admission_location",
                                    "Discharge location" = "discharge_location", 
                                        "insurance", "language",
                                        "marital_status", "race", 
                                        "hospital_expire_flag", 
                                        "gender",
                                        "Lab Events" = "labevents",
                                        "Chart Events" = "chartevents")),
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
    if(input$variable == "labevents"){
      data <- mimic_icu_cohort %>%
        select(Potassium, Sodium, Glucose, Creatinine, Chloride, Bicarbonate) %>%
        pivot_longer(cols = c("Potassium", "Sodium","Glucose","Creatinine",
                              "Chloride","Bicarbonate"), 
                     names_to = "variable", values_to = "value")
      if(input$remove == FALSE){
        output$summaryPlot <- renderPlot({
          data %>%
            ggplot(aes(x = value,y = variable)) +
            geom_boxplot(notch=TRUE) +
            xlim(0,250) +
            theme_minimal()
        })}else{
          output$summaryPlot <- renderPlot({
            data %>%
              ggplot(aes(x = value,y = variable)) +
              geom_boxplot(notch=TRUE,outlier.shape = NA) +
              xlim(0,250) +
              theme_minimal()
          })
        }
    } else if(input$variable == "chartevents"){
      data <- mimic_icu_cohort %>%
        select(`Temperature Fahrenheit`,`Non Invasive Blood Pressure diastolic`,
               `Respiratory Rate`, `Non Invasive Blood Pressure systolic`
               , `Heart Rate`) %>%
        pivot_longer(cols = c(`Temperature Fahrenheit`,`Non Invasive Blood Pressure diastolic`,
                              `Respiratory Rate`, `Non Invasive Blood Pressure systolic`
                              , `Heart Rate`), 
                     names_to = "variable", values_to = "value")
      if(input$remove == FALSE){
        output$summaryPlot <- renderPlot({
          data %>%
            ggplot(aes(x = value,y = variable)) +
            geom_boxplot(notch=TRUE) +
            xlim(0,250) +
            theme_minimal()
        })}else{
          output$summaryPlot <- renderPlot({
            data %>%
              ggplot(aes(x = value,y = variable)) +
              geom_boxplot(notch=TRUE,outlier.shape = NA) +
              xlim(0,250) +
              theme_minimal()
          })
        }
    }else {
      data <- mimic_icu_cohort
      variable <- input$variable
      output$summaryPlot <- renderPlot({
        data %>%
          ggplot(aes_string(y = variable)) +
          geom_bar() +
          theme_minimal()
      })
    }
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
