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
adt <- tbl(con_bq, "transfers") 
lab <- tbl(con_bq, "labevents") 
proc <- tbl(con_bq, "procedures_icd")
patients <- tbl(con_bq, "patients")
admission <- tbl(con_bq, "admissions") 
d_proc <- tbl(con_bq, "d_icd_procedures") 
dia <- tbl(con_bq, "diagnoses_icd") 
d_dia <- tbl(con_bq, "d_icd_diagnoses")

####
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
                 checkboxInput("remove", 
                            "Remove outliers in IQR method for measurements?")
               ),
               mainPanel(
                 plotOutput("summaryPlot")
               )
             )),
    tabPanel("Patient's ADT and ICU stay information",
             sidebarLayout(
               sidebarPanel(
                 helpText("Select a patient."),
                 textInput("patientID", "Patient ID"),
               ),
               mainPanel(
                 plotOutput("patientDetails")
               )
             ))
  )
)
server <- function(input, output) {
  dataReactive <- reactive({
    if(input$variable == "labevents"){
      data <- mimic_icu_cohort %>%
        select(Potassium, Sodium, Glucose, Creatinine, Chloride, Bicarbonate) %>%
        pivot_longer(cols = c("Potassium", "Sodium", "Glucose", "Creatinine"
                              , "Chloride", "Bicarbonate"), 
                     names_to = "variable", values_to = "value")
    } else if(input$variable == "chartevents"){
      data <- mimic_icu_cohort %>%
        select(`Temperature Fahrenheit`, `Non Invasive Blood Pressure diastolic`
               , `Respiratory Rate`, 
               `Non Invasive Blood Pressure systolic`, `Heart Rate`) %>%
        pivot_longer(cols = c(`Temperature Fahrenheit`, 
                              `Non Invasive Blood Pressure diastolic`, 
                              `Respiratory Rate`, `Non Invasive Blood Pressure systolic`
                              , `Heart Rate`), 
                     names_to = "variable", values_to = "value")
    } else {
      data <- mimic_icu_cohort
    }
    return(data)
  })
  
  # Plot for the first tab
  output$summaryPlot <- renderPlot({
    data <- dataReactive() # Use the reactive data
    if(input$variable %in% c("labevents", "chartevents")){
      if(input$remove == TRUE){
        plot <- ggplot(data, aes(x = value, y = variable)) +
          geom_boxplot(notch=TRUE, outlier.shape = NA) +
          xlim(0,250) +
          theme_minimal()
      } else {
        plot <- ggplot(data, aes(x = value, y = variable)) +
          geom_boxplot(notch=TRUE) +
          xlim(0,250) +
          theme_minimal()
      }
    } else {
      variable <- input$variable
      plot <- ggplot(data, aes_string(y = variable)) +
        geom_bar() +
        theme_minimal()
    }
    print(plot)
  })  
#Second Tab
    dataReactive2 <- reactive({
      sid <- as.numeric(input$patientID)
      sid_info <- patients %>%
        filter(subject_id == sid) %>%
        collect()
      sid_adt <- adt %>%
        filter(subject_id == sid) %>%
        collect()
      sid_adm <- admission %>%
        filter(subject_id == sid) %>%
        collect()
      sid_lab <- lab %>%
        filter(subject_id == sid) %>%
        collect()
      sid_proc <- proc %>%
        filter(subject_id == sid) %>%
        left_join(d_proc, by = c("icd_code", "icd_version")) %>%
        collect()
      sid_proc$chartdate <- as.POSIXct(sid_proc$chartdate)
      sid_diag <- dia %>%
        filter(subject_id == sid) %>%
        left_join(d_dia, by = c("icd_code", "icd_version")) %>%
        collect()
      list(sid = sid, sid_info = sid_info, sid_adt = sid_adt, sid_adm = sid_adm, 
           sid_lab = sid_lab, sid_proc = sid_proc, sid_diag = sid_diag)
   })
    # Plot for the second tab
    output$patientDetails <- renderPlot({
      data2 <- dataReactive2()
      ggplot() +
        geom_segment(
          data = data2$sid_adt %>% filter(eventtype != "discharge"),
          mapping = aes(
            x = intime,
            xend = outtime,
            y = "ADT",
            yend = "ADT",
            color = careunit,
            linewidth = str_detect(careunit, "(ICU|CCU)")
          ),
       ) +
      geom_point(
        data = data2$sid_lab %>% distinct(charttime, .keep_all = TRUE),
        mapping = aes(x = charttime, y = "Lab"),
        shape = '+',
        size = 5
      ) +
      geom_jitter(
        data = data2$sid_proc,
        mapping = aes(
          x = chartdate,
          y = "Procedure",
          shape = str_sub(long_title, 1, 25)
        ),
        size = 3,
        height = 0
      ) + 
      labs(
        title = str_c(
          "Patient ", data2$sid, ", ",
          data2$sid_info$gender, ", ",
          data2$sid_info$anchor_age + year(data2$sid_adm$admittime[1]) - data2$sid_info$anchor_year,
          " years old, ",
          str_to_lower(data2$sid_adm$race[1])
        ),
        subtitle = str_c(str_to_lower(data2$sid_diag$long_title[1:3]), collapse = "\n"),
                         x = "Calendar date",
                         y = "",
                         color = "Care unit",
                         shape = "Procedure"
                         ) +
        guides(linewidth = "none") +
        scale_y_discrete(limits = rev) +
        theme_light() +
        theme(legend.position = "bottom", legend.box = "vertical")
    })
}

shinyApp(ui, server)
