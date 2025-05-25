library(shiny)
library(shinyWidgets)
library(ggplot2)
library(plotly)
library(DT)
library(writexl)
library(dplyr)
library(lubridate)
library(RColorBrewer)
library(shinyjs)

# Load the dataset
data <- read.csv("path/dataset_final.csv")
data[data == ""] <- NA

# Define min and max values for filters
min_age <- min(data$Age_at_visit, na.rm = TRUE)
max_age <- max(data$Age_at_visit, na.rm = TRUE)
min_mmse <- min(data$MMSE, na.rm = TRUE)
max_mmse <- max(data$MMSE, na.rm = TRUE)
min_cdr <- min(data$CDR, na.rm = TRUE)
max_cdr <- max(data$CDR, na.rm = TRUE)

# UI
ui <- fluidPage(
  useShinyjs(),  # Initialize shinyjs
  titlePanel("Interfície de consulta"),
  sidebarLayout(
    sidebarPanel(
      id = "mySidebar", class = "sidebar",
      div(style = "overflow-y: auto; overflow-x: hidden; max-height: calc(100vh - 150px);",
          h2("Filters"),
          # Cohort
          pickerInput("cohort", label = h3("Cohort"), 
                      choices = unique(data$Cohort), 
                      selected = unique(data$Cohort),
                      options = list(`actions-box` = TRUE, `live-search` = TRUE),
                      multiple = TRUE),
          # Age_at_visit
          sliderInput("age", label = h3("Edat"), 
                      min = min_age, max = max_age, 
                      value = c(min_age, max_age), width = "100%"),
          # Gender
          pickerInput("gender", label = h3("Gènere"), 
                      choices = unique(data$Gender), 
                      selected = unique(data$Gender),
                      options = list(`actions-box` = TRUE, `live-search` = TRUE), 
                      multiple = TRUE),
          # DX
          pickerInput("dx", label = h3("Diagnòstic"), 
                      choices = unique(data$DX), 
                      selected = unique(data$DX),
                      options = list(`actions-box` = TRUE, `live-search` = TRUE), 
                      multiple = TRUE),
          # neuro i bio
          fluidRow(
            column(6,
                   checkboxGroupInput(
                     inputId = "neuroimaging_filter_vars",
                     label = h3("Neuroimatge"),
                     choices = c("MRI", "PET_amyloid", "PET_FDG",
                                 "PET_tau"),
                     selected = NULL
                   )
            ),
            column(6,
                   checkboxGroupInput(
                     inputId = "biomarkers_filter_vars",
                     label = h3("Biomarcadors"),
                     choices = c("CSF_AB42", "CSF_pTau", "CSF_tTau", "CSF_NfL",
                                 "Plasma_NfL", "Plasma_pTau_Simoa", "Plasma_pTau_Roche"),
                     selected = NULL
                   )
            )
          ),
          
          # MMSE and CDR
          fluidRow(
            column(6, sliderInput("mmse", label = h3("MMSE"), 
                                  min = 0, max = 30, value = c(0, 30), step = 1)),
            column(6, sliderInput("cdr", label = h3("CDR"), 
                                  min = 0, max = 3, value = c(0, 3), step = 0.5)),
            column(3, actionButton("resetFilters", "Reset Filters", class = "btn_action"))
          )
      )
    ),
    mainPanel(
      class = "main-panel",
      tabsetPanel(
        tabPanel("Gràfics",
                 fluidRow(
                   column(4, plotlyOutput("Cohortplot", height = "310px")),
                   column(4, plotlyOutput("SexPlot", height = "310px")),
                   column(4, plotlyOutput("edadPlot", height = "310px"))
                 )
        )
      ),
      DTOutput("mytable"),
      downloadButton('downloadCSV', 'Download CSV', class = "btn_csv"),
      downloadButton('downloadXLSX', 'Download EXCEL', class = "btn_csv")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  neuroimaging_vars <- c("MRI", "PET_amyloid","PET_FDG", "PET_tau")
  
  biomarkers_vars <- c("CSF_AB42", "CSF_pTau", "CSF_tTau", "CSF_NfL",
                       "Plasma_NfL", "Plasma_pTau_Simoa", "Plasma_pTau_Roche")
  
  # Botó per reiniciar filtres
  observeEvent(input$resetFilters, {
    updatePickerInput(session, "cohort", 
                      choices = unique(data$Cohort),
                      selected = unique(data$Cohort))
    
    updateSliderInput(session, "age", 
                      min = min_age, max = max_age, 
                      value = c(min_age, max_age))
    
    updatePickerInput(session, "gender", 
                      choices = c(na.omit(unique(data$Gender)), "NA"),
                      selected = c(na.omit(unique(data$Gender)), "NA"))
    
    updatePickerInput(session, "dx", 
                      choices = c(na.omit(unique(data$DX)), "NA"),
                      selected = c(na.omit(unique(data$DX)), "NA"))
    
    updateSliderInput(session, "mmse", value = c(0, 30))
    updateSliderInput(session, "cdr", value = c(0, 3))
    
    updateCheckboxGroupInput(session, "neuroimaging_filter_vars", selected = character(0))
    updateCheckboxGroupInput(session, "biomarkers_filter_vars", selected = character(0))
  })
  
  
  # Funció reactiva per filtrar dades
  deduplicated_data <- reactive({
    req(input$cohort, input$gender, input$dx, input$age, input$mmse, input$cdr)
    
    data_filtered <- data %>%
      filter(
        Cohort %in% input$cohort,
        (Gender %in% input$gender | (is.na(Gender) & "NA" %in% input$gender)),
        (DX %in% input$dx | (is.na(DX) & "NA" %in% input$dx)),
        # Edat
        (is.na(Age_at_visit) | 
           (Age_at_visit >= input$age[1] & Age_at_visit <= input$age[2])),
        # MMSE
        (is.na(MMSE) | 
           (MMSE >= input$mmse[1] & MMSE <= input$mmse[2])),
        # CDR
        (is.na(CDR) | 
           (CDR >= input$cdr[1] & CDR <= input$cdr[2]))
      )
    
    # Filtrar per neuroimatge si s'ha seleccionat alguna variable
    if (!is.null(input$neuroimaging_filter_vars) && length(input$neuroimaging_filter_vars) > 0) {
      data_filtered <- data_filtered %>%
        filter(rowSums(!is.na(select(., all_of(input$neuroimaging_filter_vars)))) > 0)
    }
    
    # Filtrar per biomarcadors si s'ha seleccionat alguna variable
    if (!is.null(input$biomarkers_filter_vars) && length(input$biomarkers_filter_vars) > 0) {
      data_filtered <- data_filtered %>%
        filter(rowSums(!is.na(select(., all_of(input$biomarkers_filter_vars)))) > 0)
    }
    
    return(data_filtered)
  })
  
  
  # Alias de les dades filtrades
  filtered_data <- reactive({
    req(deduplicated_data())
    return(deduplicated_data())
  })
  
  # Histograma de l'edat
  output$edadPlot <- renderPlotly({
    df <- filtered_data()
    if (is.null(df) || nrow(df) == 0) {
      showNotification("No data available for plotting", type = "warning")
      return(NULL)
    }
    
    df <- df %>% filter(!is.na(Age_at_visit))  # només pel gràfic!
    
    p <- ggplot(df, aes(x = Age_at_visit)) +
      geom_histogram(binwidth = 1, fill = "#69b3a2", color = "white", alpha = 0.7) +
      scale_x_continuous(
        breaks = pretty(df$Age_at_visit, n = 10) %>% round(),
        minor_breaks = NULL
      ) +
      scale_y_continuous(
        breaks = pretty(df$Age_at_visit, n = 10) %>% round(),
        minor_breaks = NULL
      ) +
      xlab("Age at Visit") +
      ylab("Frequency") +
      ggtitle("Age Distribution") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
    
    ggplotly(p)
  })
  
  # Gràfic de distribució per sexe
  output$SexPlot <- renderPlotly({
    df <- filtered_data()
    req(nrow(df) > 0)
    
    total_obs <- nrow(df)
    Sex_data <- df %>%
      group_by(Gender) %>%
      summarise(Count = n()) %>%
      ungroup()
    
    p <- ggplot(Sex_data, aes(x = "", y = Count, fill = Gender)) +
      geom_bar(stat = "identity", width = 1) +
      labs(y = "", fill = "Gender", title = paste("Observations = ", total_obs)) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      ) +
      scale_fill_manual(values = c("Male" = "#c5e0dc", "Female" = "#e6e6fa", "Unknown" = "#d3d3d3"))
    
    ggplotly(p)
  })
  
  # Gràfic circular per cohort
  output$Cohortplot <- renderPlotly({
    cohort_data <- filtered_data() %>%
      group_by(Cohort) %>%
      summarise(Count = n()) %>%
      ungroup()
    
    colors <- if (nrow(cohort_data) >= 3) {
      RColorBrewer::brewer.pal(nrow(cohort_data), "Set3")
    } else {
      RColorBrewer::brewer.pal(3, "Set3")[1:nrow(cohort_data)]
    }
    
    plot_ly(cohort_data, labels = ~Cohort, values = ~Count, type = 'pie',
            textinfo = 'percent',
            textfont = list(color = 'black'),
            insidetextorientation = 'radial',
            marker = list(colors = colors)) %>%
      layout(showlegend = TRUE, title = "Cohort Distribution")
  })
  
  # Taula amb resultats filtrats
  output$mytable <- renderDT({
    df <- filtered_data()
    req(nrow(df) > 0)
    datatable(df, options = list(
      scrollX = TRUE,
      scrollY = "250px",
      pageLength = 25
    ))
  })
  
  # Dades per exportar
  export_data <- reactive({
    df <- filtered_data()
    req(df)
    return(df)
  })
  
  # Exportació a CSV
  output$downloadCSV <- downloadHandler(
    filename = function() { paste("filtered_data_", Sys.Date(), ".csv", sep = "") },
    content = function(file) {
      write.csv2(export_data(), file, row.names = FALSE)
    }
  )
  
  # Exportació a Excel
  output$downloadXLSX <- downloadHandler(
    filename = function() { paste("filtered_data_", Sys.Date(), ".xlsx", sep = "") },
    content = function(file) {
      write_xlsx(list("Filtered Data" = export_data()), file)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
