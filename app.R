library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(readxl)
library(writexl)
library(DT)
library(lubridate)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Quality Control Data Hub"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Upload", tabName = "upload", icon = icon("upload")),
      menuItem("View Data", tabName = "view", icon = icon("table")),
      menuItem("Quality Analysis", tabName = "analysis", icon = icon("chart-bar")),
      menuItem("Reports", tabName = "reports", icon = icon("file-pdf"))
    )
  ),
  dashboardBody(
    tabItems(
      # Tab 1: Data Upload
      tabItem(tabName = "upload",
              fluidRow(
                box(
                  title = "Upload Quality Data", width = 6, status = "primary",
                  fileInput("file_upload", "Choose CSV/Excel File",
                            accept = c(".csv", ".xlsx", ".xls")),
                  textInput("analyst_name", "Your Name:", placeholder = "Enter your name"),
                  selectInput("department", "Department:",
                              choices = c("Production", "Lab", "QC", "R&D")),
                  dateInput("collection_date", "Collection Date:", value = Sys.Date()),
                  actionButton("submit", "Submit Data", class = "btn-success")
                ),
                box(
                  title = "Data Template", width = 6, status = "info",
                  downloadButton("download_template", "Download Template"),
                  tags$hr(),
                  tags$p("Required columns in your data:"),
                  tags$ul(
                    tags$li("sample_id - Unique sample identifier"),
                    tags$li("parameter - Quality parameter name"),
                    tags$li("value - Measured value"),
                    tags$li("unit - Measurement unit"),
                    tags$li("specification - Target specification")
                  )
                )
              )
      ),
      
      # Tab 2: View Data
      tabItem(tabName = "view",
              fluidRow(
                box(
                  title = "All Collected Data", width = 12,
                  DTOutput("data_table"),
                  downloadButton("download_all", "Download All Data")
                )
              )
      ),
      
      # Tab 3: Quality Analysis
      tabItem(tabName = "analysis",
              fluidRow(
                box(
                  title = "Analysis Controls", width = 3,
                  selectInput("analysis_parameter", "Select Parameter:",
                              choices = NULL),
                  dateRangeInput("date_range", "Date Range:",
                                 start = Sys.Date() - 30, end = Sys.Date()),
                  selectInput("department_filter", "Department:",
                              choices = c("All", "Production", "Lab", "QC", "R&D"))
                ),
                box(
                  title = "Control Chart", width = 9,
                  plotOutput("control_chart")
                )
              ),
              fluidRow(
                box(
                  title = "Summary Statistics", width = 6,
                  tableOutput("summary_stats")
                ),
                box(
                  title = "Outliers Detection", width = 6,
                  tableOutput("outliers_table")
                )
              )
      ),
      
      # Tab 4: Reports
      tabItem(tabName = "reports",
              fluidRow(
                box(
                  title = "Generate Reports", width = 6,
                  selectInput("report_type", "Report Type:",
                              choices = c("Daily Summary", "Weekly Analysis", 
                                          "Monthly Quality Report")),
                  dateInput("report_date", "Report Date:", value = Sys.Date()),
                  actionButton("generate_report", "Generate Report"),
                  downloadButton("download_report", "Download Report")
                )
              )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive value to store all data
  all_data <- reactiveVal(data.frame())
  
  # Download template
  output$download_template <- downloadHandler(
    filename = function() {
      "quality_data_template.csv"
    },
    content = function(file) {
      template <- data.frame(
        sample_id = character(),
        parameter = character(),
        value = numeric(),
        unit = character(),
        specification = numeric(),
        stringsAsFactors = FALSE
      )
      write.csv(template, file, row.names = FALSE)
    }
  )
  
  # Handle file upload
  observeEvent(input$file_upload, {
    req(input$file_upload)
    
    tryCatch({
      if (grepl("\\.csv$", input$file_upload$name)) {
        new_data <- read.csv(input$file_upload$datapath)
      } else if (grepl("\\.xlsx$|\\.xls$", input$file_upload$name)) {
        new_data <- read_excel(input$file_upload$datapath)
      }
      
      # Add metadata
      new_data$analyst_name <- input$analyst_name
      new_data$department <- input$department
      new_data$collection_date <- input$collection_date
      new_data$upload_timestamp <- Sys.time()
      
      # Combine with existing data
      current_data <- all_data()
      updated_data <- bind_rows(current_data, new_data)
      all_data(updated_data)
      
      showNotification("Data uploaded successfully!", type = "message")
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  observeEvent(input$submit,
               showNotification("Data sent successfully"))
  
  # Display data table
  output$data_table <- renderDT({
    req(all_data())
    datatable(all_data(), options = list(scrollX = TRUE))
  })
  
  # Download all data
  output$download_all <- downloadHandler(
    filename = function() {
      paste("all_quality_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(all_data(), file, row.names = FALSE)
    }
  )
  
  # Update parameter choices based on available data
  observe({
    req(all_data())
    params <- unique(all_data()$parameter)
    updateSelectInput(session, "analysis_parameter", choices = params)
  })
  
  # Control chart
  output$control_chart <- renderPlot({
    req(all_data(), input$analysis_parameter)
    
    filtered_data <- all_data() %>%
      filter(parameter == input$analysis_parameter,
             collection_date >= input$date_range[1],
             collection_date <= input$date_range[2])
    
    if (input$department_filter != "All") {
      filtered_data <- filtered_data %>%
        filter(department == input$department_filter)
    }
    
    if (nrow(filtered_data) == 0) return()
    
    ggplot(filtered_data, aes(x = collection_date, y = value)) +
      geom_point(aes(color = department)) +
      geom_line(alpha = 0.5) +
      geom_hline(yintercept = mean(filtered_data$specification, na.rm = TRUE), 
                 linetype = "dashed", color = "blue") +
      labs(title = paste("Control Chart for", input$analysis_parameter),
           x = "Date", y = "Value") +
      theme_minimal()
  })
  
  # Summary statistics
  output$summary_stats <- renderTable({
    req(all_data(), input$analysis_parameter)
    
    filtered_data <- all_data() %>%
      filter(parameter == input$analysis_parameter,
             collection_date >= input$date_range[1],
             collection_date <= input$date_range[2])
    
    if (nrow(filtered_data) == 0) return()
    
    filtered_data %>%
      summarise(
        Count = n(),
        Mean = round(mean(value, na.rm = TRUE), 3),
        SD = round(sd(value, na.rm = TRUE), 3),
        Min = round(min(value, na.rm = TRUE), 3),
        Max = round(max(value, na.rm = TRUE), 3),
        `Within Spec` = sum(value <= specification, na.rm = TRUE)
      )
  })
  
  # Outliers detection
  output$outliers_table <- renderTable({
    req(all_data(), input$analysis_parameter)
    
    filtered_data <- all_data() %>%
      filter(parameter == input$analysis_parameter,
             collection_date >= input$date_range[1],
             collection_date <= input$date_range[2])
    
    if (nrow(filtered_data) == 0) return()
    
    # Simple outlier detection (values beyond 2 SD from mean)
    mean_val <- mean(filtered_data$value, na.rm = TRUE)
    sd_val <- sd(filtered_data$value, na.rm = TRUE)
    
    outliers <- filtered_data %>%
      filter(abs(value - mean_val) > 2 * sd_val) %>%
      select(sample_id, value, specification, department, analyst_name)
    
    outliers
  })
}

# Run the application
shinyApp(ui = ui, server = server)