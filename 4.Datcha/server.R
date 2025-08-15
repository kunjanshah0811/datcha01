# ====================== #
# SERVER LOGIC
# ====================== #
library(shiny)
library(shinyjs)

# 1. Load the modules
source("modules/dataDeletion.R")
source("modules/dataAddition.R")
source("modules/dataEditing.R")

# 2. Define server logic
server <- function(input, output, session) { 
  # Initialize shared modules
  shared_data <- callModule(sharedUploadModule, "upload")
  
  # Compare button handler - enhanced debugging
  observeEvent(input$compare, {
    # Ensure datasets are loaded
    if (is.null(shared_data$data1) || is.null(shared_data$data2)) {
      showNotification("Error: One or both datasets are not loaded.", type = "error")
      message("Compare button: Datasets not loaded")
      return()
    }
    
    # Debug: Log dataset details
    message("Compare button clicked")
    message("Dataset 1 rows: ", nrow(shared_data$data1))
    message("Dataset 2 rows: ", nrow(shared_data$data2))
    message("Date 1: ", as.character(shared_data$date1))
    message("Date 2: ", as.character(shared_data$date2))
    
    # Force ID column detection
    shared_data$id_col_1 <- detect_id_column(shared_data$data1, input$id_col_1)
    shared_data$id_col_2 <- detect_id_column(shared_data$data2, input$id_col_2)
    
    message("ID column for Dataset 1: ", if (is.null(shared_data$id_col_1)) "NULL" else shared_data$id_col_1)
    message("ID column for Dataset 2: ", if (is.null(shared_data$id_col_2)) "NULL" else shared_data$id_col_2)
    
    # Validate datasets
    message("Validating datasets...")
    if (!compareDatasets(shared_data)) {
      message("Validation failed")
      showNotification("Validation failed. Check ID columns or dates.", type = "error")
      return()
    }
    
    # If validation passes, set comparison_done to TRUE
    shared_data$comparison_done <- TRUE
    message("Comparison successful, comparison_done set to TRUE")
    showNotification("Datasets compared successfully!", type = "message")
  })
  
  # Debug: Monitor comparison_done status
  observe({
    message("comparison_done status: ", shared_data$comparison_done)
  })
  
  # Render Overview tab outputs to confirm comparison
  output$dataset1_count <- renderText({
    req(shared_data$comparison_done, shared_data$data1)
    paste("Dataset 1 Posts:", nrow(shared_data$data1))
  })
  
  output$dataset2_count <- renderText({
    req(shared_data$comparison_done, shared_data$data2)
    paste("Dataset 2 Posts:", nrow(shared_data$data2))
  })
  
  output$removed_count <- renderText({
    req(shared_data$comparison_done, shared_data$data1, shared_data$data2, shared_data$id_col_1, shared_data$id_col_2)
    removed <- get_removed_posts(shared_data$data1, shared_data$data2, shared_data$id_col_1, shared_data$id_col_2)
    paste("Removed Posts:", if (is.null(removed)) 0 else nrow(removed))
  })
  
  output$edited_post_count <- renderText({
    req(shared_data$comparison_done)
    paste("Edited Posts: Not yet calculated (check dataEditingModule)")
  })
  
  output$completeness <- renderText({
    req(shared_data$comparison_done)
    "Completeness: Not yet calculated"
  })
  
  output$data_loss <- renderText({
    req(shared_data$comparison_done)
    "Data Loss: Not yet calculated"
  })
  
  output$removed_per_day <- renderText({
    req(shared_data$comparison_done)
    "Removed Per Day: Not yet calculated"
  })
  
  output$removal_rate_daily <- renderText({
    req(shared_data$comparison_done)
    "Removal Rate Daily: Not yet calculated"
  })
  
  # Call modules
  deletion_results <- callModule(dataDeletionModule, "deletion", shared_data)
  addition_results <- callModule(dataAdditionModule, "addition", shared_data)
  editing_results <- callModule(dataEditingModule, "editing", shared_data)
}