# ====================== #
# SERVER LOGIC
# ====================== #
library(shiny)
library(shinyjs)

# 1. Load the modules
source("modules/dataDeletion.R")
source("modules/dataAddition.R")
source("modules/dataEditing.R")

# 2. Define server
server <- function(input, output, session) {
  # Initialize shared reactive values
  shared_data <- reactiveValues(
    data1 = NULL,
    data2 = NULL,
    edit_distances = NULL,
    comparison_done = FALSE,
    file1_uploaded = FALSE, # Track Dataset 1 upload status
    file2_uploaded = FALSE  # Track Dataset 2 upload status
  )
  
  # Define the function at the top
  detect_id_column <- function(df, manual_name = NULL) {
    possible_ids <- c("id", "tweet_id", "comment_id", "post_id", "status_id")
    if (!is.null(manual_name) && manual_name %in% names(df)) {
      return(manual_name)
    }
    found <- intersect(possible_ids, names(df))
    if (length(found) > 0) return(found[1])
    NULL
  }
  
  # Disable Dataset 2 upload and Compare button initially
  observe({
    shinyjs::disable("file2")
    shinyjs::disable("compare")
  })
  
  # Handle Dataset 1 upload and ID validation
  observeEvent(input$file1, {
    req(input$file1)
    tryCatch({
      df1 <- read.csv(input$file1$datapath, stringsAsFactors = FALSE)
      id_col_1 <- detect_id_column(df1, input$id_col_1)
      
      if (is.null(id_col_1)) {
        output$id_validation_msg_1 <- renderUI({
          tags$p("Warning: ID column not found in Dataset 1 among predefined names ('id', 'tweet_id', 'comment_id', 'post_id', 'status_id'). Please specify explicitly.", style = "color: orange;")
        })
        output$date_validation_msg_1 <- renderUI(NULL)
        shared_data$file1_uploaded <- FALSE
      } else if (!"text" %in% names(df1)) {
        output$id_validation_msg_1 <- renderUI({
          tags$p("Error: 'text' column not found in Dataset 1.", style = "color: red;")
        })
        output$date_validation_msg_1 <- renderUI(NULL)
        shared_data$file1_uploaded <- FALSE
      } else if (shared_data$file2_uploaded && input$date1 >= input$date2) {
        output$id_validation_msg_1 <- renderUI({
          tags$p("Dataset 1 uploaded successfully.", style = "color: green;")
        })
        output$date_validation_msg_1 <- renderUI({
          tags$p("Error: Dataset 1 date must be before Dataset 2 date.", style = "color: red;")
        })
        shared_data$file1_uploaded <- FALSE
      } else {
        output$id_validation_msg_1 <- renderUI({
          tags$p("Dataset 1 uploaded successfully.", style = "color: green;")
        })
        output$date_validation_msg_1 <- renderUI({
          if (shared_data$file2_uploaded) {
            tags$p("Dataset 1 date validated successfully.", style = "color: green;")
          } else {
            NULL
          }
        })
        shared_data$data1 <- df1
        shared_data$file1_uploaded <- TRUE
        shinyjs::enable("file2")
      }
    }, error = function(e) {
      output$id_validation_msg_1 <- renderUI({
        tags$p(paste("Error reading Dataset 1:", e$message), style = "color: red;")
      })
      output$date_validation_msg_1 <- renderUI(NULL)
      shared_data$file1_uploaded <- FALSE
    })
  })
  
  # Handle manual ID column input for Dataset 1
  observeEvent(input$id_col_1, {
    req(input$file1, input$id_col_1)
    tryCatch({
      df1 <- read.csv(input$file1$datapath, stringsAsFactors = FALSE)
      if (!input$id_col_1 %in% names(df1) || !"text" %in% names(df1)) {
        output$id_validation_msg_1 <- renderUI({
          tags$p("Error: Specified ID column not found in Dataset 1 or 'text' column missing.", style = "color: red;")
        })
        output$date_validation_msg_1 <- renderUI(NULL)
        shared_data$file1_uploaded <- FALSE
      } else if (shared_data$file2_uploaded && input$date1 >= input$date2) {
        output$id_validation_msg_1 <- renderUI({
          tags$p("Dataset 1 ID column validated successfully.", style = "color: green;")
        })
        output$date_validation_msg_1 <- renderUI({
          tags$p("Error: Dataset 1 date must be before Dataset 2 date.", style = "color: red;")
        })
        shared_data$file1_uploaded <- FALSE
      } else {
        output$id_validation_msg_1 <- renderUI({
          tags$p("Dataset 1 ID column validated successfully.", style = "color: green;")
        })
        output$date_validation_msg_1 <- renderUI({
          if (shared_data$file2_uploaded) {
            tags$p("Dataset 1 date validated successfully.", style = "color: green;")
          } else {
            NULL
          }
        })
        shared_data$data1 <- df1
        shared_data$file1_uploaded <- TRUE
        shinyjs::enable("file2")
      }
    }, error = function(e) {
      output$id_validation_msg_1 <- renderUI({
        tags$p(paste("Error reading Dataset 1:", e$message), style = "color: red;")
      })
      output$date_validation_msg_1 <- renderUI(NULL)
      shared_data$file1_uploaded <- FALSE
    })
  })
  
  # Handle Dataset 2 upload and ID/date validation
  observeEvent(input$file2, {
    req(input$file2, shared_data$file1_uploaded)
    tryCatch({
      df2 <- read.csv(input$file2$datapath, stringsAsFactors = FALSE)
      id_col_2 <- detect_id_column(df2, input$id_col_2)
      
      if (is.null(id_col_2)) {
        output$id_validation_msg_2 <- renderUI({
          tags$p("Warning: ID column not found in Dataset 2 among predefined names ('id', 'tweet_id', 'comment_id', 'post_id', 'status_id'). Please specify explicitly.", style = "color: orange;")
        })
        output$date_validation_msg_2 <- renderUI(NULL)
        shared_data$file2_uploaded <- FALSE
      } else if (!"text" %in% names(df2)) {
        output$id_validation_msg_2 <- renderUI({
          tags$p("Error: 'text' column not found in Dataset 2.", style = "color: red;")
        })
        output$date_validation_msg_2 <- renderUI(NULL)
        shared_data$file2_uploaded <- FALSE
      } else if (input$date2 <= input$date1) {
        output$id_validation_msg_2 <- renderUI({
          tags$p("Dataset 2 ID column validated successfully.", style = "color: green;")
        })
        output$date_validation_msg_2 <- renderUI({
          tags$p("Error: Dataset 2 date must be after Dataset 1 date.", style = "color: red;")
        })
        shared_data$file2_uploaded <- FALSE
      } else {
        output$id_validation_msg_2 <- renderUI({
          tags$p("Dataset 2 uploaded successfully.", style = "color: green;")
        })
        output$date_validation_msg_2 <- renderUI({
          tags$p("Dataset 2 date validated successfully.", style = "color: green;")
        })
        shared_data$data2 <- df2
        shared_data$file2_uploaded <- TRUE
      }
    }, error = function(e) {
      output$id_validation_msg_2 <- renderUI({
        tags$p(paste("Error reading Dataset 2:", e$message), style = "color: red;")
      })
      output$date_validation_msg_2 <- renderUI(NULL)
      shared_data$file2_uploaded <- FALSE
    })
  })
  
  # Handle manual ID column input for Dataset 2
  observeEvent(input$id_col_2, {
    req(input$file2, input$id_col_2, shared_data$file1_uploaded)
    tryCatch({
      df2 <- read.csv(input$file2$datapath, stringsAsFactors = FALSE)
      if (!input$id_col_2 %in% names(df2) || !"text" %in% names(df2)) {
        output$id_validation_msg_2 <- renderUI({
          tags$p("Error: Specified ID column not found in Dataset 2 or 'text' column missing.", style = "color: red;")
        })
        output$date_validation_msg_2 <- renderUI(NULL)
        shared_data$file2_uploaded <- FALSE
      } else if (input$date2 <= input$date1) {
        output$id_validation_msg_2 <- renderUI({
          tags$p("Dataset 2 ID column validated successfully.", style = "color: green;")
        })
        output$date_validation_msg_2 <- renderUI({
          tags$p("Error: Dataset 2 date must be after Dataset 1 date.", style = "color: red;")
        })
        shared_data$file2_uploaded <- FALSE
      } else {
        output$id_validation_msg_2 <- renderUI({
          tags$p("Dataset 2 ID column validated successfully.", style = "color: green;")
        })
        output$date_validation_msg_2 <- renderUI({
          tags$p("Dataset 2 date validated successfully.", style = "color: green;")
        })
        shared_data$data2 <- df2
        shared_data$file2_uploaded <- TRUE
      }
    }, error = function(e) {
      output$id_validation_msg_2 <- renderUI({
        tags$p(paste("Error reading Dataset 2:", e$message), style = "color: red;")
      })
      output$date_validation_msg_2 <- renderUI(NULL)
      shared_data$file2_uploaded <- FALSE
    })
  })
  
  # Handle manual date input for Dataset 2
  observeEvent(input$date2, {
    req(input$file2, shared_data$file1_uploaded)
    tryCatch({
      df2 <- read.csv(input$file2$datapath, stringsAsFactors = FALSE)
      id_col_2 <- detect_id_column(df2, input$id_col_2)
      if (is.null(id_col_2) || !"text" %in% names(df2)) {
        # Do nothing, ID validation handles the message
      } else if (input$date2 <= input$date1) {
        output$date_validation_msg_2 <- renderUI({
          tags$p("Error: Dataset 2 date must be after Dataset 1 date.", style = "color: red;")
        })
        shared_data$file2_uploaded <- FALSE
      } else {
        output$date_validation_msg_2 <- renderUI({
          tags$p("Dataset 2 date validated successfully.", style = "color: green;")
        })
        shared_data$data2 <- df2
        shared_data$file2_uploaded <- TRUE
      }
    }, error = function(e) {
      output$id_validation_msg_2 <- renderUI({
        tags$p(paste("Error reading Dataset 2:", e$message), style = "color: red;")
      })
      output$date_validation_msg_2 <- renderUI(NULL)
      shared_data$file2_uploaded <- FALSE
    })
  })
  
  observeEvent(input$date1, {
    req(input$file1, shared_data$file1_uploaded, shared_data$file2_uploaded)
    tryCatch({
      df1 <- read.csv(input$file1$datapath, stringsAsFactors = FALSE)
      id_col_1 <- detect_id_column(df1, input$id_col_1)
      if (is.null(id_col_1) || !"text" %in% names(df1)) {
        # Do nothing, ID validation handles the message
      } else if (input$date1 >= input$date2) {
        output$date_validation_msg_1 <- renderUI({
          tags$p("Error: Dataset 1 date must be before Dataset 2 date.", style = "color: red;")
        })
        shared_data$file1_uploaded <- FALSE
      } else {
        output$date_validation_msg_1 <- renderUI({
          tags$p("Dataset 1 date validated successfully.", style = "color: green;")
        })
        shared_data$file1_uploaded <- TRUE
      }
    }, error = function(e) {
      output$id_validation_msg_1 <- renderUI({
        tags$p(paste("Error reading Dataset 1:", e$message), style = "color: red;")
      })
      output$date_validation_msg_1 <- renderUI(NULL)
      shared_data$file1_uploaded <- FALSE
    })
  })
  
  # Enable Compare button when both datasets are valid
  observe({
    if (shared_data$file1_uploaded && shared_data$file2_uploaded) {
      shinyjs::enable("compare")
    } else {
      shinyjs::disable("compare")
    }
  })
  
  # Call modules with shared data - FIXED: Direct module calls
  deletion_results <- dataDeletionModule(input, output, session, shared_data, detect_id_column)
  editing_results <- dataEditingModule(input, output, session, shared_data, detect_id_column)
  addition_results <- dataAdditionModule(input, output, session, shared_data)

  # Update shared data when available - MODIFIED to properly update data1 and data2
  observe({
    if (!is.null(deletion_results$data1())) {
      shared_data$data1 <- deletion_results$data1()
    }
  })
  
  observe({
    if (!is.null(deletion_results$data2())) {
      shared_data$data2 <- deletion_results$data2()
    }
  })
  
  observe({
    if (!is.null(editing_results$edit_distances())) {
      shared_data$edit_distances <- editing_results$edit_distances()
    }
  })
  
  observe({
    if (!is.null(deletion_results$comparison_done())) {
      shared_data$comparison_done <- deletion_results$comparison_done()
    }
  })
}