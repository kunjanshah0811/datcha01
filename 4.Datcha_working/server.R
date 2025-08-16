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
    comparison_done = FALSE
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