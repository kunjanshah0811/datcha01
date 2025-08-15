# ====================== #
# GLOBAL MODULE
# ====================== #

# Library imports
library(shiny)
library(shinyjs)        # For enhanced UI functionality
library(dplyr)          # Data manipulation
library(tm)             # Text mining
library(topicmodels)    # Topic modeling
library(sentimentr)     # Sentiment analysis
library(wordcloud2)     # Word cloud visualization
library(highcharter)    # Interactive charts
library(tidytext)       # Text processing
library(reshape2)       # Data reshaping
library(ggplot2)        # Visualization
library(DT)             # Interactive tables
library(stringdist)     # String distance calculation
library(shinyBS)        # For tooltips
library(quanteda)       # Quantitative text analysis
library(quanteda.textstats) # Text statistics
library(KeynessMeasures)    # Keyness analysis
library(SnowballC)      # For stemming
library(textstem)       # For lemmatization
library(LDAvis)         # LDA visualization
library(diffobj)        # For visual text diffs
library(htmltools) # HTML tools for Shiny


# TEXT PROCESSING MODULE
text_processor = list(
  clean = function(text, use_stem = FALSE, use_lemma = FALSE) {
    corpus = Corpus(VectorSource(text)) %>%
      tm_map(content_transformer(tolower)) %>%
      tm_map(removeNumbers) %>%
      tm_map(removePunctuation) %>%
      tm_map(removeWords, stopwords(source = "smart")) %>%
      tm_map(stripWhitespace)
    
    # Apply stemming if requested
    if(use_stem) {
      corpus <- tm_map(corpus, stemDocument)
    }
    
    # Apply lemmatization if requested
    if(use_lemma) {
      text_vec <- sapply(corpus, as.character)
      text_vec <- lemmatize_strings(text_vec)
      return(text_vec)
    }
    
    sapply(corpus, as.character)
  },
  
  get_freq = function(text, gram_type = "Uni-gram") {
    if (gram_type == "Uni-gram") {
      corpus <- Corpus(VectorSource(text))
      dtm <- DocumentTermMatrix(corpus)
      freq <- colSums(as.matrix(dtm))
      data.frame(word = names(freq), freq = freq) %>% 
        arrange(desc(freq))
    } else {
      tibble(text = text) %>%
        unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
        count(bigram, sort = TRUE) %>%
        rename(word = bigram, freq = n)
    }
  }
)

# SHARED DATA PROCESSING UI
sharedUploadModuleUI <- function(id) {
  #ns <- NS(id)
  tagList(
    # No UI elements needed here since they're already in the main UI
    # This is just a placeholder to match the module structure
  )
}

# Function to handle dataset comparison
compareDatasets <- function(shared_data) {
  # Debug: Log inputs
  message("compareDatasets called")
  message("Date 1: ", as.character(shared_data$date1))
  message("Date 2: ", as.character(shared_data$date2))
  message("ID col 1: ", if (is.null(shared_data$id_col_1)) "NULL" else shared_data$id_col_1)
  message("ID col 2: ", if (is.null(shared_data$id_col_2)) "NULL" else shared_data$id_col_2)
  
  # Validate dates
  if (is.null(shared_data$date1) || is.null(shared_data$date2) || shared_data$date2 <= shared_data$date1) {
    showNotification("Error: Dataset 2 date must be after Dataset 1", type = "error")
    message("Date validation failed: date2 <= date1 or dates missing")
    return(FALSE)
  }
  
  # Validate ID columns exist in the data frames
  if (is.null(shared_data$id_col_1) || !shared_data$id_col_1 %in% names(shared_data$data1)) {
    showNotification("ID column not found in Dataset 1", type = "error")
    message("ID column validation failed for Dataset 1")
    return(FALSE)
  }
  
  if (is.null(shared_data$id_col_2) || !shared_data$id_col_2 %in% names(shared_data$data2)) {
    showNotification("ID column not found in Dataset 2", type = "error")
    message("ID column validation failed for Dataset 2")
    return(FALSE)
  }
  
  message("compareDatasets validation passed")
  return(TRUE)
}

# SHARED DATA PROCESSING
# Shared data upload and processing functions
sharedUploadModule <- function(input, output, session) {
  ns <- session$ns
  
  # Reactive values for datasets
  data_reactive <- reactiveValues(
    data1 = NULL,
    data2 = NULL,
    date1 = NULL,
    date2 = NULL,
    id_col_1 = NULL,
    id_col_2 = NULL,
    comparison_done = FALSE
  )
  
  # Load Dataset 1
  observeEvent(input$file1, {
    req(input$file1)
    data_reactive$data1 <- tryCatch({
      df <- read.csv(input$file1$datapath, stringsAsFactors = FALSE)
      if (nrow(df) == 0) stop("Dataset 1 is empty")
      df
    }, error = function(e) {
      showNotification(paste("Error loading Dataset 1:", e$message), type = "error")
      NULL
    })
    # Handle empty ID column input
    id_col <- if(isTruthy(input$id_col_1)) input$id_col_1 else NULL
    data_reactive$id_col_1 <- detect_id_column(data_reactive$data1, id_col)
    data_reactive$comparison_done <- FALSE
  })
  
  # Repeat the same fix for file2
  observeEvent(input$file2, {
    req(input$file2)
    data_reactive$data2 <- tryCatch({
      df <- read.csv(input$file2$datapath, stringsAsFactors = FALSE)
      if (nrow(df) == 0) stop("Dataset 2 is empty")
      df
    }, error = function(e) {
      showNotification(paste("Error loading Dataset 2:", e$message), type = "error")
      NULL
    })
    # Handle empty ID column input
    id_col <- if(isTruthy(input$id_col_2)) input$id_col_2 else NULL
    data_reactive$id_col_2 <- detect_id_column(data_reactive$data2, id_col)
    data_reactive$comparison_done <- FALSE
  })
  
  # Date inputs
  observe({
    data_reactive$date1 <- input$date1
    data_reactive$date2 <- input$date2
    # Reset comparison when dates change
    data_reactive$comparison_done <- FALSE
  })
  
  # ID column inputs
  observe({
    # Only update if user provides non-empty input
    if (isTruthy(input$id_col_1)) {
      data_reactive$id_col_1 <- input$id_col_1
    }
    if (isTruthy(input$id_col_2)) {
      data_reactive$id_col_2 <- input$id_col_2
    }
    data_reactive$comparison_done <- FALSE
  })
  
  # Date validation message
  output$date_validation_msg <- renderUI({
    if (is.null(data_reactive$date1) || is.null(data_reactive$date2)) {
      return(div(style = "color: red;", "Please enter dates for both datasets"))
    }
    if (data_reactive$date2 <= data_reactive$date1) {
      return(div(style = "color: red;", "Error: Dataset 2 date must be after Dataset 1"))
    }
    NULL
  })
  
  # Enable/disable compare button based on validity
  observe({
    valid <- !is.null(data_reactive$date1) && 
      !is.null(data_reactive$date2) && 
      data_reactive$date2 > data_reactive$date1 &&
      !is.null(data_reactive$data1) &&
      !is.null(data_reactive$data2)
    
    shinyjs::toggleState("compare", condition = valid)
  })
  
  return(data_reactive)
}

# Function to detect ID column
detect_id_column <- function(df, manual_name = NULL) {
  possible_ids <- c("id", "tweet_id", "comment_id", "post_id", "status_id")
  if (!is.null(manual_name) && manual_name %in% names(df)) {
    return(manual_name)
  }
  found <- intersect(possible_ids, names(df))
  if (length(found) > 0) return(found[1])
  NULL
}

# Function to get removed posts
get_removed_posts <- function(df1, df2, id_col_1, id_col_2) {
  id1 <- detect_id_column(df1, id_col_1)
  id2 <- detect_id_column(df2, id_col_2)
  
  if (is.null(id1) || is.null(id2)) {
    showNotification("No valid ID column found in one or both datasets.", type = "error")
    return(NULL)
  }
  
  df1 %>% filter(!(!!sym(id1) %in% df2[[id2]]))
}

# Function to get remaining posts
get_remaining_posts <- function(df1, df2, id_col_1, id_col_2) {
  id1 <- detect_id_column(df1, id_col_1)
  id2 <- detect_id_column(df2, id_col_2)
  
  if (is.null(id1) || is.null(id2)) return(NULL)
  
  df1 %>% filter(!!sym(id1) %in% df2[[id2]])
}