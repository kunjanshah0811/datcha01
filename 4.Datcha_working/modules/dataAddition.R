# ====================== #
# modules/dataAddition.R
# ====================== #

dataAdditionModule <- function(input, output, session, shared_data) {
  
  # Create a reactive value to track if comparison was done
  comparison_done <- reactiveVal(FALSE)
  
  # Update shared data when comparison is done
  observeEvent(input$compare_addition, {
    comparison_done(TRUE)
    shared_data$comparison_done <- TRUE
    shared_data$data1 <- data1()
    shared_data$data2 <- data2()
  })
  
  # ===== Data Loading and Preparation
  
  # Load the first dataset
  data1 <- reactive({
    req(input$file1_addition)  # Ensure the file is uploaded
    read.csv(input$file1_addition$datapath, stringsAsFactors = FALSE)
  })
  
  # Load the second dataset
  data2 <- reactive({
    req(input$file2_addition)  # Ensure the file is uploaded
    read.csv(input$file2_addition$datapath, stringsAsFactors = FALSE)
  })
  
  # Function to detect the correct ID column
  detect_id_column <- function(df, manual_name = NULL) {
    possible_ids <- c("id", "tweet_id", "comment_id")
    if (!is.null(manual_name) && manual_name %in% names(df)) {
      return(manual_name)
    }
    found <- intersect(possible_ids, names(df))
    if (length(found) > 0) return(found[1])
    NULL
  }
  
  # ===== Core Analysis Functions
  
  # Date validation message
  output$date_validation_msg_addition <- renderUI({
    if (is.null(input$date1_addition) || is.null(input$date2_addition)) {
      return(div(style = "color: red;", "Please enter dates for both datasets"))
    }
    if (input$date2_addition <= input$date1_addition) {
      return(div(style = "color: red;", "Error: Dataset 2 date must be after Dataset 1"))
    }
    return(NULL)
  })
  
  # Enable/disable compare button based on date validity
  observe({
    if (!is.null(input$date1_addition) && !is.null(input$date2_addition) && input$date2_addition > input$date1_addition) {
      shinyjs::enable("compare_addition")
    } else {
      shinyjs::disable("compare_addition")
    }
  })
  
  # Calculate the number of posts in Dataset 1
  output$dataset1_count_addition <- renderText({
    req(comparison_done(), data1())
    paste("Number of posts in Dataset 1:", nrow(data1()))
  })
  
  # Calculate the number of posts in Dataset 2
  output$dataset2_count_addition <- renderText({
    req(comparison_done(), data2())
    paste("Number of posts in Dataset 2:", nrow(data2()))
  })
  
  # Identify added posts (present in the second dataset but missing in the first)
  added_posts <- eventReactive(input$compare_addition, {
    req(data1(), data2())
    df1 <- data1(); df2 <- data2()
    id1 <- detect_id_column(df1, input$id_col_1_addition)
    id2 <- detect_id_column(df2, input$id_col_2_addition)
    if (is.null(id1) || is.null(id2)) {
      showNotification("No valid ID column found in one or both datasets.", type = "error")
      return(NULL)
    }
    df2 %>% filter(!( !!sym(id2) %in% df1[[id1]] ))
  })
  
  # Get original posts (present in both datasets)
  original_posts <- eventReactive(input$compare_addition, {
    req(data1(), data2())
    df1 <- data1(); df2 <- data2()
    id1 <- detect_id_column(df1, input$id_col_1_addition)
    id2 <- detect_id_column(df2, input$id_col_2_addition)
    if (is.null(id1) || is.null(id2)) return(NULL)
    df2 %>% filter( !!sym(id2) %in% df1[[id1]] )
  })
  
  # Display the number of added posts
  output$added_count <- renderText({
    req(comparison_done(), added_posts())
    paste("Number of Added Posts:", nrow(added_posts()))
  })
  
  # Calculate growth rate
  output$growth_rate <- renderText({
    req(comparison_done(), data1(), data2())
    growth <- (nrow(data2()) - nrow(data1())) / nrow(data1()) * 100
    paste("Growth Rate:", round(growth, 1), "%")
  })
  
    
    # Calculate daily addition rate
    output$daily_addition_rate <- renderText({
      req(comparison_done(), added_posts(), input$date1_addition, input$date2_addition)
      days_diff <- as.numeric(difftime(input$date2_addition, input$date1_addition, units = "days"))
      if(days_diff <= 0) return("Invalid date range")
      paste("Daily Addition Rate:", round(nrow(added_posts())/days_diff, 1), "posts/day")
    })
    
    # ===== Word Frequency Analysis ===== #
    output$word_freq_plot_added <- renderHighchart({
      req(comparison_done(), added_posts())
      
      text_data <- added_posts()$text
      
      if (is.null(text_data) || length(text_data) == 0) {
        showNotification("Error: No text data found in added posts.", type = "error")
        return(NULL)
      }
      
      cleaned_text <- text_processor$clean(text_data, use_stem = FALSE, use_lemma = TRUE)
      word_freq <- text_processor$get_freq(cleaned_text) %>%
        filter(freq > 1) %>%
        slice(1:100)
      
      highchart() %>%
        hc_chart(type = "bar") %>%
        hc_title(text = "Added Posts") %>%
        hc_tooltip(crosshairs = TRUE, shared = FALSE, useHTML = TRUE,
                   formatter = JS(paste0("function() {
                                     var result='';
                                     result='<br/><span style=\\'color:'+this.series.color+'\\'>'+this.point.category+'</span>:<b> '
                                     +this.point.y + '</b>';
                                     return result;
    }"))) %>%
        hc_xAxis(categories = word_freq[1:100,]$word,
                 labels = list(style = list(fontSize = '11px')), max = 20, scrollbar = list(enabled = TRUE)) %>%
        hc_add_series(name = "Word", data = word_freq[1:100,]$freq, type = "column",
                      color = "#4CAF50", showInLegend = FALSE)
    })
    
    output$word_freq_plot_original <- renderHighchart({
      req(comparison_done(), original_posts())
      
      text_data <- original_posts()$text
      
      if (is.null(text_data) || length(text_data) == 0) {
        showNotification("Error: No text data found in original posts.", type = "error")
        return(NULL)
      }
      
      cleaned_text <- text_processor$clean(text_data, use_stem = FALSE, use_lemma = TRUE)
      word_freq <- text_processor$get_freq(cleaned_text) %>%
        filter(freq > 1) %>%
        slice(1:100)
      
      highchart() %>%
        hc_chart(type = "bar") %>%
        hc_title(text = "Original Posts") %>%
        hc_tooltip(crosshairs = TRUE, shared = FALSE, useHTML = TRUE,
                   formatter = JS(paste0("function() {
                                     var result='';
                                     result='<br/><span style=\\'color:'+this.series.color+'\\'>'+this.point.category+'</span>:<b> '
                                     +this.point.y + '</b>';
                                     return result;
    }"))) %>%
        hc_xAxis(categories = word_freq[1:100,]$word,
                 labels = list(style = list(fontSize = '11px')), max = 20, scrollbar = list(enabled = TRUE)) %>%
        hc_add_series(name = "Word", data = word_freq[1:100,]$freq, type = "column",
                      color = "#2196F3", showInLegend = FALSE)
    })
    
    return(list(
      added_posts = added_posts,
      original_posts = original_posts
    ))
}