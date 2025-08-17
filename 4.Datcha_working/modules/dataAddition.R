# ====================== #
# modules/dataAddition.R
# ====================== #

dataAdditionModule <- function(input, output, session, shared_data,detect_id_column) {
  
  # Create a reactive value to track if comparison was done
  comparison_done <- reactiveVal(FALSE)
  
  # Update shared data when comparison is done
  observeEvent(input$compare, {
    req(shared_data$file1_uploaded, shared_data$file2_uploaded)
    comparison_done(TRUE)
    shared_data$comparison_done <- TRUE
  })
  
  # ===== Data Loading and Preparation
  data1 <- reactive({
    req(shared_data$data1)
    shared_data$data1
  })
  
  data2 <- reactive({
    req(shared_data$data2)
    shared_data$data2
  })
  
  # ===== Core Analysis Functions
  # Identify added posts (present in the second dataset but missing in the first)
  added_posts <- eventReactive(input$compare, {
    req(data1(), data2())
    df1 <- data1(); df2 <- data2()
    id1 <- detect_id_column(df1, input$id_col_1)
    id2 <- detect_id_column(df2, input$id_col_2)
    if (is.null(id1) || is.null(id2)) {
      showNotification("No valid ID column found in one or both datasets.", type = "error")
      return(NULL)
    }
    if (!"text" %in% names(df2)) {
      showNotification("Error: 'text' column not found in Dataset 2.", type = "error")
      return(NULL)
    }
    df2 %>% filter(!( !!sym(id2) %in% df1[[id1]] ))
  })
  #added_count <- nrow(added_posts())
  
  # Data Addition Indicators
  output$addition_quality_indicators <- renderUI({
    req(comparison_done(), added_posts(), data1(), input$date1, input$date2)
    
    days_diff <- as.numeric(difftime(input$date2, input$date1, units = "days"))
    total_posts <- nrow(data1())
    added_count <- nrow(added_posts())
    
    growth <- round((added_count / total_posts * 100), 1)
    daily_addition <- ifelse(days_diff > 0, round(added_count / days_diff, 1), "N/A")
    daily_addition_percent <- ifelse(days_diff > 0, round((added_count / days_diff / total_posts * 100), 2), "N/A")
    
    tagList(
      div(style = "margin-bottom: 15px;",
          strong("Number of Added Posts"), br(),
          span(style = "font-size: 1.2em;", added_count)
      ),
      div(style = "margin-bottom: 15px;",
          strong("Data Addition"), br(),
          span(style = "color: #28a745; font-size: 1.2em;", paste0(growth, "%"))
      ),
      div(style = "margin-bottom: 15px;",
          strong("Daily Addition Rate"), br(),
          span(style = "font-size: 1.2em;", paste(daily_addition, "posts/day"))
      ),
      div(style = "margin-bottom: 15px;",
          strong("Daily Added"), br(),
          span(style = "font-size: 1.2em;", paste(daily_addition_percent, "%/day"))
      )
    )
  })
  
  # Get original posts (present in both datasets)
  original_posts <- eventReactive(input$compare, {
    req(data1(), data2())
    df1 <- data1(); df2 <- data2()
    id1 <- detect_id_column(df1, input$id_col_1)
    id2 <- detect_id_column(df2, input$id_col_2)
    if (is.null(id1) || is.null(id2)) {
      showNotification("No valid ID column found in one or both datasets.", type = "error")
      return(NULL)
    }
    if (!"text" %in% names(df2)) {
      showNotification("Error: 'text' column not found in Dataset 2.", type = "error")
      return(NULL)
    }
    df2 %>% filter( !!sym(id2) %in% df1[[id1]] )
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
  
  # Display the number of added posts
  output$added_count <- renderText({
    req(comparison_done(), added_posts())
    if (is.null(added_posts())) {
      return("Number of Added Posts: 0 (No valid data)")
    }
    paste("Number of Added Posts:", nrow(added_posts())) # Display the count of added posts
  })
  
  # ===== Word Frequency Analysis ===== #
  output$word_freq_plot_added <- renderHighchart({
    req(comparison_done(), added_posts())
    
    text_data <- added_posts()$text
    
    if (is.null(text_data) || length(text_data) == 0 || all(is.na(text_data))) {
      showNotification("Error: No valid text data found in added posts.", type = "error")
      return(NULL)
    }
    
    cleaned_text <- tryCatch({
      text_processor$clean(text_data, use_stem = FALSE, use_lemma = TRUE)
    }, error = function(e) {
      showNotification(paste("Error processing text for added posts:", e$message), type = "error")
      return(NULL)
    })
    
    if (is.null(cleaned_text)) return(NULL)
    
    word_freq <- tryCatch({
      text_processor$get_freq(cleaned_text) %>%
        filter(freq > 1) %>%
        slice_head(n = 100)
    }, error = function(e) {
      showNotification(paste("Error calculating word frequencies for added posts:", e$message), type = "error")
      return(NULL)
    })
    
    if (is.null(word_freq) || nrow(word_freq) == 0) {
      showNotification("No valid words found after processing added posts.", type = "warning")
      return(NULL)
    }
    
    highchart() %>%
      hc_chart(type = "bar") %>%
      hc_title(text = "Added Posts") %>%
      hc_tooltip(crosshairs = TRUE, shared = FALSE, useHTML = TRUE,
                 formatter = JS("function() {
                                   var result = '<br/><span style=\"color:' + this.series.color + '\">' + this.point.category + '</span>:<b> ' + this.point.y + '</b>';
                                   return result;
                 }")) %>%
      hc_xAxis(categories = word_freq$word,
               labels = list(style = list(fontSize = '11px')), max = 20, scrollbar = list(enabled = TRUE)) %>%
      hc_add_series(name = "Word", data = word_freq$freq, type = "column",
                    color = "#4CAF50", showInLegend = FALSE)
  })
  
  output$word_freq_plot_original <- renderHighchart({
    req(comparison_done(), original_posts())
    
    text_data <- original_posts()$text
    
    if (is.null(text_data) || length(text_data) == 0 || all(is.na(text_data))) {
      showNotification("Error: No valid text data found in original posts.", type = "error")
      return(NULL)
    }
    
    cleaned_text <- tryCatch({
      text_processor$clean(text_data, use_stem = FALSE, use_lemma = TRUE)
    }, error = function(e) {
      showNotification(paste("Error processing text for original posts:", e$message), type = "error")
      return(NULL)
    })
    
    if (is.null(cleaned_text)) return(NULL)
    
    word_freq <- tryCatch({
      text_processor$get_freq(cleaned_text) %>%
        filter(freq > 1) %>%
        slice_head(n = 100)
    }, error = function(e) {
      showNotification(paste("Error calculating word frequencies for original posts:", e$message), type = "error")
      return(NULL)
    })
    
    if (is.null(word_freq) || nrow(word_freq) == 0) {
      showNotification("No valid words found after processing original posts.", type = "warning")
      return(NULL)
    }
    
    highchart() %>%
      hc_chart(type = "bar") %>%
      hc_title(text = "Original Posts") %>%
      hc_tooltip(crosshairs = TRUE, shared = FALSE, useHTML = TRUE,
                 formatter = JS("function() {
                                   var result = '<br/><span style=\"color:' + this.series.color + '\">' + this.point.category + '</span>:<b> ' + this.point.y + '</b>';
                                   return result;
                 }")) %>%
      hc_xAxis(categories = word_freq$word,
               labels = list(style = list(fontSize = '11px')), max = 20, scrollbar = list(enabled = TRUE)) %>%
      hc_add_series(name = "Word", data = word_freq$freq, type = "column",
                    color = "#2196F3", showInLegend = FALSE)
  })
  
  # ===== Sentiment Analysis ===== #
  # Calculate sentiment distribution for a text vector
  get_sentiment_distribution <- function(text_vector) {
    if (is.null(text_vector)) {
      return(data.frame(
        category = c("Negative", "Neutral", "Positive"),
        percentage = c(0, 0, 0)
      ))
    }
    
    # Process in chunks for large datasets
    chunk_size <- 500
    chunks <- split(text_vector, ceiling(seq_along(text_vector)/chunk_size))
    
    results <- lapply(chunks, function(chunk) {
      sentences <- get_sentences(chunk)
      sentiment(sentences)
    })
    
    all_scores <- unlist(lapply(results, function(x) x$sentiment))
    
    category <- cut(all_scores, 
                    breaks = c(-Inf, -0.01, 0.01, Inf),
                    labels = c("Negative", "Neutral", "Positive"))
    
    counts <- table(factor(category, levels = c("Negative", "Neutral", "Positive")))
    percentages <- prop.table(counts) * 100
    
    data.frame(
      category = names(percentages),
      percentage = as.numeric(percentages)
    )
  }
  
  # Optimized function to identify most extreme posts
  get_extreme_posts <- function(df, n = 1, type = "positive") {
    if (nrow(df) == 0) return("No data")
    
    # Process in chunks
    chunk_size <- 500
    chunks <- split(df, ceiling(seq_len(nrow(df))/chunk_size))
    
    all_scores <- lapply(chunks, function(chunk) {
      sentences <- get_sentences(chunk$text)
      scores <- sentiment_by(sentences)
      data.frame(text = chunk$text, score = scores$ave_sentiment)
    }) %>% bind_rows()
    
    if (type == "positive") {
      all_scores %>% 
        arrange(desc(score)) %>% 
        slice_head(n = n) %>% 
        pull(text) %>% 
        as.character()
    } else {
      all_scores %>% 
        arrange(score) %>% 
        slice_head(n = n) %>% 
        pull(text) %>% 
        as.character()
    }
  }
  
  # Sentiment distribution plots for added and original posts
  output$sentiment_plot_added <- renderHighchart({
    req(comparison_done(), added_posts())
    
    withProgress(message = 'Analyzing sentiment...', value = 0.5, {
      sentiment_data <- get_sentiment_distribution(added_posts()$text)
    })
    
    highchart() %>%
      hc_chart(type = "column") %>%
      hc_xAxis(categories = c("Negative", "Neutral", "Positive")) %>%
      hc_yAxis(title = list(text = "Percentage"), labels = list(format = "{value}%")) %>%
      hc_add_series(name = "Added Posts", 
                    data = sentiment_data$percentage, 
                    color = "#4CAF50") %>%
      hc_tooltip(pointFormat = "<b>{point.category}</b>: {point.y:.1f}%") %>%
      hc_plotOptions(series = list(pointPadding = 0.1, groupPadding = 0.1))
  })
  
  output$sentiment_plot_original <- renderHighchart({
    req(comparison_done(), original_posts())
    
    withProgress(message = 'Analyzing sentiment...', value = 0.5, {
      sentiment_data <- get_sentiment_distribution(original_posts()$text)
    })
    
    highchart() %>%
      hc_chart(type = "column") %>%
      hc_xAxis(categories = c("Negative", "Neutral", "Positive")) %>%
      hc_yAxis(title = list(text = "Percentage"), labels = list(format = "{value}%")) %>%
      hc_add_series(name = "Original Posts", 
                    data = sentiment_data$percentage, 
                    color = "#2196F3") %>%
      hc_tooltip(pointFormat = "<b>{point.category}</b>: {point.y:.1f}%") %>%
      hc_plotOptions(series = list(pointPadding = 0.1, groupPadding = 0.1))
  })
  
  # Reactive expressions for most extreme posts
  most_positive_added_text <- reactive({
    req(comparison_done(), added_posts())
    withProgress(message = 'Finding most positive...', value = 0.5, {
      get_extreme_posts(added_posts(), type = "positive")
    })
  })
  
  most_negative_added_text <- reactive({
    req(comparison_done(), added_posts())
    withProgress(message = 'Finding most negative...', value = 0.5, {
      get_extreme_posts(added_posts(), type = "negative")
    })
  })
  
  most_positive_original_text <- reactive({
    req(comparison_done(), original_posts())
    withProgress(message = 'Finding most positive...', value = 0.5, {
      get_extreme_posts(original_posts(), type = "positive")
    })
  })
  
  most_negative_original_text <- reactive({
    req(comparison_done(), original_posts())
    withProgress(message = 'Finding most negative...', value = 0.5, {
      get_extreme_posts(original_posts(), type = "negative")
    })
  })
  
  # Dynamic UI boxes for most extreme posts
  output$most_positive_added_box <- renderUI({
    text <- most_positive_added_text()
    if(is.null(text) || text == "No data") return(div("No data"))
    
    div(style = paste0("background: #f0f8ff; padding: 10px; border-radius: 5px;",
                       "min-height: 50px; max-height: 300px;",
                       "overflow-y: auto; white-space: pre-wrap;"),
        text)
  })
  
  output$most_negative_added_box <- renderUI({
    text <- most_negative_added_text()
    if(is.null(text) || text == "No data") return(div("No data"))
    
    div(style = paste0("background: #fff0f0; padding: 10px; border-radius: 5px;",
                       "min-height: 50px; max-height: 300px;",
                       "overflow-y: auto; white-space: pre-wrap;"),
        text)
  })
  
  output$most_positive_original_box <- renderUI({
    text <- most_positive_original_text()
    if(is.null(text) || text == "No data") return(div("No data"))
    
    div(style = paste0("background: #f0f8ff; padding: 10px; border-radius: 5px;",
                       "min-height: 50px; max-height: 300px;",
                       "overflow-y: auto; white-space: pre-wrap;"),
        text)
  })
  
  output$most_negative_original_box <- renderUI({
    text <- most_negative_original_text()
    if(is.null(text) || text == "No data") return(div("No data"))
    
    div(style = paste0("background: #fff0f0; padding: 10px; border-radius: 5px;",
                       "min-height: 50px; max-height: 300px;",
                       "overflow-y: auto; white-space: pre-wrap;"),
        text)
  })
  
  return(list(
    added_posts = added_posts,
    original_posts = original_posts,
    comparison_done = comparison_done
    )
  )
}