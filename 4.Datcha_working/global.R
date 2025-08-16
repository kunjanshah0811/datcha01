# ====================== #
# GLOBAL FILE - LIBRARY IMPORTS
# ====================== #
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

# ====================== #
# 2. TEXT PROCESSING MODULE
# ====================== #
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