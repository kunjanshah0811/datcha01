# ====================== #
# UI DEFINITION
# ====================== #
ui <- navbarPage("Datcha",
                 # 0. Overview Page
                 tabPanel("Overview",
                          fluidPage(
                            useShinyjs(), 
                            tags$head(
                              tags$style(HTML("
               .form-group, .shiny-input-container { margin-bottom: 8px; }
               .form-group { padding-bottom: 0px; }
               .shiny-date-input { padding-bottom: 0px; margin-bottom: 0px; }
               .tooltip-inner { max-width: 300px; padding: 10px; background-color: #f8f9fa; color: #212529; border: 1px solid #dee2e6; border-radius: 4px; }
               .fa-info-circle { color: #007bff; margin-left: 5px; cursor: pointer; }
               .highlight-topic { stroke: #FF5722 !important; stroke-width: 3px !important; filter: drop-shadow(0 0 5px rgba(255, 87, 34, 0.5)); }
               #ldavis_output { height: 80vh; min-height: 500px; width: 100%; border: 1px solid #ddd; border-radius: 4px; padding: 10px; background: white; }
               .topic-controls { margin-top: 15px; padding: 10px; background: #f8f9fa; border-radius: 5px; }
               .topic-nav-buttons { display: flex; justify-content: space-between; margin-top: 10px; }
               .datatable { margin: 20px 0; }
               .datatable td { max-width: 300px; overflow-wrap: break-word; }
             ")),
                              tags$script(HTML('
               $(window).on("resize", function() {
                 var windowHeight = $(window).height();
                 var headerHeight = $(".navbar").outerHeight();
                 var controlsHeight = $(".topic-controls").outerHeight();
                 var newHeight = windowHeight - headerHeight - controlsHeight - 100;
                 $("#ldavis_output").height(newHeight);
               }).trigger("resize");
             ')),
                              tags$script(HTML('
               $(document).ready(function() {
                 $("[data-toggle=\'tooltip\']").tooltip();
               });
             ')),
                              tags$link(rel = "stylesheet", type = "text/css", href = "diffobj.css")
                            ),
                            
                            h2("Welcome to Datcha"),
                            p("Upload your datasets below to compare social media data collected at different times."),
                            
                            sidebarLayout(
                              sidebarPanel(
                                width = 4,
                                h4("Dataset Comparison"),
                                fluidRow(
                                  column(12, fileInput("file1", "Dataset 1 (.csv)", accept = ".csv")),
                                  column(6, textInput("id_col_1", "ID Column (optional)", placeholder = "e.g. 'id' or 'tweet_id'")),
                                  column(6, dateInput("date1", "Collection Date for Dataset 1", value = Sys.Date()-30))
                                ),
                                uiOutput("id_validation_msg_1"),
                                uiOutput("date_validation_msg_1"),
                                hr(),
                                fluidRow(
                                  column(12, fileInput("file2", "Dataset 2 (.csv)", accept = ".csv")),
                                  column(6, textInput("id_col_2", "ID Column (optional)", placeholder = "e.g. 'id' or 'tweet_id'")),
                                  column(6, dateInput("date2", "Collection Date for Dataset 2", value = Sys.Date()))
                                ),
                                uiOutput("id_validation_msg_2"),
                                uiOutput("date_validation_msg_2"),
                                hr(),
                                actionButton("compare", "Compare Datasets", class = "btn-primary", width = "100%")
                              ),
                              # hr(),
                              # h4("Data Quality Indicators"),
                              # textOutput("completeness"),
                              # textOutput("data_loss"),
                              # textOutput("removed_per_day"),
                              # textOutput("removal_rate_daily"),
                              
                              # hr(),
                              # h4("Dataset Overview"),
                              # textOutput("dataset1_count"),
                              # textOutput("dataset2_count"),
                              # textOutput("removed_count"),
                              # textOutput("overview_added_count"),
                              # textOutput("edited_post_count"),
                              
                              # hr(),
                              # h4("Data Addition Indicators"),
                              # uiOutput("addition_quality_indicators"),
                              
                              # hr(),
                              # h4("Data Editing"),
                              # uiOutput("data_editing_ui"),
                              # uiOutput("edited_post_ratio")
                              mainPanel(
                                width = 8,
                                fluidRow(
                                  column(6,
                                         h4("How to Use Datcha:"),
                                         tags$ol(
                                           tags$li("Upload your two datasets above and set their collection dates"),
                                           tags$li("Click 'Compare Datasets' to analyze the data"),
                                           tags$li("Navigate to the different tabs to explore specific analyses:"),
                                           tags$ul(
                                             tags$li("Data Deletion: Analyze removed posts"),
                                             tags$li("Data Addition: Identify newly added content"),
                                             tags$li("Data Edition: View edited posts and text changes")
                                           ),
                                           tags$li("Use the visualizations to understand patterns in your data")
                                         ),
                                         hr(),
                                         h4("Key Features:"),
                                         tags$ul(
                                           tags$li("Identifies removed, added, and edited posts"),
                                           tags$li("Analyzes word frequency and key terms"),
                                           tags$li("Performs sentiment and topic analysis"),
                                           tags$li("Calculates data quality metrics"),
                                           tags$li("Visualizes text differences")
                                         ),
                                         hr(),
                                         h4("Data Requirements:"),
                                         p("Please upload CSV files with:"),
                                         tags$ul(
                                           tags$li("A column containing unique post IDs"),
                                           tags$li("A column named 'text' containing the post content")
                                         )
                                  ),
                                  column(6,
                                         h4("Dataset Overview"),
                                         textOutput("dataset1_count"),
                                         textOutput("dataset2_count"),
                                         textOutput("removed_count"),
                                         textOutput("overview_added_count"),
                                         textOutput("edited_post_count")
                                  )
                                )
                              )
                            )
                          )
                 ),
                 
                 # Data Deletion Tab
                 tabPanel("Data Deletion",
                          fluidPage(
                            sidebarLayout(
                              sidebarPanel(
                                width = 3,
                                h4("Summary Statistics"),
                                uiOutput("deletion_quality_indicators"),
                                # div(style = "margin-bottom: 20px;",
                                #     uiOutput("deletion_quality_indicators")
                                # ),
                                hr(),
                                
                                # Topic Modeling Controls (only visible when Topic Modeling tab is active)
                                conditionalPanel(
                                  condition = "input.data_deletion_tabs == 'Topic Modeling'",
                                  div(class = "topic-controls",
                                      h4("Topic Controls"),
                                      sliderInput("num_topics", "Number of Topics:", 
                                                  min = 2, max = 10, value = 5, step = 1),
                                      radioButtons("topic_dataset", "Show Topics For:",
                                                   choices = c("Removed Posts", "Remaining Posts", "Combined View"),
                                                   selected = "Removed Posts"),
                                      hr(),
                                      div(class = "topic-nav-buttons",
                                          actionButton("prev_topic", "Previous Topic", class = "btn-sm"),
                                          actionButton("next_topic", "Next Topic", class = "btn-sm"),
                                          actionButton("clear_topic", "Clear", class = "btn-sm btn-danger")
                                      )
                                  )
                                )
                              ),
                              
                              mainPanel(
                                width = 9,
                                tabsetPanel(
                                  id = "data_deletion_tabs",
                                  tabPanel("Word Frequency", 
                                           fluidRow(
                                             column(6, h4("Removed Posts"), highchartOutput("word_freq_plot_removed")),
                                             column(6, h4("Remaining Posts"), highchartOutput("word_freq_plot_remaining"))
                                           )
                                  ),
                                  tabPanel("Keyness Analysis",
                                           fluidRow(
                                             column(12,
                                                    uiOutput("keyness_controls"),
                                                    highchartOutput("keyness_plot"),
                                                    uiOutput("keyness_interpretation")
                                             )
                                           )
                                  ),
                                  tabPanel("Topic Modeling",
                                           div(style = "display: flex; flex-direction: column; height: calc(100vh - 150px);",
                                               div(style = "flex: 1; min-height: 0;",
                                                   uiOutput("ldavis_output")
                                               )
                                           )
                                  ),
                                  tabPanel("Sentiment Analysis", 
                                           fluidRow(
                                             column(6, h4("Removed Posts Sentiment"), 
                                                    highchartOutput("sentiment_plot_removed")),
                                             column(6, h4("Remaining Posts Sentiment"), 
                                                    highchartOutput("sentiment_plot_remaining"))
                                           ),
                                           hr(),
                                           h4("Most Extreme Posts"),
                                           fluidRow(
                                             column(6, 
                                                    h5("Most Positive (Removed)"),
                                                    uiOutput("most_positive_removed_box")
                                             ),
                                             column(6, 
                                                    h5("Most Negative (Removed)"),
                                                    uiOutput("most_negative_removed_box")
                                             )
                                           ),
                                           fluidRow(
                                             column(6, 
                                                    h5("Most Positive (Remaining)"),
                                                    uiOutput("most_positive_remaining_box")
                                             ),
                                             column(6, 
                                                    h5("Most Negative (Remaining)"),
                                                    uiOutput("most_negative_remaining_box")
                                             )
                                           )
                                  )
                                )
                              )
                            )
                          )
                 ),
                 
                 # Data Addition Tab
                 tabPanel("Data Addition",
                          fluidPage(
                            useShinyjs(),
                            sidebarPanel(
                              width = 3,
                              h4("Summary Statistics"),
                              uiOutput("addition_quality_indicators"),
                              #textOutput("dataset1_count_addition"),
                              #textOutput("dataset2_count_addition"),
                              #textOutput("added_count"),
                              textOutput("growth_rate"),
                              textOutput("daily_addition_rate")
                            ),
                            mainPanel(
                              width = 9,
                              tabsetPanel(
                                id = "data_addition",
                                tabPanel("Word Frequency", 
                                         fluidRow(
                                           column(6, h4("Added Posts"), highchartOutput("word_freq_plot_added")),
                                           column(6, h4("Original Posts"), highchartOutput("word_freq_plot_original"))
                                         )
                                ),
                                tabPanel("Keyness Analysis", h4("Keyness Analysis - Coming Soon")),
                                tabPanel("Topic Modeling", h4("Topic Modeling - Coming Soon")),
                                tabPanel("Sentiment Analysis",
                                         fluidRow(
                                           column(6, h4("Added Posts Sentiment"), 
                                                  highchartOutput("sentiment_plot_added")),
                                           column(6, h4("Original Posts Sentiment"), 
                                                  highchartOutput("sentiment_plot_original"))
                                         ),
                                         hr(),
                                         h4("Most Extreme Posts"),
                                         fluidRow(
                                           column(6, 
                                                  h5("Most Positive (Added)"),
                                                  uiOutput("most_positive_added_box")
                                           ),
                                           column(6, 
                                                  h5("Most Negative (Added)"),
                                                  uiOutput("most_negative_added_box")
                                           )
                                         ),
                                         fluidRow(
                                           column(6, 
                                                  h5("Most Positive (Original)"),
                                                  uiOutput("most_positive_original_box")
                                           ),
                                           column(6, 
                                                  h5("Most Negative (Original)"),
                                                  uiOutput("most_negative_original_box")
                                           )
                                         )
                                )
                              )
                            )
                          )
                 ),
                 
                 # Data Edition Tab (updated with sidebar)
                 tabPanel("Data Editing",
                          fluidPage(
                            sidebarLayout(
                              sidebarPanel(
                                width = 3,
                                h4("Editing Statistics"),
                                div(style = "margin-bottom: 20px;",
                                    uiOutput("editing_stats_ui")
                                ),
                                hr(),
                                h4("Most Edited Posts"),
                                p("View the posts with the most significant changes between datasets."),
                                numericInput("num_edited_posts", "Number of posts to show:", 
                                             value = 20, min = 5, max = 100, step = 5)
                              ),
                              
                              mainPanel(
                                width = 9,
                                tabsetPanel(
                                  id = "data_editing_tabs",
                                  tabPanel("Text Changes",
                                           fluidRow(
                                             column(12,
                                                    h4("Most Edited Posts"),
                                                    dataTableOutput("most_edited_posts")
                                             )
                                           )
                                  )
                                )
                              )
                            )
                          )
                 ),
                 
                 # Documentation Tab
                 tabPanel("Documentation",
                          fluidPage(
                            h2("About Datcha"),
                            p("Datcha compares social media datasets over time."),
                            hr(),
                            h4("How to Use"),
                            tags$ul(
                              tags$li("Upload two .csv files with ID and text columns."),
                              tags$li("Click Compare to analyze removed and edited posts."),
                              tags$li("Explore the tabs for sentiment, topics, word usage, and changes.")
                            ),
                            hr(),
                            h4("Credits"),
                            p("Built by Dr Yannik and Kunjan using R and Shiny."),
                            p("Packages used: quanteda, sentimentr, topicmodels, diffobj, and more."),
                            hr(),
                            h4("Contact"),
                            p("For support, contact your team lead.")
                          )
                 ) #  fluidPage
) #navbarPage
