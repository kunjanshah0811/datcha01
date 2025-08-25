[![Ask DeepWiki](https://deepwiki.com/badge.svg)](https://deepwiki.com/kunjanshah0811/datcha01)

# Datcha Application Documentation
Overview
Datcha is a Shiny-based web application designed to compare two social media datasets collected at different times. It analyzes changes in posts, including deletions, additions, and edits, and provides insights through visualizations and metrics such as word frequency, keyness analysis, topic modeling, and sentiment analysis. The app is built using R and leverages packages like quanteda, sentimentr, topicmodels, diffobj, and highcharter.
Getting Started
Prerequisites
------------
R and Shiny: The app requires R and the Shiny framework.
Required Packages: Listed in global.R, including shiny, shinyjs, dplyr, tm, topicmodels, sentimentr, wordcloud2, highcharter, tidytext, DT, stringdist, shinyBS, quanteda, KeynessMeasures, SnowballC, textstem, LDAvis, diffobj, htmltools, and bslib.
Directory Setup: Ensure a www/ directory exists for storing diffobj.css.
------------
Running the App
The app is launched via app.R, which sources:

ui.R: Defines the user interface.
server.R: Contains server logic.
global.R: Includes global configurations, libraries, and shared functions.
Module files (dataDeletion.R, dataAddition.R, dataEditing.R): Handle specific analyses.

Run the app with:
source("app.R")

Data Requirements
Datcha requires two CSV files, each containing:

A unique ID column (e.g., id, tweet_id, post_id, comment_id, status_id) to identify posts.
A text column named text containing post content.
---------------
Users can manually specify ID column names if they differ from the predefined set. The datasets represent social media posts collected at two different times, with the first dataset's collection date earlier than the second.
User Interface (ui.R)
Layout
The app uses a navbarPage with a custom dark blue theme (#1c4474) and includes five tabs:
---------------
Overview: Introduces the app and allows dataset uploads.
Data Deletion: Analyzes posts removed between datasets.
Data Addition: Analyzes posts added in the second dataset.
Data Editing: Analyzes changes in matching posts.
Documentation: Provides app details and usage instructions.

Styling

Navbar: Dark blue background with white text.
Buttons: Primary buttons are blue (#1c4474), default buttons are purple (#a02b93), and the "Compare" button is always blue.
Tooltips: Provide explanations for metrics and controls.
Responsive Design: Adjusts visualizations (e.g., LDAvis output) based on window size.

Server Logic (server.R)
Structure
The server logic integrates three modules:

dataDeletionModule: Handles deletion analysis.
dataAdditionModule: Handles addition analysis.
dataEditingModule: Handles edit analysis.
-------------
These modules share data via a shared_data reactive object defined in global.R. The server updates shared_data based on module outputs and triggers UI updates.
Shared Data
The shared_data reactive object stores:
-------------
data1 and data2: The uploaded datasets.
edit_distances: Levenshtein distances for edited posts.
comparison_done: Boolean indicating if comparison is complete.
file1_uploaded and file2_uploaded: Booleans tracking valid uploads.
--------------
Global Functions (global.R)
Text Processing
The text_processor object provides:
--------------
clean: Converts text to lowercase, removes numbers, punctuation, stopwords, and applies stemming or lemmatization.
get_freq: Calculates word or bigram frequencies using tm or tidytext.

ID Detection
The detect_id_column function identifies the ID column by checking for common names (id, tweet_id, etc.) or using a user-specified name.
Data Handling
The common_data_handler function:

Disables the "Dataset 2" upload and "Compare" button until Dataset 1 is valid.
Validates uploads for ID and text columns, and ensures date1 < date2.
Displays validation messages (green for success, orange for warnings, red for errors).

Dataset Upload and Validation
Upload Process

Dataset 1:
User uploads a CSV file via fileInput("file1").
Optional: Specify ID column name in textInput("id_col_1").
Set collection date via dateInput("date1").


Dataset 2:
Enabled only after a valid Dataset 1 upload.
Similar inputs: fileInput("file2"), textInput("id_col_2"), dateInput("date2").



Validation

ID Column: Must exist in the dataset (auto-detected or manually specified).
Text Column: Must be named text.
Date: date1 must be before date2.
Messages:
Success: Green message (e.g., "Dataset 1 uploaded successfully").
Warning: Orange message if ID column not found among predefined names.
Error: Red message for missing text column, invalid ID, or incorrect dates.



The "Compare" button is enabled only when both datasets are valid (file1_uploaded and file2_uploaded are TRUE).
Tabs and Features
1. Overview Tab

Purpose: Allows dataset uploads and provides a summary of dataset statistics.
Features:
Upload Interface: Users upload two CSV files, specify ID columns (optional), and set collection dates.
Validation Messages: Displayed below each file input to indicate upload status.
Dataset Stats:
Number of posts in Dataset 1 and Dataset 2.
Number of deleted posts.
Number of added posts.
Number of edited posts.


Instructions: Lists steps to use the app and data requirements.
Key Features: Highlights capabilities like deletion/addition analysis, sentiment analysis, and visualizations.


Logic:
The common_data_handler in global.R validates uploads and updates shared_data.
Outputs like dataset1_count, dataset2_count, removed_count, overview_added_count, and edited_post_count are rendered in server.R using module results.



2. Data Deletion Tab (dataDeletion.R)

Purpose: Analyzes posts present in Dataset 1 but absent in Dataset 2.
Features:
Summary Statistics:
Completeness: Percentage of posts retained in Dataset 2 (nrow(data2) / nrow(data1) * 100).
Data Loss: Percentage of posts removed (nrow(removed_posts) / nrow(data1) * 100).
Daily Removed: Average posts removed per day (nrow(removed_posts) / days_diff).
Removal Rate: Daily removal as a percentage of total posts.


Sub-tabs:
Word Frequency: Bar charts comparing word frequencies in removed vs. remaining posts using highcharter.
Keyness Analysis: Identifies distinctive terms in removed vs. remaining posts using quanteda (Log-likelihood and Effect Size).
Topic Modeling: Visualizes topics in removed posts using LDAvis with controls for topic number and navigation.
Sentiment Analysis: Compares sentiment distributions in removed vs. remaining posts and highlights most positive/negative posts.




Logic:
Removed Posts: Calculated as data1 %>% filter(!(id1 %in% data2[[id2]])).
Remaining Posts: Calculated as data1 %>% filter(id1 %in% data2[[id2]]).
Word Frequency: Uses text_processor$clean and get_freq to process text and generate bar charts.
Keyness: Uses quanteda to compute Log-likelihood and Effect Size (ELL) for term differences.
Topic Modeling: Applies LDA with topicmodels and visualizes with LDAvis.
Sentiment: Uses sentimentr to compute sentiment scores and plot distributions.



3. Data Addition Tab (dataAddition.R)

Purpose: Analyzes posts present in Dataset 2 but absent in Dataset 1.
Features:
Summary Statistics:
Number of added posts.
Data Addition: Percentage increase (nrow(added_posts) / nrow(data1) * 100).
Daily Addition Rate: Posts added per day (nrow(added_posts) / days_diff).
Daily Added: Daily addition as a percentage.


Sub-tabs:
Word Frequency: Bar charts for added vs. original posts.
Keyness Analysis: Identifies distinctive terms in added vs. original posts.
Topic Modeling: Visualizes topics in added, original, or combined posts with controls for topic number and dataset selection.
Sentiment Analysis: Compares sentiment in added vs. original posts and highlights extreme posts.




Logic:
Added Posts: Calculated as data2 %>% filter(!(id2 %in% data1[[id1]])).
Original Posts: Calculated as data2 %>% filter(id2 %in% data1[[id1]]).
Analysis: Similar to Data Deletion, using text_processor, quanteda, topicmodels, and sentimentr.



4. Data Editing Tab (dataEditing.R)

Purpose: Analyzes changes in posts present in both datasets.
Features:
Editing Statistics:
Mean Edit Distance: Average Levenshtein distance between matching posts.
Mean Normalized Distance: Edit distance divided by the longer text length (0-1 scale).
Edited Post Ratio: Percentage of matched posts with changes.


Most Edited Posts: Displays a table of posts with the highest edit distances, showing text differences using diffobj.
Control: Numeric input (num_edited_posts) to set the number of posts displayed (5–100).


Logic:
Edit Distance: Uses stringdist::stringdist to compute Levenshtein distances for matching posts (inner_join by ID).
Text Differences: Visualized with diffChr from diffobj in a side-by-side format.
Table: Rendered with DT for interactive display, including ID, differences, edit distance, and normalized distance.



5. Documentation Tab

Purpose: Provides static information about the app.
Content:
About: Describes Datcha’s purpose.
How to Use: Lists steps for uploading and analyzing data.
Credits: Acknowledges developers (Dr. Yannik and Kunjan) and packages used.
Contact: Directs users to contact their team lead for support.



Key Features and Logic
1. Dataset Comparison

Logic: The "Compare" button triggers analysis in all modules when both datasets are valid. It uses eventReactive to compute removed, added, and edited posts based on ID column matches.
Validation: Ensures ID and text columns exist and dates are in the correct order.

2. Word Frequency Analysis

Logic: Uses text_processor$clean to preprocess text (lowercase, remove stopwords, etc.) and get_freq to compute frequencies. Top 100 words are plotted using highcharter.

3. Keyness Analysis

Logic: Uses quanteda to compute Log-likelihood and Effect Size (ELL) for terms distinctive to one group (e.g., removed vs. remaining). Visualized as bar charts with tooltips explaining metrics.

4. Topic Modeling

Logic: Applies LDA with topicmodels to identify topics. Visualized with LDAvis for interactive exploration. Users can adjust the number of topics (2–10) and navigate topics with buttons.

5. Sentiment Analysis

Logic: Uses sentimentr to compute sentiment scores. Plots distributions and highlights the most positive/negative posts in styled boxes.

6. Text Differences

Logic: Computes Levenshtein distances for matching posts and visualizes changes with diffobj. The DT table allows users to explore edited posts interactively.

7. Data Quality Metrics

Logic: Calculates metrics like completeness, data loss, growth, and daily rates based on dataset sizes and date differences.

Error Handling

Invalid Data: Notifications are shown for missing ID/text columns or invalid dates.
Empty Data: Checks for empty or invalid text data prevent crashes in analyses.
Topic Modeling: Requires at least 5 non-empty documents to proceed.

Limitations

Requires specific column names (text for content, specific ID names or manual input).
Assumes Dataset 1 is collected before Dataset 2.
Topic modeling may fail with insufficient or low-quality text data.
Performance depends on dataset size due to text processing and visualization.

Conclusion
Datcha provides a comprehensive tool for analyzing social media dataset changes over time. Its modular design and interactive visualizations make it suitable for researchers and analysts studying post dynamics. Future improvements could include support for additional file formats, enhanced performance for large datasets, and more advanced text analysis techniques.
