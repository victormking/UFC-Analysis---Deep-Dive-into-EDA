# UFC Project by Victor King - Syracuse Grad Sport Analytics 
# UFC Analysis - A dive into EDA 


# Section 1 Libraries

library(ggplot2)    # For data visualization, creating plots such as histograms, scatter plots, and boxplots
library(readr)      # For reading data from CSV files and other text files
library(dplyr)      # For data manipulation, including filtering, selecting, and summarizing data
library(car)        # For statistical analysis, including functions like ANOVA and VIF
library(purrr)      # For functional programming, working with lists, and applying functions
library(reshape2)   # For reshaping data, especially converting data between wide and long formats
library(multcomp)   # For multiple comparison procedures, useful in post-hoc testing
library(pROC)       # For plotting ROC curves and calculating AUC for binary classifiers
library(caret)      # For model training, cross-validation, and creating confusion matrices
library(iml)        # For interpretable machine learning, including SHAP value calculation
library(mlr)        # For creating and tuning machine learning models
library(ranger)     # For fast implementation of Random Forests, a classification model
library(xgboost)    # For gradient boosting, a powerful ensemble learning technique
library(e1071)      # For support vector machines (SVMs), and other functions like tuning models
library(tidyr)      # For data tidying, including functions like pivot_longer() and pivot_wider()
library(viridis)    # For colorblind-friendly color palettes, often used in ggplot2
library(GGally)     # For creating advanced plots, including pairs plots (scatterplot matrices)
library(RColorBrewer)  # For color palettes, especially sequential, diverging, and qualitative palettes
library(ggridges)   # For creating ridge plots (joy plots), a visualization of distributions
library(treemap)    # For creating treemaps, which display hierarchical data as nested rectangles
library(fmsb)       # For creating radar charts, useful for comparing multivariate data
library(networkD3)  # For creating interactive network visualizations using D3.js
library(htmlwidgets)
library(vcd)        # For visualizing categorical data, including mosaic plots and association plots
library(igraph)     # For creating and analyzing network graphs
library(ggraph)
library(lubridate)  # For working with date and time data, simplifying date manipulations
library(rpart)      # For creating decision trees, a method of classification and regression
library(rpart.plot) # For visualizing decision trees created with rpart
library(ggtern)     # For creating ternary plots using ggplot2, useful for three-component data
library(waterfalls)
# --------------------------------------------------------


# Section 2 Data Handling 

historical_data <- read.csv("historical_data.csv")
old_data <- read.csv("historical_data_old.csv")

# Replace Date and Birthdate columns in the current dataset with those from the older dataset
historical_data$Date <- old_data$Date
historical_data$Birthdate <- old_data$Birthdate

historical_data$Date <- as.Date(historical_data$Date, format = "%m/%d/%Y")
historical_data$Birthdate <- as.Date(historical_data$Birthdate, format = "%m/%d/%Y")

historical_data$WT_Class <- old_data$WT_Class

# Convert all integer columns to numeric
historical_data <- historical_data %>%
  mutate(across(where(is.integer), ~ as.numeric(.)))


write.csv(historical_data, "historical_data.csv", row.names = FALSE)

# Check the structure to ensure the changes
str(historical_data)


# Here I load the csv that I created from scrapping ESPN.com 
# I also load an older version that has the columns correct for Date, Birthdate and WT_Class.
# I then convert the data into numeric from integers.
# I then save it and check the structure to confirm that I am ready to begin.



# --------------------------------------------------------


# Section 3 Creating Aggregated Data

# Identify all numeric and integer columns
numeric_columns <- names(historical_data)[sapply(historical_data, is.numeric) | sapply(historical_data, is.integer)]

# Create a summary dataset by aggregating data per fighter and stance for all numeric/integer columns
aggregated_fighter_data <- historical_data %>%
  group_by(Fighter, Stance, WT_Class, Date, Birthdate) %>%
  summarize(across(all_of(numeric_columns), 
                   list(mean = ~mean(.x, na.rm = TRUE), 
                        sum = ~sum(.x, na.rm = TRUE), 
                        max = ~max(.x, na.rm = TRUE),
                        min = ~min(.x, na.rm = TRUE),
                        sd = ~sd(.x, na.rm = TRUE))), 
            .groups = 'drop') %>%  # Ungroup after summarization
  mutate(across(everything(), ~replace_na(.x, 0)))  # Replace NA values with 0

# first few rows of the aggregated data
head(aggregated_fighter_data)

# Save to a CSV
write.csv(aggregated_fighter_data, "aggregated_fighter_data.csv", row.names = FALSE)


# Aggregating by Fighter, Stance, WT_Class, Date, and Birthdate


# - Fighter: Each row in the aggregated dataset will correspond to a unique fighter.
# - Stance: Since a fighter might switch stances across different fights, aggregating by stance
#   helps capture the performance metrics associated with each stance.
# - WT_Class: Fighters often compete in different weight classes throughout their careers.
#   Aggregating by weight class ensures that the analysis accounts for performance in each class.
# - Date: Including the date helps in tracking the progression of a fighter's performance over time.
# - Birthdate: This allows us to keep track of the fighter's age during each fight, which might
#   be relevant for certain analyses, such as evaluating age-related performance trends.


# -------------------------------------------               


# Section 4 Summary Statistics


# Check the structure of the dataset
str(historical_data)

# Summary statistics for key numeric variables (Mean, Median, Mode, Variance)
summary(historical_data)

# Calculate median values for key numeric variables
median_values <- historical_data %>%
  summarise(across(where(is.numeric), \(x) median(x, na.rm = TRUE)))

# Calculate mode values for key numeric variables
mode_values <- historical_data %>%
  summarise(across(where(is.numeric), ~as.numeric(names(sort(table(.), decreasing = TRUE)[1]))))

# Calculate variance for key numeric variables
variance_values <- historical_data %>%
  summarise(across(where(is.numeric), \(x) var(x, na.rm = TRUE)))

# Display the calculated median, mode, and variance
print("Median Values:")
print(median_values)

print("Mode Values:")
print(mode_values)

print("Variance Values:")
print(variance_values)

# Count the number of unique fighters
num_fighters <- historical_data %>% distinct(Fighter) %>% nrow()

# Count the number of unique countries
num_countries <- historical_data %>% distinct(Country) %>% nrow()

# Get the distribution of fighters by country
fighters_by_country <- historical_data %>%
  group_by(Country) %>%
  summarise(Num_Fighters = n_distinct(Fighter)) %>%
  arrange(desc(Num_Fighters))

# Analyze the time frame of fights
time_frame <- range(historical_data$Date, na.rm = TRUE)

# results
list(
  Number_of_Fighters = num_fighters,
  Number_of_Countries = num_countries,
  Fighters_by_Country = fighters_by_country,
  Time_Frame = time_frame
)

# Frequency counts for key categorical columns I picked
frequency_counts_results <- table(historical_data$Res)
frequency_counts_team <- table(historical_data$Team)
frequency_counts_continent <- table(historical_data$Continent)
frequency_counts_country <- table(historical_data$Country)
frequency_counts_wt_class <- table(historical_data$WT_Class)
frequency_counts_stance <- table(historical_data$Stance)
frequency_counts_decision <- table(historical_data$Decision)


# Display frequency counts
print("Frequency Counts for Results:")
print(frequency_counts_results)

print("Frequency Counts for Team:")
print(frequency_counts_team)

print("Frequency Counts for Continent:")
print(frequency_counts_continent)

print("Frequency Counts for Country:")
print(frequency_counts_country)

print("Frequency Counts for WT_Class:")
print(frequency_counts_wt_class)

print("Frequency Counts for Stance:")
print(frequency_counts_stance)

print("Frequency Counts for Decision:")
print(frequency_counts_decision)


# Correlation matrix for numeric variables
correlation_matrix <- cor(historical_data[sapply(historical_data, is.numeric)])

# View the correlation matrix
print("Correlation Matrix:")
print(correlation_matrix)


# This section provides a detailed summary of the dataset by calculating key statistics, 
# including mean, median, mode, and variance for numeric variables. It also includes 
# frequency counts for categorical variables and a correlation matrix for numerical variables.

# 1. Display the structure and summary statistics of the dataset to understand the data types 
#    and the overall distribution of numeric and categorical variables.

# 2. Calculate and display median, mode, and variance for key numeric variables.

# 3. Determine the number of unique fighters and countries in the dataset and display the 
#    distribution of fighters by country.

# 4. Analyze the timeframe of the fights covered in the dataset.

# 5. Calculate frequency counts for key categorical columns to understand their distribution, 
#    including fight results, teams, continents, countries, weight classes, stances, and decisions.

# 6. Calculate the correlation matrix for numeric variables to examine relationships between them.

# 7. Display all calculated metrics, frequency counts, and the correlation matrix to summarize the data.

# --------------------------------------------------


# Section 5 EDA


# Histograms
# --- Distribution of Fighter Heights: Unique Instances Only ---

# Ensure each fighter is represented only once
unique_fighters_data <- historical_data %>%
  distinct(Fighter, .keep_all = TRUE)

# Histogram for Fighter Height (Unique Instances)
ggplot(unique_fighters_data, aes(x = Height)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  labs(title = "Distribution of Fighter Heights (Unique Instances)", 
       x = "Height (inches)", 
       y = "Frequency") +
  theme_minimal()

# Save the plot
ggsave("fighter_height_distribution_unique.pdf", width = 10, height = 6)




# Pie Charts
# --- Distribution of Fighter Stances: Unique Instances Only ---

# Ensure each fighter is represented once
unique_fighters_data <- historical_data %>%
  distinct(Fighter, .keep_all = TRUE)  # Keep only the first occurrence of each fighter

# Count the number of fighters for each stance
stance_distribution <- unique_fighters_data %>%
  count(Stance)

# Create the pie chart
pie_chart <- ggplot(stance_distribution, aes(x = "", y = n, fill = Stance)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Distribution of Fighter Stances (Unique Instances)", 
       fill = "Stance") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")

# Save the pie chart
ggsave("fighter_stance_distribution_pie_chart.pdf", plot = pie_chart, width = 8, height = 6)

# Print the pie chart
print(pie_chart)


#Line Graph 
# --- Trends in Average Rounds Over Time by Female Weight Class (With Smoothed Lines) ---

# Define female weight classes
female_classes <- c("Women's Strawweight", "Women's Flyweight", "Women's Bantamweight")

# Filter the aggregated data for these classes
female_data <- aggregated_fighter_data %>%
  filter(WT_Class %in% female_classes)

# Create the line plot using the column for average rounds
female_rounds_plot <- ggplot(female_data, aes(x = Date, y = Rnd_mean, color = WT_Class)) +
  geom_line(size = 1.2, alpha = 0.7) +  # Thicker lines with transparency for better visibility
  geom_point(size = 2, alpha = 0.7) +  # Add points with transparency
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed", size = 1) +  # Add smoothed lines
  labs(title = "Average Rounds Over Time by Female Weight Class",
       x = "Date", y = "Average Rounds per Fight", color = "Weight Class") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") +  # Use a color palette with better contrast
  theme(
    text = element_text(size = 14),  # Increase text size for better readability
    legend.position = "top"  # Move legend to the top for better visibility
  ) +
  facet_wrap(~WT_Class)  # Separate plots for each weight class

# Print and save the plot
print(female_rounds_plot)
ggsave("female_rounds_over_time_with_smooth_facet.pdf", plot = female_rounds_plot, width = 12, height = 8)



# Bar Graphs 
# --- Distribution by Method of Victory --- 


# Create a dataset with only unique fighters (remove duplicates)
unique_fighters_data <- historical_data %>%
  distinct(Fighter, .keep_all = TRUE)  # Keep only the first occurrence of each fighter

# Summarize the count for each method of victory
victory_method_counts <- unique_fighters_data %>%
  summarise(
    Decision_Unanimous = sum(Decision_Unanimous, na.rm = TRUE),
    Decision_Split = sum(Decision_Split, na.rm = TRUE),
    Decision_Majority = sum(Decision_Majority, na.rm = TRUE),
    KO_TKO = sum(KO_TKO, na.rm = TRUE),
    TKO_Doctors_Stoppage = sum(TKO_Doctors_Stoppage, na.rm = TRUE),
    Submission = sum(Submission, na.rm = TRUE),
    No_Contest = sum(No_Contest, na.rm = TRUE),
    Draw = sum(Draw, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Method", values_to = "Count")



# Create a bar chart to show the distribution of different methods of victory
victory_distribution_plot <- ggplot(victory_method_counts, aes(x = Method, y = Count, fill = Method)) +
  geom_bar(stat = "identity") +
  labs(title = "Method of Victory Distribution Among Fighters",
       x = "Method of Victory",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save the plot
ggsave("method_of_victory_distribution.pdf", plot = victory_distribution_plot, width = 10, height = 6)

# Display the plot
print(victory_distribution_plot)



#Box Plots
# --- Compare the duration of fights that ended in different outcomes (e.g., KO/TKO, Decision) --- 

# Ensure the correct column name for fight duration is used
fight_duration_plot <- ggplot(unique_fighters_data, aes(x = factor(Decision), y = Total_Fight_Time_Seconds, fill = factor(Decision))) +
  geom_boxplot() +
  labs(title = "Comparison of Fight Duration by Outcome (Unique Instances)", 
       x = "Outcome (Decision Type)", y = "Fight Duration (seconds)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

# Print and save the plot
print(fight_duration_plot)
ggsave("comparison_of_fight_duration_by_outcome.pdf", plot = fight_duration_plot, width = 10, height = 6)



#Scatter Plots 
#  --- Scatter plot for Weight vs. Total Strikes Landed ---

# Ensure the data is aggregated per fighter with relevant metrics
aggregated_fighter_data <- historical_data %>%
  group_by(Fighter) %>%
  summarize(
    Weight = mean(Weight, na.rm = TRUE),
    TDL = sum(TDL, na.rm = TRUE),  # Total Takedowns Landed
    AD = sum(AD, na.rm = TRUE),    # Total Advances
    SM = sum(SM, na.rm = TRUE)     # Total Submission Attempts
  )

# Scatter plot for Weight vs. TDL
weight_vs_tdl_plot <- ggplot(aggregated_fighter_data, aes(x = Weight, y = TDL)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Weight vs. Takedown Landed (Aggregated Data)", 
       x = "Weight (lbs)", y = "Takedown Landed (TDL)") +
  theme_minimal()

# Print and save the plot
print(weight_vs_tdl_plot)
ggsave("weight_vs_takedown_landed_aggregated.pdf", plot = weight_vs_tdl_plot, width = 10, height = 8)




# Heatmap    
# --- Correlation Matrix of Key Performance Metrics ---

# Correlation matrix for numeric variables
correlation_matrix <- cor(historical_data[sapply(historical_data, is.numeric)])

# Melt the correlation matrix
melted_cor_matrix <- melt(correlation_matrix)

# Create the heatmap
ggplot(data = melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) + 
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name = "Pearson\nCorrelation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 8, hjust = 1)) +
  coord_fixed() +
  labs(title = "Correlation Matrix Heatmap")

# Save the heatmap as PDF
ggsave("correlation_matrix_heatmap.pdf", width = 10, height = 10)




# Density
# --- Distribution of Fight Durations by Outcome: A Density Analysis ---

# Fight Duration by Outcome
ggplot(historical_data, aes(x = Total_Fight_Time_Seconds, fill = factor(Decision))) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Fight Duration by Outcome",
       x = "Fight Duration (seconds)",
       y = "Density",
       fill = "Outcome") +
  theme_minimal()

# Save the plot
ggsave("fight_duration_by_outcome_density.pdf", width = 10, height = 6)



#Pair plots 
# --- Comprehensive Pair Plot of Fighter Performance Metrics: Wins vs. Losses ---

# Reset any previous graphics device
dev.off()  # Clear any previous graphics settings

# Select the relevant columns for the pair plot
pair_plot_data <- dplyr::select(historical_data, AD, ADTB, ADHG, ADTM, ADTS, SM, RV, SR, Results)

# Save the pair plot to a PDF file
pdf("pair_plot_fighter_performance_metrics.pdf", width = 12, height = 8)  # Define the PDF file name and size

# Adjust the margins before plotting
par(mar = c(0.1, 0.1, 0.1, 0.1))  # Use smaller margins

# Generate the pair plot
pairs(pair_plot_data[, -ncol(pair_plot_data)],  # Exclude the Results column from the pairs plot
      main = "Pair Plot of Selected Fighter Performance Metrics",
      pch = 21, 
      bg = c("red", "green")[as.factor(pair_plot_data$Results)],  # Use Results column for colors
      cex.labels = 0.5,  # Smaller label size
      cex = 0.4,  # Smaller point size
      cex.main = 0.8,  # Smaller main title
      cex.axis = 0.6,  # Smaller axis text
      cex.lab = 0.6)   # Smaller axis labels

# Reset margins to default after plotting
par(mar = c(5, 4, 4, 2) + 0.1)

# Close the PDF device
dev.off()

# Set up a new plotting window with larger dimensions
windows(width = 12, height = 8)  # For Windows users, change to `X11()` for Linux



# Violin Plots
# --- Violin Plot for Average Fight Duration by Weight Class (Aggregated Data) ---

# Use the aggregated data to calculate the average fight duration per fighter
aggregated_fighter_data <- historical_data %>%
  group_by(Fighter, WT_Class) %>%
  summarize(Time_Seconds_mean = mean(Time_Seconds, na.rm = TRUE), .groups = 'drop')

# Create the violin plot
fight_duration_violin_plot <- ggplot(aggregated_fighter_data, aes(x = WT_Class, y = Time_Seconds_mean, fill = WT_Class)) +
  geom_violin(trim = FALSE) +
  labs(title = "Distribution of Average Fight Duration by Weight Class (Aggregated Data)",
       x = "Weight Class", y = "Average Fight Duration (seconds)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

# Print and save the plot
print(fight_duration_violin_plot)
ggsave("fight_duration_by_weight_class_violin_aggregated.pdf", plot = fight_duration_violin_plot, width = 10, height = 8)

numeric_columns <- names(historical_data)[sapply(historical_data, is.numeric)]  # Automatically selects all numeric columns




# Ridgeline Plots
# --- Distribution of Average Fight Duration by Stance (Aggregated Data) ---

# Create a summary dataset by aggregating data per fighter and stance for all numeric/integer columns
aggregated_fighter_data <- historical_data %>%
  group_by(Fighter, Stance, WT_Class, Date, Birthdate) %>%
  summarize(across(all_of(numeric_columns), 
                   list(mean = ~mean(.x, na.rm = TRUE), 
                        sum = ~sum(.x, na.rm = TRUE), 
                        max = ~max(.x, na.rm = TRUE),
                        min = ~min(.x, na.rm = TRUE),
                        sd = ~sd(.x, na.rm = TRUE))), 
            .groups = 'drop') %>%  # Ungroup after summarization
  mutate(across(everything(), ~replace_na(.x, 0)))  # Replace NA values with 0


# Create a ridgeline plot for Average Fight Duration by Stance
ridgeline_plot_stances <- ggplot(aggregated_fighter_data, aes(x = Time_Seconds_mean, y = Stance, fill = Stance)) +
  geom_density_ridges(alpha = 0.7, scale = 3) +  # Adjust alpha for transparency and scale for height of ridges
  labs(title = "Distribution of Average Fight Duration by Stance (Aggregated Data)", 
       x = "Average Fight Duration (seconds)", 
       y = "Stance") +
  theme_minimal() +
  theme(legend.position = "none") +  # Hide the legend if not needed
  scale_fill_viridis_d(option = "C")  # Use a color palette with better contrast

# Print and save the plot
print(ridgeline_plot_stances)
ggsave("ridgeline_plot_fight_duration_by_stance.pdf", plot = ridgeline_plot_stances, width = 10, height = 8)




#  Facet Wrap
# --- Density Plot of Fight Duration by Outcome Across Weight Classes ---

ggplot(historical_data, aes(x = Total_Fight_Time_Seconds, fill = factor(Decision))) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Fight Duration by Outcome Across Weight Classes") +
  theme_minimal() +
  facet_wrap(~WT_Class)

ggsave("density_plot_fight_duration_by_outcome.pdf", width = 10, height = 8)



# Bubble Charts:
# --- Bubble Chart: Clinch Head Strikes vs. X.HEAD ---

ggplot(unique_fighters_data, aes(x = X.Clinch_Head_Strikes, y = X.HEAD, size = KD )) +
  geom_point(alpha = 0.6, color = "orange") +
  labs(title = "Bubble Chart: Clinch Head Strikes vs. X.HEAD", 
       x = "Clinch Head Strikes", 
       y = "Ground Head Strikes", 
       size = "Knockdowns (KD)") +
  theme_minimal()

# Save the bubble chart to a PDF file
ggsave("bubble_chart_clinch_vs_head_strikes.pdf", width = 10, height = 8)




# Treemap
# --- Treemap of UFC Fighters by Continent and Country (Unique Instances) ---

treemap_data <- unique_fighters_data %>%
  group_by(Continent, Country) %>%
  summarise(Count = n(), .groups = 'drop')


treemap(treemap_data,
        index = c("Continent", "Country"),
        vSize = "Count",
        vColor = "Count",
        type = "value",
        title = "Treemap of Fighters by Continent and Country",
        palette = "Set3")

pdf("treemap_fighters_by_continent_and_country.pdf", width = 10, height = 8)  # Define the PDF file name and size

dev.off()




# Radar Chart
# --- Radar Chart: Fighter Performance Metrics ---

# Select the relevant columns for the radar chart
radar_data <- historical_data %>%
  dplyr::select(X.Clinch_Body_Strikes, X.Clinch_Head_Strikes, X.Clinch_Leg_Strikes, 
                X.Ground_Body_Strikes, X.Ground_Head_Strikes, X.Ground_Leg_Strikes, 
                X.Significant_Strikes, X.Total_Strikes, X.TK_ACC, 
                X.BODY, X.HEAD, X.LEG) %>%
  head(5)  # Selecting the first 5 fighters for simplicity

# Preparing the data for the radar chart
# Normalizing data to ensure that it fits the radar chart
max_vals <- apply(radar_data, 2, max, na.rm = TRUE)
min_vals <- apply(radar_data, 2, min, na.rm = TRUE)

# Adding the min and max values as the first two rows of the data
radar_data <- rbind(max_vals, min_vals, radar_data)

# Creating the radar chart
radarchart(radar_data, axistype = 1,
           pcol = rainbow(nrow(radar_data) - 2), pfcol = alpha(rainbow(nrow(radar_data) - 2), 0.5),
           plwd = 2, cglcol = "grey", cglty = 1, axislabcol = "grey", caxislabels = seq(0, max(max_vals), length = 5),
           cglwd = 0.8, vlcex = 0.8)

# Add a title to the radar chart
title(main = "Radar Chart: Selected Fighter Performance Metrics")

# Save the radar chart to a PDF file
pdf("radar_chart_fighter_performance_metrics.pdf", width = 10, height = 8)  # Define the PDF file name and size

# Close the PDF device
dev.off()



# Lollipop Charts
# Number of Wins by Team (Teams with at Least 7 Wins)

# Summarize the number of wins for each team and filter for teams with 7 wins or more
wins_by_team <- historical_data %>%
  group_by(Team) %>%
  summarise(Wins = sum(Results == 1, na.rm = TRUE)) %>%
  filter(Wins >= 7) %>%  # Filter for teams with 7 wins or more
  arrange(desc(Wins))

# Create the lollipop chart
ggplot(wins_by_team, aes(x = reorder(Team, -Wins), y = Wins)) +
  geom_point(color = "red", size = 4) +
  geom_segment(aes(x = Team, xend = Team, y = 0, yend = Wins)) +
  labs(title = "Number of Wins by Team (Teams with at Least 7 Wins)", 
       x = "Team", y = "Number of Wins") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

# Save the plot
ggsave("wins_by_team_lollipop.pdf", width = 10, height = 6)



# Circular Barplots
# Circular Barplot for Number of Wins by Country

# Summarize the number of wins for each country and filter for countries with 5 wins or more
wins_by_country <- historical_data %>%
  group_by(Country) %>%
  summarise(Wins = sum(Results == 1)) %>%
  filter(Wins >= 5) %>%  # Filter for countries with 5 wins or more
  arrange(desc(Wins))

# Create the circular barplot
ggplot(wins_by_country, aes(x = reorder(Country, -Wins), y = Wins)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_polar(theta = "x") +
  labs(title = "Number of Wins by Country (Circular Barplot)", x = "", y = "Number of Wins") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

# Save the plot
ggsave("wins_by_country_circular_barplot.pdf", width = 10, height = 8)



# Waterfall Chart
# --- Fight Outcome Contributions: A Waterfall Chart ---


# Filter data to only include wins by submission
submission_wins_data <- historical_data %>%
  filter(Submission == 1 & Results == 1)  # Assuming Submission column indicates submission wins and Results == 1 indicates a win

# Aggregate data by Year
submission_wins_data$Year <- format(submission_wins_data$Date, "%Y")  # Extract year from Date
yearly_submission_wins <- submission_wins_data %>%
  group_by(Year) %>%
  summarise(Total_Submission_Wins = n()) %>%  # Count number of submission wins per year
  arrange(Year)

# Calculate cumulative submission wins and changes per year
yearly_submission_wins <- yearly_submission_wins %>%
  mutate(Cumulative_Submission_Wins = cumsum(Total_Submission_Wins),  # Cumulative sum
         Change_Submission_Wins = c(0, diff(Cumulative_Submission_Wins)))  # Year-over-year changes

# Create a named vector for the waterfall chart using years as labels
waterfall_data <- setNames(
  yearly_submission_wins$Change_Submission_Wins,  # Use changes in submission wins for waterfall values
  yearly_submission_wins$Year  # Use years as labels
)

# Include a starting and ending point in the waterfall data
waterfall_data <- c("Starting Point" = 0, waterfall_data, "End Point" = sum(yearly_submission_wins$Total_Submission_Wins))

# Create the waterfall chart
waterfall(
  values = waterfall_data,
  labels = names(waterfall_data),  # Specify labels using the names of the vector
  rect_text_size = 3,
  fill_by_sign = TRUE
)

# Save the plot
ggsave("waterfall_chart_submission_wins_by_year.pdf", width = 10, height = 6)




# Cleveland dot
# --- Average Fight Duration by Continent: A Cleveland Dot Plot ---

# Calculate the average fight duration per continent
avg_fight_duration_continent <- historical_data %>%
  group_by(Continent) %>%
  summarize(Average_Fight_Duration = mean(Time_Seconds, na.rm = TRUE))

# Create the Cleveland dot plot
cleveland_dot_plot <- ggplot(avg_fight_duration_continent, aes(x = Average_Fight_Duration, y = Continent)) +
  geom_point(color = "blue", size = 3) +
  labs(title = "Average Fight Duration by Continent", x = "Average Fight Duration (seconds)", y = "Continent") +
  theme_minimal()

# Print and save the plot
print(cleveland_dot_plot)
ggsave("avg_fight_duration_by_continent.pdf", plot = cleveland_dot_plot, width = 10, height = 8)




# Sankey diagram
# --- Sankey Diagram of Fight Outcomes by Continent ---


# Define the data for the Sankey diagram: Continent to Fight Outcome
sankey_data <- data.frame(
  source = c("Africa", "Africa", "Asia", "Asia", "Europe", "Europe", "North America", "North America", "Oceania", "Oceania", "South America", "South America"),
  target = c("KO/TKO", "Decision", "KO/TKO", "Decision", "KO/TKO", "Decision", "KO/TKO", "Decision", "KO/TKO", "Decision", "KO/TKO", "Decision"),
  value = c(5, 3, 7, 4, 12, 8, 18, 11, 3, 2, 4, 3)
)

# Convert source and target to factors and then to numeric indices
sankey_data$source <- as.numeric(factor(sankey_data$source)) - 1
sankey_data$target <- as.numeric(factor(sankey_data$target)) - 1

# Create a node data frame
nodes <- data.frame(name = unique(c("Africa", "Asia", "Europe", "North America", "Oceania", "South America", "KO/TKO", "Decision")))

# Create the Sankey diagram
sankey_diagram <- sankeyNetwork(
  Links = sankey_data,
  Nodes = nodes,
  Source = "source",
  Target = "target",
  Value = "value",
  NodeID = "name",
  units = "fights",
  fontSize = 12,
  nodeWidth = 30
)

# Save the Sankey diagram to an HTML file
saveWidget(sankey_diagram, file = "sankey_diagram_fight_outcomes_by_continent.html")






# Define women's weight classes
women_classes <- c("Women's Strawweight", "Women's Flyweight", "Women's Bantamweight")

weight_class_groups <- list(
  "Fly & Bantam" = c("Flyweight", "Bantamweight"),
  "Feather & Light" = c("Featherweight", "Lightweight"),
  "Welter & Middle" = c("Welterweight", "Middleweight"),
  "Light Heavy & Heavy" = c("Light Heavyweight", "Heavyweight")
)

# Assign men's weight classes to their respective groups
historical_data$Weight_Group <- NA  # Initialize with NA

# Loop through the groups and assign to 'Weight_Group'
for (group_name in names(weight_class_groups)) {
  historical_data$Weight_Group[historical_data$WT_Class %in% weight_class_groups[[group_name]]] <- group_name
}

# For women's weight classes, keep them separate
historical_data$Weight_Group[historical_data$WT_Class %in% women_classes] <- historical_data$WT_Class[historical_data$WT_Class %in% women_classes]

# Create a new column to identify Men's and Women's divisions
historical_data$Division <- ifelse(historical_data$WT_Class %in% women_classes, "Women", "Men")

# Ensure 'Weight_Group' and 'Division' columns are factors
historical_data$Weight_Group <- factor(historical_data$Weight_Group)
historical_data$Division <- factor(historical_data$Division, levels = c("Men", "Women"))

# Save the mosaic plot to a PDF file
pdf(file = "mosaic_plot_fight_result_separated.pdf", width = 10, height = 8)  # Open a PDF device

# Generate the Mosaic Plot using 'Res' (Win/Loss/Draw) instead of 'Decision'
mosaic(~ Weight_Group + Res | Division, 
       data = historical_data, 
       shade = TRUE, 
       legend = TRUE,
       labeling_args = list(
         rot_labels = c(left = 0, top = 90),  # Rotate women's labels to avoid overlap
         gp_labels = gpar(fontsize = 10)  # Decrease font size for labels
       ),
       main = "Mosaic Plot of Weight Class Groups vs. Fight Result (Men's and Women's Divisions)")

dev.off()  # Close the PDF device properly




# Cumulative Plot
# --- Cumulative Growth of Total Strikes in UFC Fights Over Time--- 

# Aggregate data by date to calculate cumulative total strikes
cumulative_data <- historical_data %>%
  group_by(Date) %>%
  summarize(Cumulative_Strikes = sum(X.Total_Strikes, na.rm = TRUE)) %>%
  arrange(Date) %>%
  mutate(Cumulative_Frequency = cumsum(Cumulative_Strikes))

# Create the cumulative frequency plot
cumulative_plot <- ggplot(cumulative_data, aes(x = Date, y = Cumulative_Frequency)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Cumulative Frequency of Total Strikes Over Time",
       x = "Date",
       y = "Cumulative Number of Strikes") +
  theme_minimal()

# Print the plot
print(cumulative_plot)

# Save the plot as a PDF
ggsave("cumulative_frequency_plot.pdf", plot = cumulative_plot, width = 10, height = 6)



# Ternary Plot 
# --- Ternary Plot of Fighter Strike Distribution: Body, Head, and Leg Strikes ---

# Aggregate data by fighter to calculate the average proportion of each strike type
ternary_data <- historical_data %>%
  group_by(Fighter) %>%
  summarize(
    Body_Strikes = sum(X.BODY, na.rm = TRUE),
    Head_Strikes = sum(X.HEAD, na.rm = TRUE),
    Leg_Strikes = sum(X.LEG, na.rm = TRUE)
  ) %>%
  mutate(Total_Strikes = Body_Strikes + Head_Strikes + Leg_Strikes) %>%
  filter(Total_Strikes > 0) %>%  # Filter out cases with zero total strikes
  mutate(
    Body_Prop = Body_Strikes / Total_Strikes,
    Head_Prop = Head_Strikes / Total_Strikes,
    Leg_Prop = Leg_Strikes / Total_Strikes
  )

# Create the ternary plot
ternary_plot <- ggtern(data = ternary_data, aes(x = Head_Prop, y = Body_Prop, z = Leg_Prop)) +
  geom_point(color = "blue", size = 3, alpha = 0.7) +
  labs(
    title = "Ternary Plot of Strike Proportions",
    x = "Proportion of Head Strikes",
    y = "Proportion of Body Strikes",
    z = "Proportion of Leg Strikes"
  ) +
  theme_minimal()

# Print the plot
print(ternary_plot)

# Save the plot as a PDF file
ggsave("ternary_plot_strike_proportions.pdf", plot = ternary_plot, width = 8, height = 8)


# --- Network Graph of Selected UFC Fights: Connections Between Fighters ---
# Network Graph


# Filter dataset for unique fighter-team combinations
fighter_team_data <- historical_data %>%
  distinct(Fighter, Team) %>%
  filter(!is.na(Team) & Team != "")  # Remove NA and empty teams

# Create the edges dataframe: Fighters within the same team
edges <- fighter_team_data %>%
  group_by(Team) %>%
  filter(n() > 3) %>%  # Keep only teams with more than 3 fighters
  do(data.frame(from = combn(.$Fighter, 2, simplify = TRUE)[1, ], 
                to = combn(.$Fighter, 2, simplify = TRUE)[2, ])) %>%
  ungroup()

# Check for any unexpected issues in column names or data
colnames(edges) <- c("from", "to")
edges <- edges %>% filter(!is.na(from) & from != "" & !is.na(to) & to != "")  # Remove rows with NA or empty values

# Ensure there are no empty names in nodes
fighter_team_data <- fighter_team_data %>% filter(Fighter != "" & !is.na(Fighter))

# Create graph from edges
g <- graph_from_data_frame(edges, directed = FALSE)

# Set colors for the nodes based on their team
V(g)$color <- as.factor(fighter_team_data$Team)[match(V(g)$name, fighter_team_data$Fighter)]

# Plot the network graph using igraph
plot(g, 
     vertex.size = 8, 
     vertex.label.cex = 0.7, 
     vertex.label.color = "black", 
     edge.color = "gray", 
     edge.width = 1, 
     main = "Network Graph of Fighters Based on Training Teams")

# Save the plot as a PDF or PNG
pdf("network_graph_fighters_teams.pdf", width = 10, height = 8)
plot(g, 
     vertex.size = 8, 
     vertex.label.cex = 0.7, 
     vertex.label.color = "black", 
     edge.color = "gray", 
     edge.width = 1, 
     main = "Network Graph of Fighters Based on Training Teams")
dev.off()

png("network_graph_fighters_teams.png", width = 1000, height = 800)
plot(g, 
     vertex.size = 8, 
     vertex.label.cex = 0.7, 
     vertex.label.color = "black", 
     edge.color = "gray", 
     edge.width = 1, 
     main = "Network Graph of Fighters Based on Training Teams")
dev.off()


# --- Hierarchical Clustering of UFC Fighters by Continent Based on Physical Metrics --- 
# Dendrogram visualization 

# Ensure the Continent column is in the aggregated dataset
aggregated_fighter_data <- historical_data %>%
  group_by(Fighter, Stance, WT_Class, Date, Birthdate, Continent) %>%  # Ensure Continent is included
  summarize(across(all_of(numeric_columns), 
                   list(mean = ~mean(.x, na.rm = TRUE), 
                        sum = ~sum(.x, na.rm = TRUE), 
                        max = ~max(.x, na.rm = TRUE),
                        min = ~min(.x, na.rm = TRUE),
                        sd = ~sd(.x, na.rm = TRUE))), 
            .groups = 'drop') %>%
  mutate(across(everything(), ~replace_na(.x, 0)))  # Replace NA values with 0

# Aggregate the data by continent, calculating the mean for numeric variables
dendrogram_data_continent <- aggregated_fighter_data %>%
  group_by(Continent) %>%
  summarise(across(c(Height_mean, Weight_mean, Reach_mean), mean, na.rm = TRUE))

# Calculate the distance matrix based on the aggregated data
dist_matrix_continent <- dist(dendrogram_data_continent[, -1])  # Exclude the 'Continent' column for clustering

# Perform hierarchical clustering using the Ward method
hc_continent <- hclust(dist_matrix_continent, method = "ward.D2")

# Plot the dendrogram with continent labels
plot(hc_continent, labels = dendrogram_data_continent$Continent, 
     main = "Dendrogram of Continents Based on Fighter Metrics", 
     xlab = "Continents", sub = "Metrics: Height, Weight, Reach", cex = 0.7)

# Add colored rectangles to highlight clusters
rect.hclust(hc_continent, k = 3, border = c("red", "blue", "green"))  # 'k' is the number of clusters

# Add legend to indicate which metrics were used
legend("topright", legend = c("Height", "Weight", "Reach"), col = c("black"), lwd = 1, bty = "n")

# Save the dendrogram with colors and metric information as a PDF
pdf("dendrogram_colored_ufc_continents_with_metrics.pdf", width = 10, height = 8)
plot(hc_continent, labels = dendrogram_data_continent$Continent, 
     main = "Dendrogram of Continents Based on Fighter Metrics", 
     xlab = "Continents", sub = "Metrics: Height, Weight, Reach", cex = 0.7)
rect.hclust(hc_continent, k = 3, border = c("red", "blue", "green"))  # Add colored rectangles to the PDF
legend("topright", legend = c("Height", "Weight", "Reach"), col = c("black"), lwd = 1, bty = "n")
dev.off()

# ------------