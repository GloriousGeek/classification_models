# list currently installed packages
#   note: the concept of libraries and packages in R
library()
# a more detailed list of currently installed packages
in_packages <- installed.packages()
head(in_packages)
colnames(in_packages)

# Installing packages
# install.packages("readxl")
# install.packages("summarytools")
install.packages("ggcorrplot")
install.packages("ggplot2")
install.packages("corrplot")

# Load Libraries
library(readxl)
library(dplyr) # For data manipulation
library(summarytools) # For data summary and inspection
library(ggcorrplot)
library(corrplot)

# File name
file_path <- 'satisfaction_2015.xlsx'
sheet_name <- 'satisfaction_v2'

# Loading data
?read_excel
data <- read_excel(file_path, sheet = sheet_name)
# Display
print(head(data))
str(data) # Structure of DS
summary(data) # 129880 rows

############################

# Data Inspection and Cleaning: Missing Values, Duplicates, Unique Categories
null_rows_wifi <- data[data$`Inflight wifi service`==0, ]
null_rows_wifi

# Create a copy of the original data for cleaning
data_cleaned <- data

# Step1: Handling Missing Values: NA and empty strings
?sapply
missing_values <- sapply(data, function(x) sum(is.na(x) | x == "")) # lambda function
missing_values 
rows_with_missing <- rowSums(is.na(data) | data == "") > 0
n_missing_rows <- sum(rows_with_missing)
cat("Number of rows with missing values:", n_missing_rows, "\n") # 393 in Arrival Delay
# Remove rows with missing values
data_cleaned <- data_cleaned[!rows_with_missing, ]
cat("Rows with missing values removed. Dataset size: ", nrow(data_cleaned), "\n")

# Step2: Duplicate rows check 
duplicates <- data_cleaned[duplicated(data_cleaned), ]
n_duplicates <- nrow(duplicates)
cat("Number of duplicate rows: ", n_duplicates, "\n") # No duplicates, no need to clean

# Explore inconsistencies in categorical variables
categorical_columns <- sapply(data_cleaned, is.character)
cat_summary <- lapply(data_cleaned[, categorical_columns], unique)
cat_summary
# Basic frequency analysis for categorical variables
categorical_counts <- data_cleaned %>%
  summarise(across(where(is.character), ~ n_distinct(.)))
print(categorical_counts)

# Summary: 393 rows with missing values, 0 duplicates and categories are consistent.

#############################
# Step3: Outlier Detection

# List of numeric columns to check outliers
numeric_cols <- colnames(data_cleaned)[sapply(data_cleaned, is.numeric)]

# Testing on Arrival
boxplot_arrival <- boxplot(data_cleaned$`Arrival Delay in Minutes`)
boxplot_arrival$out

outlier_summary <- list() # Initialize a summary list to store outlier counts

# Loop through numeric columns to create boxplots and detect outliers
for (col in numeric_cols) {
  # Create boxplot and capture outlier values
  boxplot_result <- boxplot(data_cleaned[[col]], main = paste("Boxplot of", col),
                            xlab = col, ylab = "Values", col = "lightblue")
  
  num_outliers <- length(boxplot_result$out) # Count number of outliers
  outlier_summary[[col]] <- num_outliers # Store count in summary list
  
  # Print the detected outliers
  if (length(boxplot_result$out) > 0) {
    cat("\nOutliers detected in", col, ":\n")
    print(boxplot_result$out)
    
    # Extract rows with outliers
    outlier_rows <- data_cleaned[data_cleaned[[col]] %in% boxplot_result$out, ]
    cat("Number of outlier rows: ", nrow(outlier_rows), "\n")
    print(outlier_rows)
  } else {
    cat("\nNo outliers detected in", col, "\n")
  }
}

# Outliers Summary (fetching from the for loop above): 
cat("\nSummary of Outliers Detected:\n")
for (col in names(outlier_summary)) {
  cat("Number of outlier rows in '", col, "': ", outlier_summary[[col]], "\n", sep = "")
}

# Plotting Detected Boxplots
boxplot(data_cleaned$`Flight Distance`)
boxplot(data_cleaned$`Checkin service`)
boxplot(data_cleaned$`Departure Delay in Minutes`)
boxplot(data_cleaned$`Arrival Delay in Minutes`)

# Treatment of outliers

###########################################
# Step4: Data Exploration 

# 4.1 Numeric Data
# Histogram for numeric columns
numeric_cols <- colnames(data_cleaned)[sapply(data_cleaned, is.numeric)]
for (col in numeric_cols) {
  hist(data_cleaned[[col]], main = paste("Histogram of", col), xlab = col, col = "lightblue")
}

# Scatter plot for relationships
plot(data_cleaned$`Flight Distance`, data_cleaned$`Arrival Delay in Minutes`, 
     main = "Flight Distance vs Arrival Delay", xlab = "Flight Distance", ylab = "Arrival Delay")

# 4.2 Categorical Data
# Bar plot for categorical variables
categorical_cols <- colnames(data_cleaned)[sapply(data_cleaned, is.character)]
for (col in categorical_cols) {
  barplot(table(data_cleaned[[col]]), main = paste("Barplot of", col), col = "lightblue")
}

# 4.3 Relationships: Correlation and Heatmap
# Correlation matrix
numeric_data <- data_cleaned[, sapply(data_cleaned, is.numeric)]
cor_matrix <- cor(numeric_data, use = "complete.obs")
heatmap(cor_matrix, main = "Correlation Heatmap")
correlation_matrix <- cor(numeric_data, use = "complete.obs")

# Select only numeric columns
numeric_data <- data_cleaned[, sapply(data_cleaned, is.numeric)]

# Scatter plot for Arrival vs. Departure Delays
plot(data_cleaned$`Departure Delay in Minutes`, data_cleaned$`Arrival Delay in Minutes`,
     main = "Departure vs Arrival Delay", xlab = "Departure Delay", ylab = "Arrival Delay", col = "blue")
## Consider to drop one

boxplot(data_cleaned$`satisfaction_v2` ~ data_cleaned$`Seat comfort`, 
        main = "Satisfaction vs Seat Comfort", xlab = "Seat Comfort", ylab = "Satisfaction")

# Plot correlation matrix with labels
ggcorrplot(cor_matrix, 
           method = "circle",  # Choose 'circle' or 'square'
           type = "lower",  # Show lower triangle
           lab = TRUE,  # Display correlation values
           lab_size = 3,  # Adjust text size
           title = "Correlation Matrix with Values",
           colors = c("red", "white", "blue"))  # Color scheme

# Target specific correlations
cor(data_cleaned$`Arrival Delay in Minutes`, data_cleaned$`Departure Delay in Minutes`, use = "complete.obs")
cor(data_cleaned$`Seat comfort`, data_cleaned$`satisfaction_v2`, use = "complete.obs")

# Convert satisfaction_v2 to numeric
data_cleaned$`satisfaction_numeric` <- as.numeric(factor(data_cleaned$`satisfaction_v2`))
## “neutral or dissatisfied” → 1, “satisfied” → 2

boxplot(data_cleaned$`satisfaction_numeric` ~ data_cleaned$`Seat comfort`, 
        main = "Satisfaction vs Seat Comfort", 
        xlab = "Seat Comfort", ylab = "Satisfaction (Encoded)")

# Boxplot for satisfaction vs other service features
boxplot(data_cleaned$`satisfaction_numeric` ~ data_cleaned$`Inflight wifi service`, 
        main = "Satisfaction vs Inflight Wifi Service", 
        xlab = "Inflight Wifi Service", ylab = "Satisfaction (Encoded)")

boxplot(data_cleaned$`satisfaction_numeric` ~ data_cleaned$`Food and drink`, 
        main = "Satisfaction vs Food and Drink", 
        xlab = "Food and Drink Rating", ylab = "Satisfaction (Encoded)")

boxplot(data_cleaned$`satisfaction_numeric` ~ data_cleaned$`Inflight entertainment`, 
        main = "Satisfaction vs Inflight Entertainment", 
        xlab = "Inflight Entertainment Rating", ylab = "Satisfaction (Encoded)")

# Satisfaction vs Delays
boxplot(data_cleaned$`satisfaction_numeric` ~ data_cleaned$`Departure Delay in Minutes`, 
        main = "Satisfaction vs Departure Delay", 
        xlab = "Departure Delay (Minutes)", ylab = "Satisfaction (Encoded)")

boxplot(data_cleaned$`satisfaction_numeric` ~ data_cleaned$`Arrival Delay in Minutes`, 
        main = "Satisfaction vs Arrival Delay", 
        xlab = "Arrival Delay (Minutes)", ylab = "Satisfaction (Encoded)")

# Satisfaction by Class and Customer Types
# Barplot for Satisfaction vs Travel Class
barplot(table(data_cleaned$`Class`, data_cleaned$`satisfaction_v2`), 
        beside = TRUE, col = c("lightblue", "pink"), 
        legend = rownames(table(data_cleaned$`Class`, data_cleaned$`satisfaction_v2`)), 
        main = "Satisfaction vs Travel Class", xlab = "Class", ylab = "Count")

# Add a trend line
abline(lm(data_cleaned$`satisfaction_numeric` ~ data_cleaned$`Departure Delay in Minutes`), col = "red")

# Barplot for Satisfaction vs Customer Type
barplot(table(data_cleaned$`Customer Type`, data_cleaned$`satisfaction_v2`), 
        beside = TRUE, col = c("lightblue", "pink"), 
        legend = rownames(table(data_cleaned$`Customer Type`, data_cleaned$`satisfaction_v2`)), 
        main = "Satisfaction vs Customer Type", xlab = "Customer Type", ylab = "Count")

# Load ggplot2 for better visualizations
library(ggplot2)

# Group age into categories for a cleaner plot
data_cleaned$Age_Group <- cut(data_cleaned$Age, 
                              breaks = c(0, 18, 30, 45, 60, 100), 
                              labels = c("0-18", "19-30", "31-45", "46-60", "60+"))

# Plot the bar chart
ggplot(data_cleaned, aes(x = Age_Group, fill = `Type of Travel`)) +
  geom_bar(position = "dodge") +
  labs(title = "Travel Type Distribution Across Age Groups", 
       x = "Age Group", 
       y = "Count", 
       fill = "Type of Travel") +
  theme_minimal()

# Filter rows with Age < 18 and Type of Travel == "Business travel"
under_18_business <- data_cleaned[data_cleaned$Age < 18 & data_cleaned$`Type of Travel` == "Business travel", ]
print(under_18_business)

barplot(table(data_cleaned$`Age`, data_cleaned$`satisfaction_v2`), 
        beside = TRUE, col = c("lightblue", "pink"), 
        legend = rownames(table(data_cleaned$`Class`, data_cleaned$`satisfaction_v2`)), 
        main = "Satisfaction vs Age", xlab = "Age", ylab = "Count")

## Findings: People under 18 are travelling for Business. Something we can decide to keep or remove. 
## Decided to remove entries with age < 18

##########################################
# Step5: Feature Engineering: Selecting features on the basis of the findings above. 


#########################################
# Data Sampling
