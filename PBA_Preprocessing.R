# Load necessary libraries
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)


# Read the dataset
df <- read_excel("satisfaction.xlsx", sheet = "satisfaction_v2")

# View the first few rows
head(df)

# Summary statistics for categorical data
cat("Distribution of satisfaction levels:\n")
satisfaction_distribution <- df %>%
  group_by(satisfaction_v2) %>%
  summarise(Count = n(), Percentage = round((n() / nrow(df)) * 100, 2))
print(satisfaction_distribution)

cat("\nDistribution of Customer Type:\n")
customer_type_distribution <- df %>%
  group_by(`Customer Type`) %>%
  summarise(Count = n())
print(customer_type_distribution)

cat("\nDistribution of Type of Travel:\n")
travel_type_distribution <- df %>%
  group_by(`Type of Travel`) %>%
  summarise(Count = n())
print(travel_type_distribution)

cat("\nDistribution of Class:\n")
class_distribution <- df %>%
  group_by(Class) %>%
  summarise(Count = n())
print(class_distribution)

# Numerical features statistics
cat("\nStatistics for numerical features:\n")
numerical_summary <- df %>%
  summarise(
    Age_Min = min(Age, na.rm = TRUE),
    Age_Max = max(Age, na.rm = TRUE),
    Age_Mean = round(mean(Age, na.rm = TRUE), 2),
    Flight_Distance_Min = min(`Flight Distance`, na.rm = TRUE),
    Flight_Distance_Max = max(`Flight Distance`, na.rm = TRUE),
    Flight_Distance_Mean = round(mean(`Flight Distance`, na.rm = TRUE), 2)
  )
print(numerical_summary)

# Service ratings (Columns 9 to 22)
cat("\nSummary of service ratings:\n")
service_ratings_summary <- df %>%
  select(`Seat comfort`, `Food and drink`, `Inflight wifi service`, 
         `Inflight entertainment`, `Online support`, 
         `Ease of Online booking`, `On-board service`, 
         `Leg room service`, `Baggage handling`, 
         `Checkin service`, `Cleanliness`, `Online boarding`) %>%
  summarise_all(~ list(min = min(.x, na.rm = TRUE), 
                       max = max(.x, na.rm = TRUE), 
                       mean = round(mean(.x, na.rm = TRUE), 2)))

print(service_ratings_summary)

# Delay metrics
cat("\nDelay metrics:\n")
delay_metrics <- df %>%
  summarise(
    Departure_Delay_Mean = round(mean(`Departure Delay in Minutes`, na.rm = TRUE), 2),
    Arrival_Delay_Mean = round(mean(`Arrival Delay in Minutes`, na.rm = TRUE), 2),
    Departure_Delay_Max = max(`Departure Delay in Minutes`, na.rm = TRUE),
    Arrival_Delay_Max = max(`Arrival Delay in Minutes`, na.rm = TRUE)
  )
print(delay_metrics)

########################## cleaning ###################################

# caluculating the count of Nan values in each column 
df_NA_count <- apply(is.na(df), 2, sum)

df_NA_count


df_cleaned <- df %>%
  filter(!is.na(`Arrival Delay in Minutes`))

# Remove rows where Age is below 18
df_cleaned <- df_cleaned %>%
  filter(Age >= 18)

# Display the first few rows of the cleaned dataset
head(df_cleaned)

# Summary of rows removed
total_rows_removed <- nrow(df) - nrow(df_cleaned)
cat("Total rows removed during cleaning:", total_rows_removed, "\n")

###################### visualizing #####################################




df_cleaned$`Type of Travel` <- as.factor(df_cleaned$`Type of Travel`)
df_cleaned$Class <- as.factor(df_cleaned$Class)
df_cleaned$Gender <- as.factor(df_cleaned$Gender)
df_cleaned$`Customer Type` <- as.factor(df_cleaned$`Customer Type`)

df_cleaned$satisfaction_v2 <- as.factor(df_cleaned$satisfaction_v2)


# Helper function to calculate percentages
add_percentage <- function(data, group_col, fill_col) {
  data %>%
    group_by(!!sym(group_col), !!sym(fill_col)) %>%
    summarise(Count = n(), .groups = "drop") %>%
    group_by(!!sym(group_col)) %>%
    mutate(Percentage = round((Count / sum(Count)) * 100, 1))
}

# Bar plot for Type of Travel
type_of_travel_data <- add_percentage(df_cleaned, "Type of Travel", "satisfaction_v2")
ggplot(type_of_travel_data, aes(x = `Type of Travel`, y = Count, fill = satisfaction_v2)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(Percentage, "%")), 
            position = position_stack(vjust = 0.5), color = "white", size = 5) +
  labs(title = "Distribution of Satisfaction by Type of Travel", 
       x = "Type of Travel", y = "Count", fill = "Satisfaction") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        legend.text = element_text(size = 12) , axis.title.x = element_text(size = 14))



# Bar plot for Class
class_data <- add_percentage(df_cleaned, "Class", "satisfaction_v2")
ggplot(class_data, aes(x = Class, y = Count, fill = satisfaction_v2)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(Percentage, "%")), 
            position = position_stack(vjust = 0.5), color = "white", size = 5) +
  labs(title = "Distribution of Satisfaction by Class", 
       x = "Class", y = "Count", fill = "Satisfaction") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        legend.text = element_text(size = 12), axis.title.x = element_text(size = 14))




# Bar plot for Gender
gender_data <- add_percentage(df_cleaned, "Gender", "satisfaction_v2")
ggplot(gender_data, aes(x = Gender, y = Count, fill = satisfaction_v2)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(Percentage, "%")), 
            position = position_stack(vjust = 0.5), color = "white", size = 5) +
  labs(title = "Distribution of Satisfaction by Gender", 
       x = "Gender", y = "Count", fill = "Satisfaction") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        legend.text = element_text(size = 12), axis.title.x = element_text(size = 14))



# Bar plot for Customer Type
customer_type_data <- add_percentage(df_cleaned, "Customer Type", "satisfaction_v2")
ggplot(customer_type_data, aes(x = `Customer Type`, y = Count, fill = satisfaction_v2)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(Percentage, "%")), 
            position = position_stack(vjust = 0.5), color = "white", size = 5) +
  labs(title = "Distribution of Satisfaction by Customer Type", 
       x = "Customer Type", y = "Count", fill = "Satisfaction") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        legend.text = element_text(size = 12) , axis.title.x = element_text(size = 14))





# Density plot for Age by Satisfaction with more x-axis labels
ggplot(df_cleaned, aes(x = Age, fill = satisfaction_v2)) +
  geom_density(alpha = 0.5) +
  scale_x_continuous(breaks = seq(0, max(df_cleaned$Age, na.rm = TRUE), by = 5)) +
  labs(title = "Age Distribution by Satisfaction", 
       x = "Age", 
       y = "Density", 
       fill = "Satisfaction") +
  theme_minimal()



# Bar Chart for sevice rating columns averages 


# Select only survey columns
survey_columns <- df_cleaned %>%
  select(`Seat comfort`, `Food and drink`, `Inflight wifi service`, 
         `Inflight entertainment`, `Online support`, 
         `Ease of Online booking`, `On-board service`, 
         `Leg room service`, `Baggage handling`, 
         `Checkin service`, `Cleanliness`, `Online boarding`)

# Calculate the average rating for each survey column
avg_ratings <- survey_columns %>%
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "Survey_Aspect", values_to = "Average_Rating")

# Plot the average ratings with values on the bars
ggplot(avg_ratings, aes(x = Survey_Aspect, y = Average_Rating)) +
  geom_bar(stat = "identity", fill = "steelblue") + # Use a single color for bars
  geom_text(aes(label = round(Average_Rating, 2)), vjust = -0.5, size = 5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.4))) + # Adds 10% space above the tallest bar
  labs(title = "Average Ratings of Services", 
       x = "Service Aspect", 
       y = "Average Rating (out of 5)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))




# Heatmap for correlation analysis

# Select numerical columns for correlation analysis
numerical_columns <- df_cleaned %>%
  select(Age, `Flight Distance`, 
         `Seat comfort`, `Departure/Arrival time convenient`, `Food and drink`, 
         `Gate location`, `Inflight wifi service`, `Inflight entertainment`, 
         `Online support`, `Ease of Online booking`, `On-board service`, 
         `Leg room service`, `Baggage handling`, `Checkin service`, 
         `Cleanliness`, `Online boarding`, `Departure Delay in Minutes`, 
         `Arrival Delay in Minutes`)

# Calculate the correlation matrix
correlation_matrix <- cor(numerical_columns, use = "complete.obs", method = "pearson")

# Melt the correlation matrix into a long format for ggplot2
melted_cor_matrix <- melt(correlation_matrix)

# Create the heatmap
ggplot(melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") + # Adds white borders to the tiles
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) + # Color gradient
  geom_text(aes(label = round(value, 2)), color = "black", size = 3) + # Adds correlation values
  labs(title = "Correlation Heatmap of Numerical Features", x = "", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.title = element_text(size = 14, hjust = 0.5))



# Rename the column from satisfaction_v2 to passenger_satisfaction
df_cleaned <- df_cleaned %>%
  rename(passenger_satisfaction = satisfaction_v2)


# Save the updated dataset to a CSV file
write.csv(df_cleaned, "CleanedAirlinePBA.csv", row.names = FALSE)


