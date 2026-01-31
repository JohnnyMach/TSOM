# Loading libraries
library(tidyverse)
library(ggplot2)
library(caret)
library(cluster)
library(factoextra)
library(reshape2)
library(readr)
library("dplyr")


# 2. Import Data set using read csv
iced_coffee <- read.csv("C:\\Users\\johnn\\OneDrive\\Documentos\\TSOM\\capstone project\iced_coffee_data-1.csv")

# Ensure it's a data frame
iced_coffee <- as.data.frame(iced_coffee)

# 3. Data Sanity Checks
# Check structure and summary
str(iced_coffee)
summary(iced_coffee)

# Check for duplicates
iced_coffee <- iced_coffee[!duplicated(iced_coffee), ]

# Check missing values
colSums(is.na(iced_coffee))

# 4. Variable Transformation
# Convert categorical variables to factors
iced_coffee <- iced_coffee %>%
  mutate(across(c(coffee_type, milk_type, flavor_additions, packaging_type,
                  serving_size, texture_level, buy_again), as.factor))


# 5. Descriptive Analysis
# Q1: Satisfaction distribution by coffee type
ggplot(iced_coffee, aes(x = coffee_type, y = satisfaction)) +
  geom_boxplot() +
  theme_minimal() +
  ggtitle("Satisfaction by Coffee Type")

# Q2: Average satisfaction by milk type
iced_coffee %>%
  group_by(milk_type) %>%
  summarise(Avg_Satisfaction = mean(satisfaction, na.rm = TRUE))
# How many missing values are present per milk type 
iced_coffee %>%
  group_by(milk_type) %>%
  summarise(Missing_Satisfaction = sum(is.na(satisfaction)))

# Q3: Sugar % vs. Satisfaction
ggplot(iced_coffee, aes(x = sugar_pct, y = satisfaction)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm") +
  ggtitle("Sugar Percentage vs Satisfaction")

# Q4: Packaging vs Satisfaction
ggplot(iced_coffee, aes(x = packaging_type, y = satisfaction)) +
  geom_boxplot(fill = "lightgreen") +
  theme_minimal()

# Q5: Satisfaction by Flavor Addition
iced_coffee %>%
  group_by(flavor_additions) %>%
  summarise(Avg_Satisfaction = mean(satisfaction, na.rm = TRUE)) %>%
  arrange(desc(Avg_Satisfaction))


# 6. Predictive Analysis (Regression Models)
# Model 1: Predicting satisfaction
model1 <- lm(satisfaction ~ sugar_pct + bitterness_level + caffeine_strength + aftertaste + aroma_strength, data = iced_coffee)
summary(model1)

# Model 2: Predicting churn_rate
model2 <- lm(churn_rate ~ satisfaction + price + discount_pct + packaging_type, data = iced_coffee)
summary(model2)

# Model 3: Predicting price
model3 <- lm(price ~ coffee_type + packaging_type + serving_size + caffeine_strength + flavor_additions, data = iced_coffee)
summary(model3)

# 7. Clustering Analysis
# Keep the cleaned dataset before scaling
iced_coffee_cluster <- iced_coffee %>%
  select(record_id, sugar_pct, bitterness_level, aroma_strength, aftertaste, satisfaction, churn_rate) %>%
  drop_na()

# Scale it
cluster_data <- iced_coffee_cluster %>%
  select(-record_id) %>%
  scale()

# Determine optimal clusters
fviz_nbclust(cluster_data, kmeans, method = "wss")

# Apply K-means with 3 clusters
set.seed(123)
kmeans_result <- kmeans(cluster_data, centers = 3, nstart = 25)

# Add cluster labels to the cleaned dataset
iced_coffee_cluster$cluster <- as.factor(kmeans_result$cluster)

# Visualize clusters
fviz_cluster(kmeans_result, data = cluster_data)

# Cluster summaries using the cleaned dataset with cluster labels
iced_coffee_cluster %>%
  group_by(cluster) %>%
  summarise(across(c(satisfaction, churn_rate, sugar_pct, bitterness_level), mean, na.rm = TRUE))

# 8. Save Processed Dataset

write.csv(iced_coffee, "processed_iced_coffee.csv", row.names = FALSE)

