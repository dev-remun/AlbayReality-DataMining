# =====================================================================
# User Segmentation Analysis (K-Means Clustering)
# Goal: Identify distinct user personas based on engagement behavior
# =====================================================================

# 1. Load required libraries
if (!require("cluster")) install.packages("cluster")
if (!require("ggplot2")) install.packages("ggplot2")
library(cluster)
library(ggplot2)

# 2. Load the raw data
user_features_data <- read.csv("features_raw.csv")

# 3. Prepare the numeric data for clustering
# We focus on engagement metrics: number of attempts and time spent
clustering_input_data <- user_features_data[, c("attempt_count", "total_time")]

# 4. Standardize the data (Scaling)
# This is critical because time is in minutes (high) and attempts are counts (low).
# Scaling ensures they are treated with equal importance.
scaled_user_metrics <- scale(clustering_input_data)

# 5. THE ELBOW METHOD: Determining the optimal number of groups (K)
# We calculate the 'Total Within-Cluster Sum of Squares' for K = 1 to 10
within_cluster_variance <- sapply(1:10, function(k) {
  kmeans(scaled_user_metrics, centers = k, nstart = 20)$tot.withinss
})

# Plot the Elbow Curve
plot(1:10, within_cluster_variance, type = "b", pch = 19, frame = FALSE, 
     main = "Elbow Method: Finding Optimal Number of Clusters",
     xlab = "Number of Clusters (K)", ylab = "Total Variance Within Groups")

# 6. Perform the Final Clustering (K = 3)
set.seed(123) # Ensure consistent results
final_kmeans_model <- kmeans(scaled_user_metrics, centers = 3, nstart = 25)

# 7. Add cluster labels back to our dataset
user_features_data$user_persona_group <- as.factor(final_kmeans_model$cluster)

# 8. RESULTS: Characterizing the 3 User Personas
persona_summary <- aggregate(
  clustering_input_data, 
  by = list(Group = user_features_data$user_persona_group), 
  mean
)
cat("\n=== User Persona Characteristics ===\n")
print(persona_summary)

# 9. MEASUREMENT: Silhouette Analysis
# Evaluates how well each user fits into their assigned group
silhouette_results <- silhouette(final_kmeans_model$cluster, dist(scaled_user_metrics))
average_silhouette_score <- mean(silhouette_results[, 3])

cat("\n=== Model Measurement ===\n")
cat("Overall Silhouette Score: ", round(average_silhouette_score, 3), "\n")
cat("Interpretation: Scores > 0.50 indicate strong, well-defined clusters.\n")

# 10. Visualization of User Personas
ggplot(user_features_data, aes(x = total_time, y = attempt_count, color = user_persona_group)) +
  geom_point(size = 4, alpha = 0.7) +
  theme_minimal() +
  labs(title = "User Segmentation Results",
       subtitle = paste("Silhouette Score:", round(average_silhouette_score, 3)),
       x = "Total Time Spent (Minutes)", 
       y = "Number of Quizzes (Attempts)",
       color = "Persona Group")