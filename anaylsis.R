# Load necessary libraries
library(ggplot2)    # For creating plots
library(dplyr)      # For data manipulation
library(caret)      # For model building and evaluation
library(cluster)    # For k-means clustering
library(glmnet)     # For logistic regression models
library(randomForest)  # For random forest models
library(tidyverse)  # A collection of packages for data manipulation and visualization
library(caTools)    # For data splitting and shuffling
library(gridExtra)  # For arranging multiple plots
library(pROC)       # For ROC curve analysis
library(ROCR)       # For ROC curve visualization
library(e1071)      # For support vector machine (SVM) modeling
library(reshape2)   # For data reshaping tasks

# Load datset
bank_data <- bank
bank

# Data exploration
dim(bank_data)         # Dimensions of the dataset
nrow(bank_data)        # Number of rows
ncol(bank_data)        # Number of columns
colnames(bank_data)    # Display column names of the dataset
head(bank_data)        # Display first few rows of the dataset
tail(bank_data)        # Display last few rows of the dataset
sum(is.na(bank_data))  # Handle missing values 
na.omit(bank_data)     # Remove NA values

#Modification of columns 
bank_data <- bank_data %>%
  mutate(loan = ifelse(loan == 'yes', 1, 0))%>%
  mutate(y= ifelse(y=='yes',1,0))%>%
  select(-c(duration))%>%
  arrange(age)
View(bank_data)

# Descriptive Data
summary(bank_data)  # Summary statistics for each variable
str(bank_data)      # Structure of the dataset


#outliers
#frequency table for categorixcal data
table(bank_data$job)
table(bank_data$marital)
table(bank_data$education)
table(bank_data$contact)

#boxplot for numeric data
boxplot(bank_data$age)
boxplot(bank_data$balance)

# Pairwise scatterplots
plot.new()  # Open a new graphics device
pairs(bank_data[, sapply(bank_data, is.numeric)])

# Correlation analysis
cor(bank_data[, sapply(bank_data, is.numeric)])

#outliers based on  chosen criteria
z_scores <- scale(bank_data[, sapply(bank_data, is.numeric)])
outliers <- bank_data[abs(z_scores) > 3, ] 
na.omit(outliers)
dim(outliers)


#visualization
box_visual<- ggplot(bank_data, aes(x = factor(y), y = balance, fill = as.factor(y))) +
  geom_boxplot() +
  labs(title = "Balance Distribution by Subscription",
       x = "Subscription Status",
       y = "Balance") +
  scale_x_discrete(labels = c("No", "Yes")) + 
  theme_minimal()
box_visual

# Visualizing Numeric Variables
status_plot<-ggplot(bank_data, aes(x = age, y = balance)) +
  geom_point(aes(color = default)) +
  labs(title = "Scatter Plot of Age vs Balance by Subscription Status")
status_plot


#histogram of balance
hist_plott<-ggplot(bank_data, aes(x = balance)) +
  geom_histogram(binwidth = 5000, fill = "skyblue", color = "coral4", alpha = 10) +
  labs(title = "Histogram of Balance")
hist_plott

# Boxplot of Age by Marital Status
age_plot<- ggplot(bank_data, aes(x = marital, y = age, fill =as.factor(loan))) +
  geom_boxplot() +
  labs(title = "Boxplot of Age by Marital Status and Subscription Status")
age_plot

# Visualizing Campaign Outcomes
compain_plot<- ggplot(bank_data, aes(x = poutcome, fill = marital)) +
  geom_bar(position = "dodge") +
  labs(title = "Bar Plot of Previous Campaign Outcomes by Subscription Status")
compain_plot

# Subset the data with numerical variables
linear_data <- bank_data[, c("age", "day", "campaign", "pdays", "previous", "balance")]
linear_model <- lm(balance ~ ., data = linear_data)
summary(linear_model)


#kmeans algo
# Perform k-means clustering
kmeans_model <- kmeans(bank_data[, c("age", "balance", "campaign", "pdays", "previous")], centers = 3)

# Scatter plot of cluster centers
centers <- as.data.frame(kmeans_model$centers)
centers$cluster <- row.names(centers)
centers_long <- tidyr::pivot_longer(centers, cols = -cluster)

scatterplot_centers <- ggplot(centers_long, aes(x = name, y = value, color = cluster)) +
  geom_point(size = 3) +
  labs(title = "Scatter Plot of Cluster Centers", x = "Feature", y = "Value") +
  theme_minimal()

# Add cluster assignments to the dataset
bank_data$cluster <- as.factor(kmeans_model$cluster)

# Scatter plot of data points with cluster assignments
scatterplot_cluster_assignments <- ggplot(bank_data, aes(x = balance, y = age, color = cluster)) +
  geom_point() +
  labs(title = "Scatter Plot with Cluster Assignments", x = "Balance", y = "Age") +
  theme_minimal()

# Calculate silhouette scores
silhouette <- silhouette(kmeans_model$cluster, dist(bank_data[, c("age", "balance", "campaign", "pdays", "previous")]))

# Silhouette plot
silhouette_plot <- plot(silhouette)

# Combine plots
grid.arrange(scatterplot_centers, scatterplot_cluster_assignments, silhouette_plot, ncol = 2, nrow = 2)



# Model Building - Classification
set.seed(123)
train_indices <- createDataPartition(bank_data$y, p = 0.8, list = FALSE)
bank_data$y <- as.factor(bank_data$y)
train_data <- bank_data[train_indices, ]
train_data$y <- as.factor(train_data$y)
test_data <- bank_data[-train_indices, ]
test_data$y <- as.factor(test_data$y)

# Build a logistic regression model
#model 1
model_logistic <- glm(y ~ ., data = train_data, family = "binomial")
summary(model_logistic)

# Evaluate the logistic regression model on the test set
predictions_logistic <- predict(model_logistic, newdata = test_data, type = "response")
predictions_logistic <- as.factor(ifelse(predictions_logistic > 0.5, "yes", "no"))
levels(predictions_logistic) <- levels(test_data$y)
predictions_logistic

# Assess model performance
conf_matrix_logistic <- confusionMatrix(as.factor(predictions_logistic), test_data$y)
conf_matrix_logistic

# Calculate metrics for logistic regression
calculate_metrics <- function(conf_matrix) {
  accuracy <- sum(diag(conf_matrix))/sum(conf_matrix)
  precision <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
  recall <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
  f1_score <- 2 * (precision * recall) / (precision + recall)
  return(c(Accuracy = accuracy, Precision = precision, Recall = recall, F1_Score = f1_score))
}

metrics_logistic <- calculate_metrics(as.matrix(conf_matrix_logistic))
metrics_logistic

# Build a Random Forest model
#model 2
model_rf <- randomForest(y ~ ., data = train_data, ntree = 500)
summary(model_rf)
# Plot feature importance
varImpPlot(model_rf)
#predictions
predictions_rf <- predict(model_rf, newdata = test_data)
predictions_rf <- as.factor(predictions_rf)
levels(predictions_rf) <- levels(test_data$y)
predictions_rf

# Assess Random Forest model performance
conf_matrix_rf <- confusionMatrix(predictions_rf, test_data$y)
conf_matrix_rf
metrics_rf <- calculate_metrics(as.matrix(conf_matrix_rf))
metrics_rf


# Support Vector Machines (SVM)
#model 3
model_svm <- svm(y ~ ., data = train_data, kernel = "radial")
summary(model_svm)
predictions_svm <- predict(model_svm, newdata = test_data)
predictions_svm
conf_matrix_svm <- confusionMatrix(predictions_svm, test_data$y)
conf_matrix_svm

# Calculate metrics for SVM
metrics_svm <- calculate_metrics(as.matrix(confusionMatrix(predictions_svm, test_data$y)))
metrics_svm

# Organize comparison metrics into a dataframe
comparison_metrics <- data.frame(
  Model = c("SVM", "Logistic Regression", "Random Forest"),
  Accuracy = c(metrics_svm["Accuracy"], metrics_logistic["Accuracy"], metrics_rf["Accuracy"]),
  Precision = c(metrics_svm["Precision"], metrics_logistic["Precision"], metrics_rf["Precision"]),
  Recall = c(metrics_svm["Recall"], metrics_logistic["Recall"], metrics_rf["Recall"]),
  F1_Score = c(metrics_svm["F1_Score"], metrics_logistic["F1_Score"], metrics_rf["F1_Score"])
)
comparison_metrics

#visualization for matrices
par(mfrow = c(1, 2))

# Confusion Matrix - Logistic Regression
conf_matrix_logistic_vis <- as.table(conf_matrix_logistic)
rownames(conf_matrix_logistic_vis) <- colnames(conf_matrix_logistic_vis) <- c("No", "Yes")
image(
  conf_matrix_logistic_vis, 
  main = "Confusion Matrix - Logistic Regression", 
  col = c("yellow", "deepskyblue3", "darkviolet"),
  axes = FALSE
)
axis(1, at = 1:2, labels = colnames(conf_matrix_logistic_vis), las = 2)
axis(2, at = 1:2, labels = rownames(conf_matrix_logistic_vis), las = 2)
text(expand.grid(1:2, 1:2), labels = conf_matrix_logistic_vis, cex = 1.5, pos = 4)


# Confusion Matrix - Random Forest
conf_matrix_rf_vis <- as.table(conf_matrix_rf)
rownames(conf_matrix_rf_vis) <- colnames(conf_matrix_rf_vis) <- c("No", "Yes")
image(
  conf_matrix_rf_vis, 
  main = "Confusion Matrix - Random Forest", 
  col = c("deepskyblue2", "deeppink3", "darkslategray"),
  axes = FALSE
)
axis(1, at = 1:2, labels = colnames(conf_matrix_rf_vis), las = 2)
axis(2, at = 1:2, labels = rownames(conf_matrix_rf_vis), las = 2)
text(expand.grid(1:2, 1:2), labels = conf_matrix_rf_vis, cex = 1.5, pos = 4)


# Confusion Matrix - SVM
conf_matrix_svm_vis <- as.table(conf_matrix_svm)
rownames(conf_matrix_svm_vis) <- colnames(conf_matrix_svm_vis) <- c("No", "Yes")
image(
  conf_matrix_svm_vis, 
  main = "Confusion Matrix - SVM", 
  col = c("darkred", "gold", "pink"),
  axes = FALSE
)
axis(1, at = 1:2, labels = colnames(conf_matrix_svm_vis), las = 2)
axis(2, at = 1:2, labels = rownames(conf_matrix_svm_vis), las = 2)
text(expand.grid(1:2, 1:2), labels = conf_matrix_svm_vis, cex = 1.5, pos = 4)

#Model representations 
comparison_metrics_long <- tidyr::pivot_longer(comparison_metrics, cols = -Model)
# Bar plot for Accuracy, Precision, Recall, and F1 Score
ggplot(comparison_metrics_long, aes(x = Model, y = value, fill = name)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Model Performance Comparison",
       x = "Model", y = "Value") +
  theme_minimal() +
  facet_wrap(~name, scales = "free_y")

# ROC Curve for Logistic Regression
roc_curve_logistic <- roc(test_data$y, as.numeric(predictions_logistic))
plot(roc_curve_logistic, col = "navy", main = "ROC Curve - Logistic Regression")

# ROC Curve for Random Forest
roc_curve_rf <- roc(test_data$y, as.numeric(predictions_rf))
plot(roc_curve_rf, col = "orange", add = TRUE)

# ROC Curve for SVM
roc_curve_svm <- roc(test_data$y, as.numeric(predictions_svm))
plot(roc_curve_svm, col = "darkred", add = TRUE)
legend("bottomright", legend = c("Logistic Regression", "Random Forest", "SVM"),
       col = c("navy", "orange", "darkred"), lty = 1)

# Calculate AUC for Logistic Regression
auc_logistic <- auc(roc_curve_logistic)
cat("AUC for Logistic Regression Model:", auc_logistic, "\n")

# Calculate AUC for Random Forest
auc_rf <- auc(roc_curve_rf)
cat("AUC for Random Forest Model:", auc_rf, "\n")

# Calculate AUC for SVM
auc_svm <- auc(roc_curve_svm)
cat("AUC for SVM Model:", auc_svm, "\n")
# Create a dataframe with model performance metrics and AUC scores
analysis_df <- data.frame(
  Model = c("Logistic Regression", "Random Forest", "SVM"),
  Accuracy = comparison_metrics$Accuracy,
  Precision = comparison_metrics$Precision,
  Recall = comparison_metrics$Recall,
  F1_Score = comparison_metrics$F1_Score,
  AUC = c(auc_logistic, auc_rf, auc_svm)
)

# Write the dataframe to a CSV file
write.csv(analysis_df, "analysis.csv", row.names = FALSE)

