# Load necessary libraries
library(ggplot2)        # For creating plots
library(dplyr)          # For data manipulation
library(RColorBrewer)   # For color palettes
library(tidyverse)      # For data analysis and visualization
library(caret)          # For machine learning models
library(factoextra)     # For visualizing clustering results
library(cluster)        # For clustering analysis
library(pROC)           # For ROC curve analysis
library(yardstick)      # For model evaluation metrics
library(gplots)          # For additional plotting functions


# Load the dataset
student_evaluation <- read.csv("https://raw.githubusercontent.com/kranthi419/Turkiye-Student-Evaluation-Data-Set/master/Dataset/turkiye_student_ratings.csv")
View(student_evaluation)


# Data Exploration
nrow(student_evaluation)  # Number of rows
ncol(student_evaluation)  # Number of columns
dim(student_evaluation)   # Dimensions (rows x columns)
colnames(student_evaluation) # Display column names of the dataset
head(student_evaluation)  # Display the first few rows of the dataset
tail(student_evaluation)   # Display the last few rows of the dataset
any(is.na(student_evaluation))  # Check for missing values
na.omit(student_evaluation)   #remove na values

#neww columns 
student_evaluation <- student_evaluation %>%
  mutate(
    course_related = rowMeans(select(., 6:17), na.rm = TRUE),
    instructor_related = rowMeans(select(., 18:33), na.rm = TRUE),
    satisfaction_binary = ifelse(Q9 >= 4, 1, 0)
  )
student_evaluation

# Descriptive statistics
summary(student_evaluation)
str(student_evaluation)
summary_table <- student_evaluation %>%
  summarise(across(where(is.numeric), list(mean = mean, median = median, sd = sd)))

# outliers
# Frequency table for 'instr'
table_instr <- table(student_evaluation$instr)
cat("Frequency Table for 'instr':\n", table_instr, "\n")
pairs(student_evaluation[, 6:17], col="maroon", main="Scatterplot Matrix for Variables 6 to 17")
outliers <- boxplot.stats(student_evaluation$course_related)$out
outliers
student_evaluation$course_related <- ifelse(student_evaluation$course_related %in% outliers, NA, student_evaluation$course_related)

# Creating a new feature 'total_related' by summing 'course_related' and 'instructor_related'
student_evaluation$total_related <- student_evaluation$course_related + student_evaluation$instructor_related
cor_matrix <- cor(student_evaluation[, c("course_related", "instructor_related", "satisfaction_binary")])
cor_matrix

# Specify the columns for which you want to find unique values
selected_columns <- c("instr", "class", "nb.repeat", "attendance", "difficulty", "course_related", "instructor_related")
# Use sapply to find and print unique values for each selected column
unique_values <- sapply(student_evaluation[, selected_columns], unique)
unique_values


#visualization 
# Scatter plot for 'difficulty' vs. 'satisfaction_binary'
scatter_plot<-ggplot(student_evaluation, aes(x = difficulty, y = satisfaction_binary)) +
  geom_point() +
  labs(title = "Scatter plot of Difficulty vs. Satisfaction", 
       x = "Difficulty", y = "Satisfaction Binary")
scatter_plot

# Histogram for 'attendance'
histo_plot <- ggplot(student_evaluation, aes(x = attendance)) +
  geom_histogram(binwidth = 1, fill = "navy", color = "yellow") +
  labs(title = "Histogram of Attendance", x = "Attendance", y = "Frequency")
histo_plot

# Box plot for 'course_related' by 'nb.repeat'
box_plot<- ggplot(student_evaluation, aes(x = as.factor(nb.repeat), y = course_related)) +
  geom_boxplot(fill = "orange", color = "black") +
  labs(title = "Box plot of Course Related by Number of Repeats", 
       x = "Number of Repeats", y = "Course Related")
box_plot


# Bar plot for 'instr'
ins_plot<- ggplot(student_evaluation, aes(x = as.factor(instr))) +
  geom_bar(fill = "green", color = "black") +
  labs(title = "Bar plot of Instructors", x = "Instructor ID", y = "Count")
ins_plot

# Bar plot for 'class'
class_bar<- ggplot(student_evaluation, aes(x = as.factor(class))) +
  geom_bar(fill = "purple", color = "black") +
  labs(title = "Bar plot of Classes", x = "Class ID", y = "Count")
class_bar

# Bar plot for 'satisfaction_binary'
satis_bar<- ggplot(student_evaluation, aes(x = as.factor(satisfaction_binary))) +
  geom_bar(fill = "red", color = "black") +
  labs(title = "Bar plot of Satisfaction Binary", 
       x = "Satisfaction Binary", y = "Count")
satis_bar

# Pie chart for 'nb.repeat'
pie_chart<- ggplot(student_evaluation, aes(x = "", fill = as.factor(nb.repeat))) +
  geom_bar(width = 1, stat = "count") +
  coord_polar(theta = "y") +
  labs(title = "Pie Chart of Number of Repeats")
pie_chart

# Explore satisfaction scores based on different instructors
bar_plot_instr_satisfaction <- ggplot(student_evaluation, aes(x = as.factor(instr), fill = as.factor(satisfaction_binary))) +
  geom_bar(position = "dodge") +
  labs(title = "Satisfaction Binary by Instructor", x = "Instructor ID", y = "Count", fill = "Satisfaction Binary")
bar_plot_instr_satisfaction


# Time series plot for attendance
line_bar<- ggplot(student_evaluation, aes(x = as.Date("2000-01-01") + attendance, y = Q9)) +
  geom_line() +
  labs(title = "Time Series Plot of Q9 by Attendance", x = "Date", y = "Q9")
line_bar


#classification
# Select relevant columns for clustering
clustering_data <- student_evaluation %>%
  select(course_related, instructor_related, satisfaction_binary)

# Perform k-means clustering with 5 clusters
k <- 5
kmeans_result <- kmeans(clustering_data, centers = k, nstart = 25)

# Adding of cluster
student_evaluation$cluster <- as.factor(kmeans_result$cluster)

# Visualize the clusters

cluster_plot<- ggplot(student_evaluation, aes(x = course_related, y = instructor_related, color = cluster)) +
  geom_point() +
  labs(title = "K-means Clustering", x = "Course Related", y = "Instructor Related", color = "Cluster")
cluster_plot


#  a scatter plot with ellipses
cluster_ellipse <- ggplot(student_evaluation, aes(x = course_related, y = instructor_related, color = cluster)) +
  geom_point(alpha = 0.2) +
  stat_summary(fun.y = "mean", geom = "point", size = 3, shape = 10) +
  stat_ellipse(aes(fill = cluster), geom = "polygon", alpha = 0.2, show.legend = FALSE) +
  ggtitle("Cluster Diagram: Course Related VS Instructor Related") +
  xlab("Course Related") +
  ylab("Instructor Related") +
  theme_bw()
cluster_ellipse


#  a bar plot for the distribution of clusters
plot_clusters <- ggplot(student_evaluation, aes(x = cluster, fill = cluster)) +
  geom_bar() +
  ggtitle("Distribution of Observations Across Clusters") +
  xlab("Cluster") +
  ylab("Count") +
  theme_bw()
plot_clusters

# a heatmap for the distribution of clusters
heatmap_clusters <- ggplot(student_evaluation, aes(x = 1, y = cluster, fill = as.factor(cluster))) +
  geom_tile() +
  ggtitle("Heatmap of Cluster Distribution") +
  xlab("") +
  ylab("Cluster") +
  scale_fill_discrete(name = "Cluster") +  
heatmap_clusters





# Linear regression model
model <- lm(Q9 ~ course_related + instructor_related, data = student_evaluation)
summary(model)
# Predictions from the linear regression model
predictions_linear <- predict(model, newdata = student_evaluation)
predictions_linear
# Create a data frame for actual vs. predicted values
scatter_data <- data.frame(Actual = student_evaluation$Q9, Predicted = predictions_linear)
scatter_data
# Scatterplot of actual vs. predicted values
scatterplot_linear <- ggplot(scatter_data, aes(x = Actual, y = Predicted)) +
  geom_point(color = "cyan4", alpha = 0.7) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "Actual vs. Predicted (Linear Regression)", x = "Actual", y = "Predicted")
scatterplot_linear

# Residuals vs. Fitted values plot
residuals_plot <- ggplot(model, aes(x = .fitted, y = .resid)) +
  geom_point(color = "darkolivegreen", alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "chocolate") +
  labs(title = "Residuals vs. Fitted Values", x = "Fitted Values", y = "Residuals")
residuals_plot

# Normal Q-Q plot
qq_plot <- ggplot(model, aes(sample = .resid)) +
  stat_qq() +
  stat_qq_line(color = "darkred") +
  labs(title = "Normal Q-Q Plot")
qq_plot

# Scale-Location (Sqrt(|Residuals|) vs. Fitted values) plot
scale_location_plot <- ggplot(model, aes(x = .fitted, y = sqrt(abs(.resid)))) +
  geom_point(color = "darkmagenta", alpha = 0.7) +
  geom_smooth(se = FALSE, color = "orange", linetype = "dashed") +
  labs(title = "Scale-Location Plot", x = "Fitted Values", y = "Sqrt(|Residuals|)")
scale_location_plot


# Residuals vs. Leverage plot
leverage_plot <- ggplot(model, aes(x = .hat, y = .resid)) +
  geom_point(color = "darkviolet", alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(title = "Residuals vs. Leverage", x = "Leverage", y = "Residuals")
leverage_plot


# Logistic regression model
logistic_model <- glm(satisfaction_binary ~ course_related + instructor_related, family = binomial(link = "logit"), data = student_evaluation)
summary(logistic_model)
# Predictions from the logistic regression model
predictions_logistic <- predict(logistic_model, newdata = student_evaluation, type = "response")


# Scatterplot of actual vs. predicted probabilities
scatterplot_logistic <- ggplot(data.frame(Actual = student_evaluation$satisfaction_binary, Predicted = predictions_logistic), aes(x = Actual, y = Predicted)) +
  geom_point(color = "#006666", alpha = 0.7) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "darkred") +
  labs(title = "Actual vs. Predicted Probabilities (Logistic Regression)", x = "Actual", y = "Predicted")
scatterplot_logistic

# Residuals vs. Fitted values plot
residuals_plot_logistic <- ggplot(logistic_model, aes(x = .fitted, y = residuals(logistic_model, type = "deviance"))) +
  geom_point(color = "darkgreen", alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs. Fitted Values", x = "Fitted Values", y = "Residuals")
residuals_plot_logistic

# ROC Curve

roc_curve <- roc(student_evaluation$satisfaction_binary, predictions_logistic)
roc_plot <- ggroc(roc_curve, color = "darkblue", legacy.axes = TRUE) +
  labs(title = "ROC Curve", x = "1 - Specificity", y = "Sensitivity") +
  theme_minimal()
roc_plot

# Probability Density Histogram
density_plot <- ggplot(data.frame(Predicted = predictions_logistic), aes(x = Predicted)) +
  geom_histogram(binwidth = 0.02, fill = "orange", color = "black", alpha = 0.7) +
  labs(title = "Probability Density Histogram", x = "Predicted Probability", y = "Density")
density_plot


# Selecting columns from the original dataset
selected_columns <- c("instr", "class", "nb.repeat", "attendance", "difficulty", "course_related", "instructor_related")
# Creating the final dataset
final_dataset <- cbind(student_evaluation[, selected_columns], reduced_dimensions)
head(final_dataset)
write.csv(final_dataset,"explortory.csv")







