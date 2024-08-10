#libraries
library(ggplot2)
library(dplyr)
library(ggthemes)
library(RColorBrewer)
library(cluster)
library(DT)
library(lubridate)
library(reshape2)
library(tidyverse)
library(caTools)
library(ROCR)
library(pROC)
library(ranger)
library(caret)
library(data.table)
library(caret)
library(e1071)



#read 
football<- read.csv("C:/Users/PMLS/Desktop/LaLiga.csv")
View(football)

#inspection of dataset
nrow(football)
ncol(football)
dim(football)
colnames(football)
head(football,3)
tail(football,4)
summary(football)
View(summary(football))
str(football)

# Check data for cleaning
is.na(football)
sum(is.na(football))
#clean data
football_1 <- na.omit(football)
nrow(football_1)
dim(football_1)
summary(football_1)
View(football_1)

#outliers
plot1 <- boxplot(football_1[, c("possession", "total.Shots.Taken")])
plot1
plot2 <- boxplot(football_1$attendance, 
                 main = "Boxplot of Attendance", 
                 xlab = "Category", 
                 ylab = "Attendance")
plot2

#dplyr functions
#data wrangling

#select
selected_data <- football_1%>%
  select(date, comp, opponent,Goals.for,Goals.against)%>%
  filter(comp == "Champions Lg" & Goals.for> 2)
selected_data

# mutate
football_efficiency <- football_1 %>%
  mutate(shooting_pct = shots.on.target / total.Shots.Taken * 100,
         goal_diff = as.numeric(goals.scored) - as.numeric(Goals.against)) %>%
  filter(comp == "Champions Lg" & !is.na(goal_diff)) %>%
  select(comp,team,Goals.for,shots.on.target,total.Shots.Taken,Goals.against,goal_diff,shooting_pct)%>%
  arrange(desc(shooting_pct))
football_efficiency
View(football_efficiency)

# Summarize data by competition
# Average goals scored per team
team_goals <- football_1 %>%
  group_by(team) %>%
  summarize(avg_goals = mean(goals.scored))
team_goals

# Average possession by competition
comp_possession <- football_1 %>%
  group_by(comp) %>%
  summarize(avg_possession = mean(possession))
comp_possession


#corelation
cor(football_1$possession, football_1$total.Shots.Taken)
cor(football_1$attendance, football_1$goals.scored)

#visualization 
#plots 
#facets
plot_one<-ggplot(football_1, aes(x = total.Shots.Taken, y = goals.scored, color = team)) +
  geom_point(size= 1) +
  facet_wrap(~ comp) +
  labs(title = "Shots vs. Goals with Expected Goals Facets",
       x = "Total Shots Taken",
       y = "Goals Scored",
       color = "Team") +
  scale_y_continuous(limits = c(0, 5))
plot_one


#facet
plot_two  <- ggplot(football_1, aes(x = time, y = possession, color = as.factor(penalty.kicks))) +
  geom_point()+
  facet_wrap(~ team) +
  labs(title = "Cumulative Possession by Team with Goal Markers",
       x = "Time",
       y = "Possession (%)") +
  theme_bw() 

plot_two


#high scored rate
high_scored <- ggplot(football_1, aes(x = round, y = Goals.against, color = as.factor(possession))) +
  geom_point() +
  labs(title = "Goals Against vs Rounds Played by Competition",
       x = "Round",
       y = "Goals Against",
       color = "Possession") 
high_scored


#boxplot
plot_two<- ggplot(football_1, aes(x =(Expected.goals.against*10), y = Goals.against)) +
  geom_boxplot(fill = "orange", color = "black") +
  labs(title = "Boxplot of Goals Against (conceded by the team)",
       x = "Expected Againste goal",
       y = "Against goal")
plot_two

#histogram
histogram_plot <- ggplot(football_1, aes(x = possession)) +
  geom_histogram(binwidth = 5, fill = "lightblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Possession",
       x = "Possession (%)",
       y = "Frequency") +
  theme_minimal()

histogram_plot
# Linear Regression
# Linear Regression Model 1: 
#possession
#apply regression manually
newdata<-football_1
newdata
x<-newdata$possession
x
y<-newdata$shot.goals
y
sumX<-sum(x)
sumX
sumY<-sum(y)
sumY
N<-nrow(newdata)     
N
Xbar<-sumX/N
Xbar
Ybar<-sumY/N
Ybar
xy<- x * y
xy
sumXY<-sum(xy)
sumXY
BXY<-((N*sumXY)-(sumX*sumY))/((N*sumY^2)-(sumY)^2)
BXY
BYX<-((N*sumXY)-(sumX*sumY))/((N*sumX^2)-(sumX)^2)
BYX
xreg <-BXY *(y -  Ybar) +  Xbar
xreg
yreg <-BYX * (x - Xbar) + Ybar
yreg
new_data<-newdata%>%
  mutate(X = x,
         Y = y,
         SumX = sumX,
         SumY = sumY,
         No_of_obs = N,
         bxy = BXY,
         byx = BYX,
         Xreg = xreg,
         Yreg =yreg)
new_data
View(new_data)

#linear by formula
linear_model_pos <- lm(shot.goals ~ possession, data = football_1)
linear_model_pos
summary(linear_model_pos)

linear_model_po <- lm(yreg~xreg,data= new_data)
linear_model_po
summary(linear_model_po)
#visualization of data afte linear regression

# Scatter plot with linear regression lines
plot_poss<- ggplot(new_data, aes(x = X, y = Y)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "deeppink") +
  labs(title = "Scatter Plot with Linear Regression Lines",
       x = "Possession",
       y = "Shot Goals") +
  theme_minimal()
plot_poss

box_poss<- ggplot(new_data, aes(x = possession, y = Xreg)) +
  geom_boxplot(color="blueviolet") +
  labs(title = "Boxplot of X Regression Values",
       x = "Possession",
       y = "X Regression Values") +
  theme_minimal()
box_poss
# Bar plot
bar_poss<-ggplot(new_data, aes(x = possession, y = Yreg)) +
  geom_bar(stat = "identity", fill = "navy") +
  labs(title = "Bar Plot of Y Regression Values",
       x = "Possession",
       y = "Y Regression Values") +
  theme_minimal()
bar_poss

# Linear Regression Model 2: 
#goals.scored
goal_data<-football_1
x<-goal_data$goals.scored
x
y<-goal_data$shot.goals
y
sumX<-sum(x)
sumX
sumY<-sum(y)
sumY
N<-nrow(goal_data)     
N
Xbar<-sumX/N
Xbar
Ybar<-sumY/N
Ybar
xy<- x * y
xy
sumXY<-sum(xy)
sumXY
BXY<-((N*sumXY)-(sumX*sumY))/((N*sumY^2)-(sumY)^2)
BXY
BYX<-((N*sumXY)-(sumX*sumY))/((N*sumX^2)-(sumX)^2)
BYX
xreg <-BXY *(y -  Ybar) +  Xbar
xreg
yreg <-BYX * (x - Xbar) + Ybar
yreg
goal_data<-goal_data%>%
  mutate(X = x,
         Y = y,
         SumX = sumX,
         SumY = sumY,
         No_of_obs = N,
         bxy = BXY,
         byx = BYX,
         Xreg = xreg,
         Yreg =yreg)
goal_data
View(goal_data)
#linear regression without library
linear_model_goals <- lm(shot.goals ~ goals.scored, data = football_1)
summary(linear_model_goals)
linear_reg <-lm(yreg~xreg,data=goal_data)
summary(linear_reg)

#visualization
goal_plot<- ggplot(goal_data, aes(x = X, y = Y)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "deeppink") +
  labs(title = "Scatter Plot with Linear Regression Lines",
       x = "Goal Scored",
       y = "Shot Goals") +
  theme_minimal()
goal_plot
goal_box <- ggplot(goal_data, aes(x = goals.scored, y = Xreg)) +
  geom_boxplot(color= "navy",fill = "grey") +
  labs(title = "Boxplot of X Regression Values",
       x = "Goal Scored",
       y = "X Regression Values") +
  theme_minimal()
goal_box

# Bar plot
goal_bar <- ggplot(goal_data, aes(x = goals.scored, y = Yreg)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  labs(title = "Bar Plot of Y Regression Values",
       x = "Goal scored",
       y = "Y Regression Values") +
  theme_minimal()
goal_bar



# Linear Regression Model 3:
#distance
#manually
dis_data<-football_1
dis_data
summary(dis_data)
x<-dis_data$distance
x
y<-dis_data$shot.goals
y
sumX<-sum(x)
sumX
sumY<-sum(y)
sumY
N<-nrow(dis_data)     
N
Xbar<-mean(x)
Xbar
Ybar<-mean(y)
Ybar
xy<- x * y
xy
sumXY<-sum(xy)
sumXY
BXY<-((N*sumXY)-(sumX*sumY))/((N*sumY^2)-(sumY)^2)
BXY
BYX<-((N*sumXY)-(sumX*sumY))/((N*sumX^2)-(sumX)^2)
BYX
xreg <-BXY *(y -  Ybar) +  Xbar
xreg<-na.omit(xreg)
xreg
summary(xreg)
yreg <-BYX * (x - Xbar) + Ybar
yreg<-na.omit(yreg)
yreg
summary(yreg)
dis_data<-dis_data%>%
  mutate(X = x,
         Y = y,
         SumX = sumX,
         SumY = sumY,
         No_of_obs = N,
         bxy = BXY,
         byx = BYX,
         Xreg = xreg,
         Yreg =yreg)
dis_data
View(dis_data)
#reg by formula
linear_model_distance <- lm(shot.goals ~ distance, data = football_1)
summary(linear_model_distance)

#visualization
# Check the structure of dis_data
str(dis_data)

dis_data <- dis_data %>%
  mutate(X = distance,
         Y = shot.goals)
dis_plot <- ggplot(dis_data, aes(x = X, y = Y)) +
  geom_point(color = "cadetblue") +
  geom_smooth(method = "lm", se = FALSE, color = "blueviolet") +
  labs(title = "Scatter Plot with Linear Regression Lines",
       x = "Distance",
       y = "Shot Goals") +
  theme_minimal()
dis_plot

dis_box <- ggplot(dis_data, aes(x = distance, y = Xreg)) +
  geom_boxplot(color= "navy",fill = "darkgoldenrod") +
  labs(title = "Boxplot of X Regression Values",
       x = "Distance",
       y = "X Regression Values") +
  theme_minimal()
dis_box

# Bar plot
dis_bar <- ggplot(dis_data, aes(x = distance, y = Yreg)) +
  geom_bar(stat = "identity", fill = "chocolate") +
  labs(title = "Bar Plot of Y Regression Values",
       x = "Distance",
       y = "Y Regression Values") +
  theme_minimal()
dis_bar


#reproducibility
set.seed(123)
subset_data <- football_1[sample(nrow(football_1), 500), ]
# Perform K-means clustering on the variables ('Goals.for' and 'possession')
kmean <- kmeans(subset_data[, c("Goals.for", "possession")], centers = 5)
subset_data$Cluster <- as.factor(kmean$cluster)
cluster_center <- kmean$centers
plut_one <- ggplot(subset_data, aes(x = Goals.for, y = possession)) +
  geom_point() +
  geom_smooth()

plut_one  


subset_data$Goals.for <- as.numeric(subset_data$Goals.for)
subset_data$possession <- as.numeric(subset_data$possession)

# Create the scatter plot with a linear regression line
poss_goal_plot <- ggplot(subset_data, aes(x = Goals.for, y = possession, color = Cluster)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "deepskyblue", size = 1) +
  ggtitle("Scatter Plot: Goals.for vs Possession") +
  xlab("Goals.for") +
  ylab("Possession") +
  theme_bw()

poss_goal_plot

# Cluster Diagram
library(cluster)
cluster_plot <- ggplot(subset_data, aes(x = Goals.for, y = possession, fill = Cluster)) +
  geom_point(alpha = 0.2) +
  stat_summary(fun.y = "mean", geom = "point", size = 3, shape = 10) +
  stat_ellipse(aes(fill = Cluster), geom = "polygon", alpha = 0.2, show.legend = FALSE) +
  ggtitle("Cluster Diagram: Goals.for VS Possession") +
  xlab("Goals.for") +
  ylab("Possession") +
  theme_bw()

cluster_plot


# Bar Plot
cluster_counts <- subset_data %>%
  count(Cluster)

goal_poss_bar <- ggplot(cluster_counts, aes(x = Cluster, y = n, fill = Cluster)) +
  geom_bar(stat = "identity", alpha = 0.6) +
  ggtitle("Bar Plot") +
  xlab("Cluster") +
  ylab("Count") +
  theme_bw()
goal_poss_bar

# Box Plot
goal_poss_box <- ggplot(subset_data, aes(x = Cluster, y = Goals.for, fill = Cluster)) +
  geom_boxplot(alpha = 0.6) +
  ggtitle("Box Plot: Goals.for") +
  xlab("Cluster") +
  ylab("Goals.for") +
  theme_bw()
goal_poss_box

#heatmap
subset_data <- football_1[, c("Goals.for", "possession")]
subset_data$Goals.for <- as.numeric(subset_data$Goals.for)
subset_data$possession <- as.numeric(subset_data$possession)
str(subset_data)

# Calculate the correlation matrix
correlation_matrix <- cor(subset_data)
# Melt the correlation matrix
melted_correlation <- melt(correlation_matrix)

# Create the heatmap plot
heatmap_plot <- ggplot(melted_correlation, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_viridis(name = "Correlation", option = "C") +  # Using viridis color palette#library
  theme_minimal() +
  labs(title = "Correlation Heatmap: Goals.for vs Possession",
       x = "Variables",
       y = "Variables") +
  theme(panel.background = element_rect(fill = "darkred"),
        plot.background = element_rect(fill = "darkkhaki"),
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "cyan4"),
        legend.title = element_text(color = "darkorange"),
        legend.text = element_text(color = "darkgoldenrod4"))
heatmap_plot
#prediction1
# Sample split for Linear Model 1: possession
data_sample_pos <- sample.split(football_1$shot.goals, SplitRatio = 0.80)
data_sample_pos

train_data_pos <- subset(football_1, data_sample_pos == TRUE)
train_data_pos

test_data_pos <- subset(football_1, data_sample_pos == FALSE)
test_data_pos 
dim(test_data_pos)
dim(train_data_pos)
dim(data_sample_pos)

# Linear Regression Model with Original 'possession'
linear_model_pos <- lm(as.numeric(Goals.for) ~ possession, data = train_data_pos)
predictions_pos <- predict(linear_model_pos, newdata = test_data_pos)

# Plot 1: Predictions vs. Actual (Original 'possession')
predict_plot_1 <- plot(test_data_pos$possession, test_data_pos$Goals.for, col = "darkolivegreen", 
                       main = "Predictions vs. Actual (possession)", xlab = "Possession", ylab = "Goals.for")
points(test_data_pos$possession, predictions_pos, col = "purple", pch = 6)
legend("topright", legend = c("Actual", "Predicted"), col = c("darkolivegreen", "purple"),
       pch = c(1, 6))

# Predictions2
# Sample split for Linear Model 2: goals.scored
data_sample_goals <- sample.split(football_1$shot.goals, SplitRatio = 0.80)
data_sample_goals
train_data_goals <- subset(football_1, data_sample_goals == TRUE)
train_data_goals
test_data_goals <- subset(football_1, data_sample_goals == FALSE)
test_data_goals
linear_model_goals <- lm(shot.goals ~ goals.scored, data = train_data_goals)
predictions_goals <- predict(linear_model_goals, newdata = test_data_goals)
predict_goal_plot <-plot(test_data_goals$goals.scored, test_data_goals$shot.goals, col = "yellow", 
                         main = "Predictions vs. Actual (Goals Scored)", 
                         xlab = "Goals Scored", ylab = "Shot Goals")
points(test_data_goals$goals.scored, predictions_goals, col = "navy", pch = 2)
legend("topright", legend = c("Actual", "Predicted"), col = c("yellow", "navy"), pch = c(7, 2))
predict_goal_plot

#prediction3
# Sample split for Linear Model 3: distance
data_sample_distance <- sample.split(football_1$shot.goals, SplitRatio = 0.80)
data_sample_distance
train_data_distance <- subset(football_1, data_sample_distance == TRUE)
train_data_distance
test_data_distance <- subset(football_1, data_sample_distance == FALSE)
test_data_distance
linear_model_distance <- lm(shot.goals ~ distance, data = train_data_distance)
predictions_distance <- predict(linear_model_distance, newdata = test_data_distance)
dist_predict<- plot(test_data_distance$distance, test_data_distance$shot.goals, col = "darkorchid", 
                    main = "Predictions vs. Actual (Distance)", 
                    xlab = "Distance", ylab = "Shot Goals")
points(test_data_distance$distance, predictions_distance, col = "darkorange", pch = 2)
legend("topright", legend = c("Actual", "Predicted"), col = c("darkorchid", "darkorange"), pch = c(1, 2))




#logistic regression
football_1$result_binary <- as.integer(football_1$result == "W")
# Sample split for Logistic Regression
data_sample_log <- sample.split(football_1$result_binary, SplitRatio = 0.80)
train_data_log <- subset(football_1, data_sample_log == TRUE)
test_data_log <- subset(football_1, data_sample_log == FALSE)

# Logistic Regression Model
logistic_model <- glm(result_binary ~ possession + total.Shots.Taken + venue + result, 
                      data = train_data_log, 
                      family = "binomial")

# Summary of the Logistic Regression Model
summary(logistic_model)
# Prediction and Evaluation
predictions_log <- predict(logistic_model, newdata = test_data_log, type = "response")
predictions_binary <- ifelse(predictions_log > 0.5, 1, 0)
# Confusion Matrix
conf_matrix <- table(predictions_binary, test_data_log$result_binary)
# Accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
accuracy
# Precision
precision <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
precision
# Recall (Sensitivity)
recall <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
recall
# F1-Score
f1_score <- 2 * (precision * recall) / (precision + recall)
f1_score
# AUC-ROc
roc_curve <- roc(test_data_log$result_binary, predictions_log)
auc_score <- auc(roc_curve)
auc_score

# Confusion Matrix Plot
conf_matrix_plot_log <- ggplot() +
  geom_tile(aes(x = factor(1), y = factor(1), fill = as.factor(conf_matrix[1, 1])), color = "white") +
  geom_tile(aes(x = factor(2), y = factor(1), fill = as.factor(conf_matrix[1, 2])), color = "white") +
  geom_tile(aes(x = factor(1), y = factor(2), fill = as.factor(conf_matrix[2, 1])), color = "white") +
  geom_tile(aes(x = factor(2), y = factor(2), fill = as.factor(conf_matrix[2, 2])), color = "white") +
  scale_fill_manual(values = c("yellow", "lightblue", "lightcoral", "pink")) +
  theme_minimal() +
  labs(title = "Confusion Matrix - Logistic Regression",
       x = "Predicted",
       y = "Actual",
       fill = "Count")

conf_matrix_plot_log



#naive_bayes_model
# Sample split for Naive Bayes
data_sample_nb <- sample.split(football_1$result_binary, SplitRatio = 0.80)
train_data_nb <- subset(football_1, data_sample_nb == TRUE)
test_data_nb <- subset(football_1, data_sample_nb == FALSE)
# Naive Bayes Model
naive_bayes_model <- naiveBayes(result_binary ~ possession + total.Shots.Taken + venue + result, data = train_data_nb)
# Prediction
predictions_nb <- predict(naive_bayes_model, newdata = test_data_nb)
# Confusion Matrix
conf_matrix_nb <- table(predictions_nb, test_data_nb$result_binary)
# Accuracy Calculation
accuracy_nb <- sum(diag(conf_matrix_nb)) / sum(conf_matrix_nb)
cat("Naive Bayes Accuracy:", accuracy_nb, "\n")

#visualization
conf_matrix_plot_nb <- ggplot() +
  geom_tile(aes(x = factor(1), y = factor(1), fill = as.factor(conf_matrix_nb[1, 1])), color = "white") +
  geom_tile(aes(x = factor(2), y = factor(1), fill = as.factor(conf_matrix_nb[1, 2])), color = "white") +
  geom_tile(aes(x = factor(1), y = factor(2), fill = as.factor(conf_matrix_nb[2, 1])), color = "white") +
  geom_tile(aes(x = factor(2), y = factor(2), fill = as.factor(conf_matrix_nb[2, 2])), color = "white") +
  scale_fill_manual(values = c("green", "lightblue", "lightcoral", "pink")) +
  theme_minimal() +
  labs(title = "Confusion Matrix - Naive Bayes",
       x = "Predicted",
       y = "Actual",
       fill = "Count")

conf_matrix_plot_nb


