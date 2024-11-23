install.packages("tidyverse")
library(tidyverse)
install.packages("caret")
library(caret)
install.packages("dslabs")
library(dslabs)


horse_colic_data <- read.csv("C:/Users/NANA/Downloads/horse.csv")

# View the first few rows
head(horse_colic_data)
glimpse(horse_colic_data)
str(horse_colic_data)

# Check the structure of the dataset

colnames(horse_colic_data)
View(horse_colic_data)
nrow(horse_colic_data)
ncol(horse_colic_data)
dim(horse_colic_data)
# column types whether categorical or numeric
sapply(horse_colic_data,class)


#check for missing values
anyNA(horse_colic_data)
#is.na(horse_colic_data)
sum(is.na(horse_colic_data))
#summary statistics for checking na
summary(horse_colic_data)
#checking columns with na 
colSums(is.na(horse_colic_data))
#checking for distinct values in certain categorical columns
unique(horse_colic_data$temp_of_extremities)
length(unique(horse_colic_data$temp_of_extremities)) # number of distinct values
table(horse_colic_data$mucous_membrane)
horse_colic_data %>% distinct(abdomo_appearance)
n_distinct(horse_colic_data$abdomo_appearance) # number of distinct value
#unique_check <- c("surgery","age","temp_of_extremities","peripheral_pulse","mucous_membrane","capillary_refill_time","pain","peristalsis")
#imputing missing values in rectal_temp with mean
#horse_colic_data$rectal_temp[is.na(horse_colic_data$rectal_temp)] <- mean(horse_colic_data$rectal_temp,na.rm = TRUE)
#sum(is.na(horse_colic_data$rectal_temp))
#distribution of outcomes
horse_colic_data %>%  ggplot(aes(x = outcome)) + 
  geom_bar(fill = "skyblue") + 
  theme_minimal() + 
  labs(title = "Distribution of Horse Colic Outcomes", x = "Outcome", y = "Count")

#Age distribution of horses
horse_colic_data %>%  ggplot(aes(x = (as.factor(age)))) + 
  geom_bar(fill = "green", color = "black") + 
  theme_minimal() + 
  labs(title = "Age Distribution of Horses", x = "Age", y = "Count")

#numerical variables
num_variables <- c("rectal_temp","pulse","respiratory_rate","nasogastric_reflux_ph","packed_cell_volume","total_protein","abdomo_protein")
imputer <- function(data,columns){
  for (col in columns){
    avg_x <- mean(data[[col]],na.rm=TRUE)
    data[[col]][is.na(data[[col]])] <- avg_x
  }
  return(data)
}
horse_colic_data <- imputer(horse_colic_data,num_variables)
#check for missing values after imputing numerical variables
colSums(is.na(horse_colic_data))
#Correlation heatmap for numerial variables
install.packages("corrplot")
library(corrplot)
corr_matrix <- cor(horse_colic_data[, sapply(horse_colic_data, is.numeric)])
corrplot(corr_matrix, method = "color", addCoef.col = "black", 
         title = "Correlation Heatmap", tl.cex = 0.6)
# Survival by treatment type
horse_colic_data %>%  ggplot(aes(x = surgery, fill = outcome)) + 
  geom_bar(position = "fill") + 
  theme_minimal() + 
  labs(title = "Survival by Treatment Type", x = "Surgery", y = "Proportion")
#survival by age group
horse_colic_data %>%  ggplot(aes(x = age, fill = outcome)) + 
  geom_bar(position = "fill") + 
  theme_minimal() + 
  labs(title = "Survival by Treatment Type", x = "Surgery", y = "Proportion")


#removing hospital_number column
horse_colic_data <- horse_colic_data %>%select(-hospital_number)
colnames(horse_colic_data)

#checking  all categorical variables
cat_variables <- horse_colic_data %>% select(-all_of(num_variables))
cat_variables

#replace NAs in categorical variables with mode
# Function to calculate mode (most frequent value)
get_mode <- function(x) {
  freq_table <- table(x)  # Create a frequency table of the values
  mode_val <- names(freq_table)[which.max(freq_table)]  # Find the value with the highest frequency
  return(mode_val)
}
#convert all characters to factors
for (col in names(cat_variables)) {
  if (is.character(horse_colic_data[[col]])) {
    horse_colic_data[[col]] <- as.factor(horse_colic_data[[col]])  
  }
}



# Loop through each categorical variable column in the dataframe
for (col in names(cat_variables)) {
  # Check if the column is a factor (categorical data)
  if (is.factor(horse_colic_data[[col]])) {
    # Check for NAs before replacing them
    if (sum(is.na(horse_colic_data[[col]])) > 0) {
      # Get the mode (most frequent value) for each column
      mode_val <- get_mode(horse_colic_data[[col]])  # Extract the column using [[col]]
      
      # Impute NAs in the column with the mode
      horse_colic_data[[col]][is.na(horse_colic_data[[col]])] <- mode_val
    }
  } else {
    # If the column is not a factor, print a message (optional)
    print(paste(col, "is not a factor. It will not be processed."))
  }
}




#checking for the unique set in all categorical
unique_value_list <- list()
for (col in names(cat_variables)){
  uniqueness <- length(unique(cat_variables[[col]]))
  unique_value_list[[col]] <- uniqueness
}
unique_value_list


#Since lesions 1,2,3 are not factors but has a combination of codes(composite)


# Split the code into separate components for each lesion
split_code <- function(code) {
  # Extract each part of code
  site <- as.factor(substr(code, 1, 1))  # First digit
  type <- as.factor(substr(code, 2, 2))  # Second digit
  subtype <- as.factor(substr(code, 3, 3))  # Third digit
  specific_code <- as.factor(substr(code, 4, 4))  # Fourth digit
  
  # Return the split parts as a list or dataframe
  return(c(site, type, subtype, specific_code))
}

# Apply the function to lesion_1, lesion_2, and lesion_3
horse_colic_data$site_1 <- sapply(horse_colic_data$lesion_1, function(x) split_code(x)[1])
horse_colic_data$type_1 <- sapply(horse_colic_data$lesion_1, function(x) split_code(x)[2])
horse_colic_data$subtype_1 <- sapply(horse_colic_data$lesion_1, function(x) split_code(x)[3])
horse_colic_data$specific_code_1 <- sapply(horse_colic_data$lesion_1, function(x) split_code(x)[4])

horse_colic_data$site_2 <- sapply(horse_colic_data$lesion_2, function(x) split_code(x)[1])
horse_colic_data$type_2 <- sapply(horse_colic_data$lesion_2, function(x) split_code(x)[2])
horse_colic_data$subtype_2 <- sapply(horse_colic_data$lesion_2, function(x) split_code(x)[3])
horse_colic_data$specific_code_2 <- sapply(horse_colic_data$lesion_2, function(x) split_code(x)[4])

horse_colic_data$site_3 <- sapply(horse_colic_data$lesion_3, function(x) split_code(x)[1])
horse_colic_data$type_3 <- sapply(horse_colic_data$lesion_3, function(x) split_code(x)[2])
horse_colic_data$subtype_3 <- sapply(horse_colic_data$lesion_3, function(x) split_code(x)[3])
horse_colic_data$specific_code_3 <- sapply(horse_colic_data$lesion_3, function(x) split_code(x)[4])

#verify dataset is in right format for prediction
head(horse_colic_data)
str(horse_colic_data)
sapply(horse_colic_data,class)

#removing columns not needed for machine learning 
horse_colic_data <- horse_colic_data %>% select(-lesion_1,-lesion_2,-lesion_3)
horse_colic_data

#getting train and test data
set.seed(123)
trainIndex <- createDataPartition(horse_colic_data$outcome,p=0.8,list = FALSE)
train_data <- horse_colic_data[trainIndex,]
test_data <- horse_colic_data[-trainIndex,]

#installing packages
install.packages("nnet")          #multinomial logistic regression,neural network
install.packages("rpart")         # Decision Tree
install.packages("randomForest")  # Random Forest
install.packages("e1071")         # SVM, Naive Bayes
install.packages("class")         # KNN
install.packages("gbm")           # Gradient Boosting Machine
install.packages("glmnet")        # Elastic Net Logistic Regression

#run rf model 
library(randomForest)
rf_model <- randomForest(outcome ~ ., data = train_data, importance = TRUE, ntree = 100)
summary(rf_model)
rf_predictions <- predict(rf_model,newdata = test_data)
library(e1071)
conf_matrix <- confusionMatrix(rf_predictions, test_data$outcome)
print(conf_matrix)
print(conf_matrix$byClass)
importance(rf_model)


#run multinomial logistic regression
library(nnet)
multinom_model <- multinom(outcome ~ ., data = train_data)
summary(multinom_model)
predicted_probs <- predict(multinom_model, newdata = test_data, type = "probs")
# Get the predicted class labels (based on highest probability)
predicted_classes <- predict(multinom_model, newdata = test_data)
conf_matrix <- confusionMatrix(predicted_classes, test_data$outcome)
print(conf_matrix)
accuracy <- conf_matrix$overall["Accuracy"]
print(paste("Accuracy: ", accuracy))

#run decision tree
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)
decision_model <- rpart(outcome ~ ., data = train_data, method = "class")
summary(decision_model)
#visualize decision tree
rpart.plot(decision_model, main = "Decision Tree for Horse Colic Outcome", extra = 104)
predicted_classes <- predict(decision_model, newdata = test_data, type = "class")
conf_matrix <- confusionMatrix(predicted_classes, test_data$outcome)
print(conf_matrix)
accuracy <- conf_matrix$overall["Accuracy"]
print(paste("Accuracy: ", accuracy))

#run svm
library(e1071)
svm_model <- svm(outcome ~ ., data = train_data, kernel = "radial", cost = 1, gamma = 0.1)
summary(svm_model)
predicted_classes <- predict(svm_model, newdata = test_data)
conf_matrix <- confusionMatrix(predicted_classes, test_data$outcome)
print(conf_matrix)
accuracy <- conf_matrix$overall["Accuracy"]
print(paste("Accuracy: ", accuracy))

#run naive bayes    
naive_bayes_model <- naiveBayes(outcome ~ ., data = train_data)
summary(naive_bayes_model)
predicted_classes <- predict(naive_bayes_model, newdata = test_data)
conf_matrix <- confusionMatrix(predicted_classes, test_data$outcome)
print(conf_matrix)
accuracy <- conf_matrix$overall["Accuracy"]
print(paste("Accuracy: ", accuracy))
    
  
#run gbm
library(gbm)
gbm_model <- gbm(outcome ~ ., 
                 data = train_data, 
                 distribution = "multinomial",  # For multi-class classification
                 n.trees = 1000,                # Number of boosting iterations (trees)
                 interaction.depth = 3,         # Max depth of trees
                 shrinkage = 0.01,              # Learning rate
                 cv.folds = 5,                  # Cross-validation folds
                 n.cores = NULL,                # Number of cores to use (NULL uses all available)
                 verbose = TRUE)

#Plot the variable importance 
# This will show the importance of each feature
plot(gbm_model, i.var = 1)  # Plot importance of the first variable
summary(gbm_model)          # Summarize the model
predictions_gbm <- predict(gbm_model, test_data, n.trees = 1000, type = "response")

# Convert the predictions to class labels
# Since the predictions are in the form of probabilities, we take the class with the highest probability
predicted_class <- apply(predictions_gbm, 1, function(x) colnames(predictions_gbm)[which.max(x)])
conf_matrix <- confusionMatrix(factor(predicted_class), test_data$outcome)
print(conf_matrix)
accuracy <- conf_matrix$overall["Accuracy"]
print(paste("Accuracy: ", accuracy))

#tuning the GBM model using caret's train() function
train_control <- trainControl(method = "cv", number = 5)

# Tuning GBM using caret's train() function
tuned_model <- train(outcome ~ ., data = train_data, method = "gbm", trControl = train_control,
                     tuneGrid = expand.grid(n.trees = c(500, 1000), 
                                            interaction.depth = c(1, 3, 5),
                                            shrinkage = c(0.01, 0.1),
                                            n.minobsinnode = 10))

# Best model after tuning
print(tuned_model)

    


# Neural networks are sensitive to the scale of input features, so we will scale the predictors
numeric_columns <- sapply(train_data, is.numeric)
train_data_scaled <- train_data
test_data_scaled <- test_data

# Scale the predictors (excluding the outcome variable)
train_data_scaled[, numeric_columns] <- scale(train_data[, numeric_columns])
test_data_scaled[, numeric_columns] <- scale(test_data[, numeric_columns])
#  Train the Neural Network Model

# Train the neural network using nnet function
nn_model <- nnet(outcome ~ ., 
                 data = train_data_scaled, 
                 size = 3,          # Number of hidden units (neurons)
                 linout = FALSE,     # Classification task (not regression)
                 decay = 0.001,      # L2 regularization (helps avoid overfitting)
                 maxit = 1000,       # Maximum number of iterations (epochs)
                 trace = TRUE)       # Print progress
predictions_nn <- factor(predict(nn_model, test_data_scaled, type = "class"))
test_data$outcome <- factor(test_data$outcome)
conf_matrix <- confusionMatrix(predictions_nn, test_data$outcome)
print(conf_matrix)
accuracy <- conf_matrix$overall["Accuracy"]
print(paste("Accuracy: ", accuracy))

# Cross-validation using caret
train_control <- trainControl(method = "cv", number = 5)  # 5-fold cross-validation
tuned_nn_model <- train(outcome ~ ., data = train_data, method = "nnet", 
                        trControl = train_control, 
                        tuneGrid = expand.grid(size = c(3, 5, 7), decay = c(0.001, 0.01)),
                        linout = FALSE, trace = FALSE)

# Display the best model found by cross-validation
print(tuned_nn_model)


