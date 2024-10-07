#-----Section 01-------------------------------------------

#Before model testing and training,
#follow the QDA script of "u2550212_DS7003_CW_DataExploratory.R"

# Load necessary packages
library(caret)         # Train/test functions
library(pacman)        # Load/unload packages

set.seed(123)

#---------------------Section 02-------------------
# Evaluate the model
# Generate predictions for the testing dataset using the regression tree model
library(rpart)
m.rpart <- rpart(NObeyesdad ~ ., data = obesity_train)
p1.rpart <- predict(m.rpart, obesity_test)

# Compare the distribution of predicted values vs. actual values
summary(p1.rpart)
summary(obesity_test$NObeyesdad)

boxplot(obesity_test$NObeyesdad, p1.rpart, names = c("Actual", "Predicted"), main = "Obesity Level")
plot(obesity_test$NObeyesdad, p1.rpart, main = "Actual vs Predicted", xlab = "Actual", ylab = "Predicted")

# Prune the regression tree
m.rpart_prune <- prune(m.rpart, cp = 0.05)

# Generate predictions for the testing dataset using the pruned tree
p2.rpart <- predict(m.rpart_prune, obesity_test)
boxplot(obesity_test$NObeyesdad, p2.rpart, names = c("Actual", "Predicted"), main = "Obesity Level")
plot(obesity_test$NObeyesdad, p2.rpart, main = "Actual vs Predicted", xlab = "Actual", ylab = "Predicted")
cor.test(obesity_test$NObeyesdad, p2.rpart, method = "spearman", exact = FALSE)

#-----Section 03: Regression Tree----------------------
#- Train multiple models and compare their performance using appropriate evaluation metrics (e.g., accuracy, precision, recall, F1-score).
# Load the required library for decision trees
library(rpart.plot)

# Train Decision Tree model
decision_tree_model <- rpart(NObeyesdad ~ ., data = obesity_train, method = "anova")

# Visualize the decision tree
rpart.plot(decision_tree_model, box.palette = "RdBu", shadow.col = "gray", nn = TRUE)

# Make predictions on the test data
obesity_predictions <- predict(decision_tree_model, obesity_test)

# Convert obesity_predictions to factor with the same levels as obesity_test$NObeyesdad
obesity_predictions <- factor(obesity_predictions, levels = levels(obesity_test$NObeyesdad))

# Compute confusion matrix
confusion_matrix <- confusionMatrix(obesity_test$NObeyesdad, obesity_test_pred)

# Display evaluation metrics
confusion_matrix

{
  #Collecting Selected details for further decisions.
  Accuracy <- confusion_matrix$overall["Accuracy"]
  Kappa <- confusion_matrix$overall["Kappa"]
  F1_Class1 <- confusion_matrix$byClass["Class: 1","F1"]
  F1_Class2 <- confusion_matrix$byClass["Class: 2","F1"]
  F1_Class3 <- confusion_matrix$byClass["Class: 3","F1"]
  F1_Class4 <- confusion_matrix$byClass["Class: 4","F1"]
  F1_Class5 <- confusion_matrix$byClass["Class: 5","F1"]
  F1_Class6 <- confusion_matrix$byClass["Class: 6","F1"]
  F1_Class7 <- confusion_matrix$byClass["Class: 7","F1"]
  # Create a group of values collection for table
  dtree_value1 <- c(Accuracy, Kappa, F1_Class1, F1_Class2, F1_Class3, F1_Class4, F1_Class5, F1_Class6, F1_Class7)
}

# --------------
# Set up the trainControl function for cross-validation
ctrl <- trainControl(method = "cv", number = 5)

# Define the tuning grid
tuneGrid <- expand.grid(cp = seq(0.01, 0.05, by = 0.01))  # Grid for complexity parameter (cp)

# Tune the model
tuned_model <- train(NObeyesdad ~ .,
                     data = obesity_train,
                     method = "rpart",
                     trControl = ctrl,
                     tuneGrid = tuneGrid)

# Get the best hyperparameters
best_cp <- tuned_model$bestTune

# Train Decision Tree model with the best hyperparameters
final_decision_tree_model <- rpart(NObeyesdad ~ .,
                                   data = obesity_train,
                                   method = "class",
                                   cp = 0.05)

# Visualize the decision tree
rpart.plot(final_decision_tree_model, box.palette = "RdBu", shadow.col = "gray", nn = TRUE)
dev.off()

# Make predictions on the test data
obesity_predictions <- predict(final_decision_tree_model, obesity_test, type = "class")

# Evaluate model performance using appropriate metrics
confusion_matrix <- confusionMatrix(obesity_predictions, obesity_test$NObeyesdad)

# Print evaluation metrics
print(confusion_matrix)

{
  #Collecting Selected details for further decisions.
  Accuracy <- confusion_matrix$overall["Accuracy"]
  Kappa <- confusion_matrix$overall["Kappa"]
  Sensitivity <- confusion_matrix$byClass["Sensitivity"] 
  Precision <- confusion_matrix$byClass["Precision"] 
  F1 <- confusion_matrix$byClass["F1"] 
  Positive_Class <- confusion_matrix$positive
  
  # Create a group of values collection for table
  dtree_value2 <- c(Accuracy, Kappa, Sensitivity, Precision, F1, Positive_Class)
}

# return to 'best' model, follow the below table
# Combine the metrics and values into a table
dtree_table <- cbind(dtree_value1,dtree_value2)

# Add row names
colnames(dtree_table) <- c("dtree_value1", "dtree_value2")

# Define the evaluation metrics
rownames(dtree_table) <-  c("Accuracy", "Kappa", "F1_Class1", "F1_Class2", "F1_Class3", "F1_Class4", "F1_Class5", "F1_Class6", "F1_Class7")

#View the table and compare
print(dtree_table)
View(dtree_table)

set.seed(12345)
m.rpart <- rpart(NObeyesdad ~ ., data = obesity_train)

# get basic information about the tree
m.rpart

# get more detailed information about the tree
summary(m.rpart)

# use the rpart.plot package to create a visualization
library(rpart.plot)

# a basic decision tree diagram
rpart.plot(m.rpart, digits = 3)

# a few adjustments to the diagram
rpart.plot(m.rpart, digits = 3, fallen.leaves = TRUE, type = 3, extra = 101)

# alternative
library(rattle)

fancyRpartPlot(m.rpart)

#---------------Final End Here-------------
