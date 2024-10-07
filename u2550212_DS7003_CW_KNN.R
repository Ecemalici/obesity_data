#-----Section 01-------------------------------------------

#Before model testing and training,
#follow the QDA script of "u2550212_DS7003_CW_DataExploratory.R"

#-----Section 02-------------------------------------------
# Training a model on the data
# load the "class" library
library(class)

# Perform kNN, use k=21 as starting point
obesity_test_pred <- knn(train = obesity_train, test = obesity_test,
                         cl = obesity_train_labels, k=6)

# Inspect results
obesity_test_pred

#-----Section 03-------------------------------------------
# Evaluating model performance
library(gmodels)
library(caret)
# Create the cross tabulation of predicted vs. actual
CrossTable(x = obesity_test_labels, y = obesity_test_pred, prop.chisq=FALSE)

#While our class variable is a numeric so converting into factor
obesity_test$NObeyesdad <- factor(obesity_test$NObeyesdad)

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
  values_k6 <- c(Accuracy, Kappa, F1_Class1, F1_Class2, F1_Class3, F1_Class4, F1_Class5, F1_Class6, F1_Class7)
}

#-----Section 04-------------------------------------------
# Try several different values of k (odd values)

obesity_test_pred <- knn(train = obesity_train, test = obesity_test, cl = obesity_train_labels, k=1)
CrossTable(x = obesity_test_labels, y = obesity_test_pred, prop.chisq=FALSE)

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
  values_k1 <- c(Accuracy, Kappa, F1_Class1, F1_Class2, F1_Class3, F1_Class4, F1_Class5, F1_Class6, F1_Class7)
}

#--------------------K = 10
obesity_test_pred <- knn(train = obesity_train, test = obesity_test,
                         cl = obesity_train_labels, k=10)
CrossTable(x = obesity_test_labels, y = obesity_test_pred, prop.chisq=FALSE)
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
  values_k10 <- c(Accuracy, Kappa, F1_Class1, F1_Class2, F1_Class3, F1_Class4, F1_Class5, F1_Class6, F1_Class7)
}


# Display evaluation metrics and comparing with previous results with new.
# Define the values for the previous results

# Evaluate qualitatively, PCC, FN vs FP, k=1 may overfit and not be good predictor

# Combine the metrics and values into a matrix
knn_table <- cbind(values_k1, values_k6, values_k10)

# Add row names
colnames(knn_table) <- c("KNN_k1", "KNN_k6", "KNN_k10")

# Define the evaluation metrics
rownames(knn_table) <-  c("Accuracy", "Kappa", "F1_Class1", "F1_Class2", "F1_Class3", "F1_Class4", "F1_Class5", "F1_Class6", "F1_Class7")

#View the table and compare
print(knn_table)
View(knn_table)

#---------------Final End Here-------------
