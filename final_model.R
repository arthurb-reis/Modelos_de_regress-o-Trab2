evaluate_model = function(model_to_test,newData,cutoff_threshold = 0.5){
  
  predicted_probs <- predict(model_to_test, newdata = newData, type = "response")
  predicted_class <- ifelse(predicted_probs > cutoff_threshold, 1, 0)
  
  confusion_matrix <- table(Actual = newData$income, Predicted = predicted_class)
  
  accuracy <- (confusion_matrix[1, 1] + confusion_matrix[2, 2]) / sum(confusion_matrix)
  
  precision <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
  
  recall <- confusion_matrix[2, 2] / sum(confusion_matrix[2 ,])
  
  f1_score <- 2 * (precision * recall) / (precision + recall)
  
  print(confusion_matrix)
  print(paste("Cutoff threshold: ",cutoff_threshold))
  print(paste("accuracy: ",accuracy))
  print(paste("precision: ",precision))
  print(paste("recall: ",recall))
  print(paste("f1_score: ",f1_score))
}

library(caret)

k <- 5
set.seed(123)  
index <- createDataPartition(data$income, p = 0.8, list = FALSE)

# Split the data into training and testing sets
training_data <- data[index, ]
testing_data <- data[-index, ]

cv_folds <- createFolds(training_data$income, k = k)

best_auc <- 0
for (fold in seq_along(cv_folds)) {
  fold_training_data <- training_data[-cv_folds[[fold]], ]
  fold_validation_data <- training_data[cv_folds[[fold]], ]
  
  model <- glm(income ~ . - fnlwgt - age - education.num, data = fold_training_data, family = binomial(link = "logit"))
  evaluate_model(model,fold_validation_data)
  auc <- ROC_eval(model,fold_validation_data)
  if(auc > best_auc){
    best_model_data <- fold_training_data
    best_auc <- auc
  }
}
best_model <- glm(income ~ . - fnlwgt - age - education.num, data = best_model_data, family = binomial(link = "logit"))
summary(best_model)
evaluate_model(best_model,testing_data)
auc <- ROC_eval(best_model,testing_data)

library(MASS)
#k=10.35 (log(n), for BIC) discard native country by much and race by little; k=2 (actual AIC) doesn't discard anyone 
final_model <- stepAIC(best_model, direction = "both", k=10.35)

summary(final_model)
evaluate_model(final_model,testing_data)
auc <- ROC_eval(final_model,testing_data)
evaluate_model(final_model,testing_data,cutoff_threshold = 0.25)
