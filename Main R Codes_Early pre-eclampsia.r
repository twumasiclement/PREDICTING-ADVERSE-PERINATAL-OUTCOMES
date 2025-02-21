# setting working directory
#setwd("/Users/clementtwumasi/Desktop")

options(repr.plot.width=8, repr.plot.height=8,repr.plot.res = 300) #Setting plot size

# importing the cleaned datasets Fetaldata2
dataset <- read.csv('Fetaldata_cleaned.csv')

names(dataset)

#Loading some relevant R packages
library(caret)
library(pROC)
library(MASS)



################ Primary Analysis of Adverse Perinatal Outcomes ##############

    #For results reproducibility
   set.seed(1234)

    # Create a stratified split based on the target variable
    split <- createDataPartition(dataset$adverseoutcome, p = 0.8, list = FALSE)

    # Create the training and test sets
    train_data <- dataset[split, ]
    test_data <- dataset[-split, ]

    # Check proportions in the target variable
    cat("Proportions in Full Dataset:\n")
     table(dataset$adverseoutcome)
    print(prop.table(table(dataset$adverseoutcome)))

    cat("\nProportions in Training Set:\n")
    table(train_data$adverseoutcome)
    print(prop.table(table(train_data$adverseoutcome)))

    cat("\nProportions in Test Set:\n")
    table(test_data$adverseoutcome)    
    print(prop.table(table(test_data$adverseoutcome)))

train_data <- na.omit(train_data)
test_data <- na.omit(test_data)
dim(test_data)


 # train_data$adverseoutcome <- as.factor(train_data$adverseoutcome)
# test_data$adverseoutcome <- as.factor(test_data$adverseoutcome)
class(train_data$adverseoutcome)



############## Primary Analysis: Fitting the Logistic regression model ##############

logistic_model <- glm(adverseoutcome ~ EFW + GA + UADcode + DVcode + deliverycode, data = na.omit(train_data), family = binomial)
# null_model <- glm(adverseoutcome ~ 1, data = train_data, family = binomial)
# stepwise_model <- stepAIC(null_model, direction = "both",  scope = list(lower = null_model, upper = full_model), trace = FALSE)
epiDisplay::logistic.display(logistic_model, decimal = 4)
train_data$adverseoutcome <- factor(train_data$adverseoutcome, levels = c(0,1), labels = c("No", "Yes"))

train_control <- trainControl(method = "cv", number = 10, classProbs = TRUE, summaryFunction = twoClassSummary)  
logistic_model <- train(adverseoutcome ~ GA + DVcode + UADcode + EFW + deliverycode, 
                        data = train_data, 
                        method = "glm", 
                        family = "binomial",  
                        trControl = train_control,
                        metric = "ROC")

# the best logistic regression model (using the Caret package)
# Specify type of training method used and the number of folds
# ctrlspecs <- trainControl(method="cv", 
                          # number=10, 
                          # savePredictions="all",
                          # classProbs=TRUE)


# logistic_model <- train(adverseoutcome  ~ EFW + GA + UADcode + DVcode + deliverycode,  # Formula specifying the outcome variable and predictors
  # data = na.omit(train_data),  # Your training dataset
  # method = "glm",  # Method for logistic regression
  # trControl=ctrlspecs,  # 10-fold cross-validation
  # family = binomial  # Specify the family for logistic regression
# )

# print(logistic_model)

varImp(logistic_model)

#Assessing goodness-of-fit
predictions_logit <-  predict(logistic_model, newdata = test_data, type = "prob")[ ,2]
predictions_logit_class<- ifelse(as.numeric(predictions_logit)>0.5, "1", "0")

# Create a confusion matrix to assess model fit/performance on test data
conf_matrix_logistic<- caret::confusionMatrix(data=factor(predictions_logit_class), factor(test_data$adverseoutcome))
conf_matrix_logistic

# Extract accuracy
accuracy_logistic <- conf_matrix_logistic$overall['Accuracy']
accuracy_logistic 

# Extract accuracy confidence interval
accuracy_ci_logistic <- conf_matrix_logistic$overall[c('AccuracyLower', 'AccuracyUpper')]
accuracy_ci_logistic

roc_curve_logistic <- roc( test_data$adverseoutcome, as.numeric(predictions_logit))
plot(smooth(roc_curve_logistic, method="density", bandwidth=0.1) , main = "ROC Curve", col = "blue", lwd = 2)
auc(roc_curve_logistic )
print(ci_auc_logistic  <- ci.auc(roc_curve_logistic))



############## Primary Analysis: Fitting the Random Forest model ##############

library(randomForest)
library(mlbench)
library(caret)
library(e1071)

# levels(test_data$UADcode) <- levels(train_data$UADcode)
# print(levels(train_data$UADcode))
# print(levels(test_data$UADcode))
# train_data$UADcode <- factor(train_data$UADcode)
# test_data$UADcode <- factor(test_data$UADcode, levels = levels(train_data$UADcode))


set.seed(123)
 bestMtry <- tuneRF(x,y, stepFactor = 2, improve = 1e-5, ntree = 500)

print(bestMtry)

set.seed(123)
options(warn=-1)
#10 folds repeat 3 times
# Random search
 control <- trainControl(method='repeatedcv',    number=10, repeats=3, classProbs=TRUE, search = 'grid')
# Hyperparameter Tuning
# Define the tuning grid



# control <- trainControl(method = "cv", number = 5,classProbs=TRUE)

#Metric compare model is Accuracy
metric <- "ROC"
set.seed(123)
#Number randomly variable selected is mtry
mtry <- 1 #sqrt(ncol(x))


tunegrid <- expand.grid(.mtry=mtry)
rf_model<- train(adverseoutcome ~ GA + DVcode + UADcode + EFW + deliverycode, 
                      data=train_data, 
                      method='rf', 
                      metric='accuracy', 
                      tuneGrid=tunegrid, 
                      trControl=control,tuneLength = 5)
print(rf_model)

# Getting model predictions
predicted_rf <- predict(rf_model, newdata = test_data)
predicted_classes<- ifelse(as.numeric(predicted_rf  )>0.5, "1", "0")

# Compute Confusion Matrix
conf_matrix_rf <- confusionMatrix(factor(predicted_classes), factor(test_data$adverseoutcome))
print(conf_matrix_rf)

# Extract accuracy
accuracy_rf <- conf_matrix_rf$overall['Accuracy']
accuracy_rf

# Extract accuracy confidence interval
accuracy_ci_rf<- conf_matrix_rf$overall[c('AccuracyLower', 'AccuracyUpper')]
accuracy_ci_rf

library(pROC)

# Compute the ROC curve
roc_curve_rf <- roc(response = test_data$adverseoutcome, predictor = predicted_rf )

# Plot the original ROC curve
plot(roc_curve_rf, main = "ROC Curve for Random Forest")

# Apply smoothing
smoothed_roc_rf <- smooth(roc_curve_rf)

# Plot the smoothed ROC curve in red
plot(smoothed_roc_rf, col = "red", add = TRUE)

# Compute AUC
original_auc_rf <- auc(roc_curve_rf)
print(paste("AUC for original model:", original_auc_rf))

# Confidence interval for AUC
print(ci_auc_rf <- ci.auc(roc_curve_rf))


# Extract variable importance
var_imp_rf <- rf_model$finalModel$importance
var_imp_df_rf <- data.frame(Variable = rownames(var_imp_rf), Importance = var_imp_rf[, 1])
var_imp_df_rf <- var_imp_df_rf[order(-var_imp_df_rf$Importance),]


print(var_imp_df_rf)
# Plot variable importance
ggplot(var_imp_df_rf, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
      theme_minimal() +
  xlab("Variables") +
  ylab("Importance Score") +
  ggtitle("Random Forest Variable Importance")

# Load necessary libraries
library(caret)
library(gbm)
library(pROC)
library(ggplot2)







############## Primary Analysis: Fitting the Gradient Boosting Machines (GBM) model ##############
set.seed(1234)
train_control <- trainControl(method = "cv", number = 10, classProbs = TRUE, summaryFunction = twoClassSummary)

train_data$adverseoutcome <- factor(train_data$adverseoutcome, levels = c(0, 1), labels = c("No", "Yes"))
test_data$adverseoutcome <- factor(test_data$adverseoutcome, levels = c(0, 1), labels = c("No", "Yes"))

#Tuning for hyperparameters
tune_grid <- expand.grid(
  interaction.depth = c(1, 3, 5),  
  n.trees = seq(50, 300, by = 50), 
  shrinkage = c(0.01, 0.1, 0.3),   
  n.minobsinnode = c(10, 20)       
)

#Creating the model
gbm_model <- train(adverseoutcome ~ GA + DVcode + UADcode + EFW + deliverycode, data = train_data, method = "gbm", trControl = train_control,
  metric = "ROC",verbose = FALSE,  tuneGrid = tune_grid)

print(gbm_model$bestTune)

# Model predictions
predictions_gbm <- predict(gbm_model, newdata = test_data, type = "prob")[, 2]  # Probabilities for "Yes"
predicted_classes <- ifelse(predictions_gbm>0.5 ,"Yes","No")

# Ensure factor levels match before confusion matrix
# predicted_classes <- factor(predicted_classes, levels = levels(test_data$adverseoutcome))

# Compute Confusion Matrix
conf_matrix_gbm <- confusionMatrix(factor(predicted_classes), factor(test_data$adverseoutcome))
print(conf_matrix_gbm)

# Extract accuracy
accuracy_gbm <- conf_matrix_gbm$overall['Accuracy']
accuracy_gbm

# Extract accuracy confidence interval
accuracy_ci_gbm<- conf_matrix_gbm$overall[c('AccuracyLower', 'AccuracyUpper')]
accuracy_ci_gbm

# ROC Curve and AUC
roc_curve_gbm <- roc(response = test_data$adverseoutcome, predictor = predictions_gbm)
# Apply smoothing

plot(smooth(roc_curve_gbm, method="density", bandwidth=.1), main = "ROC Curve for GBM Model")
gbm_auc <- auc(roc_curve_gbm)
print(paste("AUC for GBM model:", gbm_auc))
print(ci_auc_gbm <- ci.auc(roc_curve_gbm))

# Extract feature importance
var_imp_gbm <- varImp(gbm_model, scale = FALSE)
print(var_imp_gbm)

# Plot variable importance
ggplot(var_imp_gbm$importance, aes(x = reorder(rownames(var_imp_gbm$importance), Overall), y = Overall)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  theme_minimal() +
  xlab("Variables") +
  ylab("Importance Score") +
  ggtitle("GBM Variable Importance")






############## Primary Analysis: Fitting the eXtreme Boosting (XGBoost) model ##############
    # train_data$adverseoutcome<- as.numeric(train_data$adverseoutcome)-1
    # test_data$adverseoutcome<- as.numeric(test_data$adverseoutcome)-1
# Specify type of training method and number of folds

set.seed(1234)
ctrl <- trainControl(
  method = "cv",  # Cross-validation
  number = 10,     # Number of folds
  classProbs = TRUE,  # For classification problems
  summaryFunction = multiClassSummary  # For multi-class classification
)
train_data$adverseoutcome <- factor(train_data$adverseoutcome, levels = c(0, 1), labels = c("No", "Yes"))

# Define the tuning grid for XGBoost
xgbGrid <- expand.grid(
  nrounds = c(50, 100, 150),
  max_depth = c(3, 6, 9),
  eta = c(0.01, 0.1, 0.3),
  gamma = c(0, 0.1, 0.2),
  colsample_bytree = c(0.6, 0.8, 1),
  min_child_weight = c(1, 2, 3),
  subsample = c(0.6, 0.8, 1)
)


# Train the model
xgb_model<- train(
  adverseoutcome ~ UADcode + DVcode + deliverycode + EFW + GA, data = train_data, method = "xgbTree", trControl = ctrl,
  tuneGrid = xgbGrid, metric = "Accuracy")

# Print the model
print(bestTune <- xgbModel_Try$bestTune)

# Get predictions
xgb_predictions <- predict(xgb_model, newdata = test_data, type = "prob")[, 2]

# Convert probabilities to class predictions
xgb_class_predictions <- ifelse(xgb_predictions > 0.5, "1", "0")

# Confusion Matrix
conf_matrix_xgb<- confusionMatrix(factor(xgb_class_predictions), factor(test_data$adverseoutcome))
conf_matrix_xgb

# Extract accuracy
accuracy_xgb<- conf_matrix_xgb$overall['Accuracy']
accuracy_xgb

# Extract accuracy confidence interval
accuracy_ci_xgb<- conf_matrix_xgb$overall[c('AccuracyLower', 'AccuracyUpper')]
accuracy_ci_xgb

roc_curve_xgb <- roc(response = test_data$adverseoutcome, predictor = xgb_predictions)
plot(smooth(roc_curve_xgb), main = "ROC Curve for XGBoost Model")
xgboost_auc <- auc(roc_curve_xgb)
print(paste("AUC for XGBoost model:", xgboost_auc))
print(ci_auc_xgb <- ci.auc(roc_curve_xgb))

 # train_data$adverseoutcome<- as.numeric(train_data$adverseoutcome)-1
  # test_data$adverseoutcome<- as.numeric(test_data$adverseoutcome)-1







############## Primary Analysis: Fitting the KNN model ##############
   
set.seed(1)

# Set up training control for cross-validation with class probabilities
train_control <- trainControl(method = "cv", 
                               number = 10, 
                               classProbs = TRUE,   
                               summaryFunction = twoClassSummary)  

# Define tuning grid for kNN
tuneGrid <- expand.grid(k = seq(1, 20, by = 1)) 

# Train kNN model
knn_model <- train(adverseoutcome ~ GA + DVcode + UADcode + EFW + deliverycode, data = train_data, method = "knn", 
                   trControl = train_control, metric = "ROC", tuneGrid = tuneGrid)

# Output the best tuning parameters
print(knn_model$bestTune)

# Specify type of training method and number of folds

# Define hyperparameter grid for KNN (number of neighbors)
# Define tuning grid for kNN
# tuneGrid <- expand.grid(k = seq(1, 20, by = 1)) 


# Predict probabilities on test data
knnPredict<- predict(knn_model, newdata = test_data, type = "prob")[, 2]
# Predict class labels
knn_classes <- ifelse( knnPredict >0.5, 1, 0)

# Evaluate accuracy
conf_matrix_knn<- confusionMatrix(factor(knn_classes), factor(test_data$adverseoutcome))
conf_matrix_knn

# Extract accuracy
accuracy_knn<- conf_matrix_knn$overall['Accuracy']
accuracy_knn

# Extract accuracy confidence interval
accuracy_ci_knn<- conf_matrix_knn$overall[c('AccuracyLower', 'AccuracyUpper')]
accuracy_ci_knn

# Compute ROC curve

roc_curve_knn <- roc(test_data$adverseoutcome, knnPredict )

# Plot ROC curve
options(repr.plot.width=8, repr.plot.height=8,repr.plot.res = 300) #Setting plot size
plot(smooth(roc_curve_knn, method="density", bandwidth=0.1),
  main="ROC Curve for k-NN Model") 
# Reset to default plot settings
options(repr.plot.width = 8, repr.plot.height = 8, repr.plot.res = 300)

# Print AUC
auc_value_knn <- auc(roc_curve_knn)
print(paste("AUC:", auc_value_knn))
print(ci_auc_knn <- ci.auc(auc_value_knn))








############## Primary Analysis: Fitting the Support Vector Machine (SVM) model ##############

# Install if necessary 
# install.packages("caret")
# install.packages("kernlab")  # For SVM with caret


# Load libraries
library(caret)
library(kernlab)



set.seed(123456789)
library(caret)
library(e1071)  # For SVM

# Convert adverseoutcome to factor (for classification
train_data$adverseoutcome <- factor(train_data$adverseoutcome, levels = c(0,1), label = c("No", "Yes"))


# Define the grid for hyperparameter tuning (C for linear kernel)
#tune_grid <- expand.grid(
 # C = seq(0.5, 10, by = 0.5)  # Try values like 0.5, 1, 1.5, 2
#)

svm_model <- train(adverseoutcome ~ GA + EFW + UADcode + DVcode + deliverycode, 
                   data = train_data, 
                   method = "svmLinear",
                   trControl = trainControl(method = "cv", number = 10, classProbs = TRUE), 
                   #tuneGrid = tune_grid,
                   probability = TRUE)



# Output the best tuning result
print(svm_model$bestTune)

# Make predictions on the test data
# predictions_svm <- predict(svm_model, newdata = test_data)

predictions_svm <- predict(svm_model , newdata = test_data,type="prob")[,2]
positive_class_probs <- as.numeric(predictions_svm )


svm1_class_predictions <- ifelse(positive_class_probs  > 0.5, 1, 0)
#Get the confusion matrix to see accuracy value and other parameter values
conf_matrix_svm<- confusionMatrix(factor(svm1_class_predictions), factor(test_data$adverseoutcome ))
conf_matrix_svm

roc_curve_svm <- roc(factor(test_data$adverseoutcome),positive_class_probs)
# Plot ROC curve
options(repr.plot.width=8, repr.plot.height=8,repr.plot.res = 300) #Setting plot size
plot(smooth(roc_curve_svm, method="density", bandwidth=0.1),main="ROC Curve for SVM Linear Model") 
# Reset to default plot settings
options(repr.plot.width = 8, repr.plot.height = 8, repr.plot.res = 300)

# Print AUC
auc_value_svm <- auc(roc_curve_svm )
print(paste("AUC:", auc_value_svm))
print(ci_auc_svm  <- ci.auc(auc_value_svm))


# Extract accuracy
accuracy_svm<- conf_matrix_svm$overall['Accuracy']
accuracy_svm

# Extract accuracy confidence interval
accuracy_ci_svm<- conf_matrix_svm$overall[c('AccuracyLower', 'AccuracyUpper')]
accuracy_ci_svm






############## Primary Analysis: Fitting the Gaussian Naive Bayes (GNB) model ##############

 

set.seed(12345)
library(caret)
library(e1071) 

train_data$adverseoutcome <- factor(train_data$adverseoutcome, levels = c(0,1), labels = c("No","Yes"))
test_data$adverseoutcome <- factor(test_data$adverseoutcome, levels = c(0,1), labels = c("No","Yes"))                                    

tune_grid <- expand.grid(fL = seq(0, 1, by = 0.1),
                         usekernel = c(TRUE, FALSE),
                         adjust = seq(0.5, 1.5, by = 0.5))

train_control <- trainControl(method = "cv", number = 10)

# Train the Gaussian Naive Bayes model with the correct tuning parameters
gnb_model <- train(adverseoutcome ~ EFW + UADcode + DVcode + GA + deliverycode, 
                   data = train_data, 
                   method = "nb", 
                   trControl = train_control,
                   tuneGrid = tune_grid)


# Print the model
# print(gnb_model)
 
# Make predictions on the test set
predicted_classes <- predict(gnb_model, newdata = test_x)
 
# Compute Confusion Matrix
conf_matrix_gnb <- confusionMatrix(predicted_classes, test_y)
print(conf_matrix_gnb)
 
# Convert test labels to numeric for ROC curve (0 = No, 1 = Yes)
test_y_numeric <- as.numeric(test_y) - 1  # Convert "No" to 0, "Yes" to 1
 
# Get predicted probabilities for the positive class
predicted_gnb <- predict(gnb_model, newdata = test_x, type = "prob")[, "Yes"]
 
# Compute ROC curve
roc_curve_gnb  <- roc(test_y_numeric, predicted_gnb)
 

# Extract accuracy
accuracy_gnb<- conf_matrix_gnb$overall['Accuracy']
accuracy_gnb

# Extract accuracy confidence interval
accuracy_ci_gnb<- conf_matrix_gnb$overall[c('AccuracyLower', 'AccuracyUpper')]
accuracy_ci_gnb

# Plot ROC curve
plot(smooth(roc_curve_gnb, method="density", bandwidth=0.1), main= "ROC Curve for Gaussian Naive Bayes Model (caret)") 
 
# Print AUC
auc_value_gnb <- auc(roc_curve_gnb)
print(paste("AUC:", auc_value_gnb ))
 
# Print AUC confidence interval
ci_auc_gnb  <- ci.auc(auc_value_gnb )
print(ci_auc_gnb)



# Function to convert importance scores into percentages
percent_importance_func<- function(x){
        prop<- (x/sum(x))*100
    return(prop)

    }

# Models: Logistic, RF, GBM, KNN, XGBoost, SVM and GNB. 
# 1. Variable Importance - Random Forest
rf_importance <- varImp(rf_model, scale = FALSE)
rf_imp <- as.data.frame(rf_importance$importance)
rf_imp$Feature <- rownames(rf_imp)
colnames(rf_imp) <- c("RF_Importance", "Feature")
rf_imp$RF_Importance_Percent<- percent_importance_func(x=rf_imp$RF_Importance)
rf_imp<- rf_imp[, -1]
 # rf_imp

# 2. Variable Importance - GBM
gbm_importance <- varImp(gbm_model, scale = FALSE)
gbm_imp<- gbm_importance$importance
gbm_imp$Feature <- rownames(gbm_imp)
gbm_imp$Feature<- c('GA','DVcode', 'UADcode',"EFW",'deliverycode')
colnames(gbm_imp) <- c("GBM_Importance","Feature")
gbm_imp$GBM_Importance_Percent<- percent_importance_func(x=gbm_imp$GBM_Importance)
gbm_imp<- gbm_imp[, -1]
  # gbm_imp


# 3. Variable Importance - XGBoost
xgb_importance <- varImp(xgb_model)
xgb_imp <- xgb_importance$importance
xgb_imp$Feature<- rownames(xgb_imp)
colnames(xgb_imp) <- c("XGB_Importance", "Feature")
xgb_imp$XGB_Importance_Percent<- percent_importance_func(x=xgb_imp$XGB_Importance)
xgb_imp<- xgb_imp[, -1]
 # xgb_imp

# 4. Variable Importance - SVM

# Use caret's varImp function to get feature importance
svm_importance <- varImp(svm_model, scale = FALSE)
svm_imp <- as.data.frame(svm_importance$importance)
svm_imp$Feature <- rownames(svm_imp)
svm_imp=svm_imp[, -1]
colnames(svm_imp) <- c("SVM_Importance", "Feature")
svm_imp$SVM_Importance_Percent<- percent_importance_func(x=svm_imp$SVM_Importance)
svm_imp<- svm_imp[, -1]
 # svm_imp


# 5. Variable Importance - Logistic Regression
logistic_importance<- varImp(logistic_model, scale = FALSE)
logistic_imp<- logistic_importance$importance
logistic_imp$Feature<- rownames(logistic_imp)
# logistic_imp<- gnb_imp[, -2]
 colnames(logistic_imp) <- c("Logistic_Importance", "Feature")
logistic_imp$Logistic_Importance_Percent<- percent_importance_func(x=logistic_imp$Logistic_Importance)
logistic_imp<- logistic_imp[, -1]
   # logistic_imp


# 6 Variable Importance - KNN
knn_importance <-  varImp(knn_model, scale = FALSE)
knn_imp <- as.data.frame(knn_importance$importance)
knn_imp$Feature <- rownames(knn_imp)
 knn_imp<- knn_imp[, -2]
 colnames(knn_imp) <- c("KNN_Importance", "Feature")
 knn_imp$KNN_Importance_Percent<- percent_importance_func(x=knn_imp$KNN_Importance)
 knn_imp<-  knn_imp[, -1]
 # knn_imp

#  7. Variable Importance - GNB
gnb_importance<- varImp(gnb_model, scale = FALSE)
gnb_imp<- as.data.frame(gnb_importance$importance)
gnb_imp$Feature<- rownames(gnb_imp)
gnb_imp<- gnb_imp[, -2]
colnames(gnb_imp) <- c("GNB_Importance", "Feature")
gnb_imp$GNB_Importance_Percent<- percent_importance_func(x=gnb_imp$GNB_Importance)
gnb_imp<- gnb_imp[, -1]
 # gnb_imp



# Merge importance scores
# Assuming each importance dataframe has columns: "Feature" and "Importance"
importance_combined <- Reduce(function(x, y) merge(x, y, by = "Feature", all = TRUE), 
                               list(rf_imp, gbm_imp, xgb_imp, svm_imp, knn_imp,logistic_imp,gnb_imp))

# importance_combined



# Rename columns for clarity (optional)
 colnames(importance_combined) <- c("Feature", "RF_Importance_Percent", "GBM_Importance_Percent", "XGB_Importance_Percent",
             "SVM_Importance_Percent", "KNN_Importance_Percent","Logistic_Importance_Percent","GNB_Importance_Percent")

# Handle any NAs (if some features were not present in all models)
 importance_combined[is.na(importance_combined)] <- 0  # Replace NA with 0 (assuming missing means no importance)

 importance_combined$Mean_Importance_Percent <- rowMeans(importance_combined[, c("RF_Importance_Percent", "GBM_Importance_Percent", 
                                    "XGB_Importance_Percent", "SVM_Importance_Percent", "KNN_Importance_Percent",
                                    "Logistic_Importance_Percent","GNB_Importance_Percent")])


# Setting the importance score in descending order
rf_imp <- rf_imp[order(-rf_imp$RF_Importance_Percent), ]
gbm_imp <- gbm_imp[order(-gbm_imp$GBM_Importance_Percent), ]
xgb_imp <- xgb_imp[order(-xgb_imp$XGB_Importance_Percent), ]
svm_imp <- svm_imp[order(-svm_imp$SVM_Importance_Percent), ]
knn_imp <- knn_imp[order(-knn_imp$KNN_Importance_Percent), ]

logistic_imp<- logistic_imp[order(-logistic_imp$Logistic_Importance_Percent), ]
gnb_imp<- gnb_imp[order(-gnb_imp$GNB_Importance_Percent), ]
# stack_imp<- stack_imp[order(-stack_imp$Stack_Importance), ]


# Sort combined importance by Mean Importance
 importance_combined$Mean_Importance_Percent <- rowMeans(importance_combined[, -1], na.rm = TRUE)
 importance_combined <- importance_combined[order(-importance_combined$Mean_Importance_Percent), ]

# Sort by importance
 print(importance_combined)

# Plot all variable importance together
par(mfrow = c(4, 2), mar=c(5,4,2,1))  # 3x2 layout, adjusted margins for better spacing

colors_model<- c("lightblue",  "pink",
               "orange", "purple","cyan", "red","brown", "lightgreen")

barplot(rf_imp$RF_Importance, names.arg = rf_imp$Feature, las = 1, 
        main = "a) Random Forest (RF)", col = colors_model[1], ylab = "Importance Score (%)")

barplot(gbm_imp$GBM_Importance, names.arg = gbm_imp$Feature, las = 1, 
        main = "b) Gradient Boosting Machine (GBM)", col = colors_model[2], ylab = "Importance Score (%)")

barplot(xgb_imp$XGB_Importance, names.arg = xgb_imp$Feature, las = 1, 
        main = "c) Extreme Gradient Boosting (XGBoost)", col = colors_model[3], ylab = "Importance Score (%)")

barplot(svm_imp$SVM_Importance, names.arg = svm_imp$Feature, las = 1, 
        main = "d) Support Vector Machine (SVM)", col = colors_model[4], ylab = "Importance Score (%)")

barplot(knn_imp$KNN_Importance, names.arg = knn_imp$Feature, las = 1, 
        main = "e) K-Nearest Neighbour (KNN)", col = colors_model[5], ylab = "Importance Score (%)")


 barplot(gnb_imp$GNB_Importance, names.arg = gnb_imp$Feature, las = 1, 
          main = "g) Gaussian Naive Bayes (GNB)", col = colors_model[6], ylab = "Importance Score (%)")

 barplot(logistic_imp$Logistic_Importance, names.arg = logistic_imp$Feature, las = 1, 
          main = "f) Logistic Regression (LR)", col = colors_model[7], ylab = "Importance Score (%)")


barplot(importance_combined$Mean_Importance, names.arg = importance_combined$Feature, las = 1,
        main = "h) Pooled Feature Importance ", col = colors_model[8], ylab = "Mean Importance Score (%)")





# Plot all variable importance together
par(mfrow = c(4, 2), mar=c(5,4,2,1))  # 3x2 layout, adjusted margins for better spacing

colors_model<- c("lightblue",  "pink",
               "orange", "purple","cyan", "red","brown")

barplot(rf_imp$RF_Importance, names.arg = rf_imp$Feature, las = 1, 
        main = "a) Random Forest (RF)", col = colors_model[1], ylab = "Importance Score")

barplot(gbm_imp$GBM_Importance, names.arg = gbm_imp$Feature, las = 1, 
        main = "b) Gradient Boosting Machine (GBM)", col = colors_model[2], ylab = "Importance Score")

barplot(xgb_imp$XGB_Importance, names.arg = xgb_imp$Feature, las = 1, 
        main = "c) Extreme Gradient Boosting (XGBoost)", col = colors_model[3], ylab = "Importance Score")

barplot(svm_imp$SVM_Importance, names.arg = svm_imp$Feature, las = 1, 
        main = "d) Support Vector Machine (SVM)", col = colors_model[4], ylab = "Importance Score")

barplot(knn_imp$KNN_Importance, names.arg = knn_imp$Feature, las = 1, 
        main = "e) K-Nearest Neighbour (KNN)", col = colors_model[5], ylab = "Importance Score")


 barplot(logistic_imp$Logistic_Importance, names.arg = logistic_imp$Feature, las = 1, 
          main = "f) Logistic Regression (LR)", col = colors_model[6], ylab = "Importance Score")

 barplot(gnb_imp$GNB_Importance, names.arg = gnb_imp$Feature, las = 1, 
          main = "g) Gaussian Naive Bayes (GNB)", col = colors_model[7], ylab = "Importance Score")


 barplot(stack_imp$Stack_Importance, names.arg = stack_imp$Feature, las = 1,
          main = "h) Stacked ML models", col = colors_model[8], ylab = "importance Score")

library(pROC)
options(repr.plot.width=8, repr.plot.height=8,repr.plot.res=300) #Setting plot size


# Function to safely smooth the ROC curve, only if possible
safe_smooth <- function(roc_curve) {
  # tryCatch({
      
     smoothed_roc <- smooth(roc_curve,  method = "density", bandwidth = 0.1)
     
    return(smoothed_roc)
  # }, error = function(e) {
     # message("Smoothing failed: ", e$message)
    # return(roc_curve)  # Return the original curve if smoothing fails
  # })
}

# Smooth the ROC curves, if possible
roc_curve_smooth_rf <- smooth(roc_curve_rf)
roc_curve_smooth_gbm <- safe_smooth(roc_curve_gbm)
roc_curve_smooth_xgb <- smooth(roc_curve_xgb)
roc_curve_smooth_svm <- safe_smooth(roc_curve_svm)
# roc_curve_smooth_svm<-safe_smooth(roc_curve_svm)

 # roc_curve_smooth_knn<-safe_smooth(roc_curve_knn)
roc_curve_smooth_knn <- safe_smooth(roc_curve_knn)

roc_curve_smooth_logistic  <- safe_smooth(roc_curve_logistic)
# roc_curve_smooth_logistic <- safe_smooth(roc_curve_logistic)  # Smooth SVM ROC curve

  roc_curve_smooth_gnb <- safe_smooth(roc_curve_gnb)
  # roc_curve_smooth_gnb<-safe_smooth(roc_curve_gnb)

# roc_curve_smooth_ensemble<- smooth(roc_curve_stack, method = "density", bandwidth = 0.1)


# Calculate AUC and its 95% CI for each model
auc_rf <- auc(roc_curve_rf)
auc_rf_ci <- ci.auc(roc_curve_rf)

auc_knn <- auc(roc_curve_knn)
auc_knn_ci <- ci.auc(roc_curve_knn)

auc_gbm <- auc(roc_curve_gbm)
auc_gbm_ci <- ci.auc(roc_curve_gbm)

auc_xgb <- auc(roc_curve_xgb)
auc_xgb_ci <- ci.auc(roc_curve_xgb)

# auc_ensemble <- auc(roc_curve_stack)
# auc_ensemble_ci <- ci.auc(roc_curve_stack)

auc_logreg <- auc(roc_curve_logistic)  # AUC for Logistic Regression
auc_logreg_ci <- ci.auc(roc_curve_logistic)  # CI for Logistic Regression

auc_svm <- auc(roc_curve_svm)  # AUC for SVM
auc_svm_ci <- ci.auc(roc_curve_svm)  # CI for SVM

auc_gnb <- auc(roc_curve_gnb)  # AUC for SVM
auc_gnb_ci <- ci.auc(roc_curve_gnb)  # CI for SVM

# accuracy_stack
# accuracy_ci_stack

# Plot the first ROC curve (Random Forest) with reversed x-axis
options(repr.plot.width=8, repr.plot.height=8,repr.plot.res = 500) #Setting plot size

colors_model<- c("lightblue",  "pink",
               "orange", "purple","cyan", "red","brown")


#Exporting the comparative/combined ROC curves
jpeg("roc_curve_plot_PrimaryAnalysis.jpg", width = 4300, height = 3000, res = 500)


colors_model<- c("lightblue",  "pink",
               "orange", "purple","cyan", "red","brown")

plot(roc_curve_smooth_rf, col = colors_model[1], type = "l", main = "", 
     xlim = c(1, 0), ylim = c(0, 1), lwd = 3, 
     xlab = "Specificity", ylab = "Sensitivity", 
     lty = 1)  # Line type for smooth curve

# Add ROC curve for Gradient Boosting Machine
 lines(roc_curve_smooth_gbm, col = colors_model[2], lwd = 3, lty = 2)  # Dashed line for GBM

# Add ROC curve for XGBoost
lines(roc_curve_smooth_xgb, col = colors_model[3], lwd = 3, lty = 3)  # Dotted line for XGBoost


# Add ROC curve for Support Vector Machine
 lines(roc_curve_smooth_svm, col = colors_model[4], lwd = 3, lty = 4)  # Solid line for SVM


# Add ROC curve for KNN
lines(roc_curve_smooth_knn, col = colors_model[5], lwd = 3, lty = 5)  # Solid line for knn

# Add ROC curve for GNB
lines(roc_curve_smooth_gnb, col = colors_model[6], lwd = 3, lty = 9)  # Solid line for knn

# Add ROC curve for Logistic Regression
lines(roc_curve_smooth_logistic, col = colors_model[7], lwd = 3, lty = 6)  # Solid line for Logistic Regression


# Add ROC curve for Ensemble Stack
# lines(roc_curve_smooth_ensemble, col = colors_model[8], lwd = 3, lty = 8)  # Dash-dot line for Ensemble



legend_labels<-c(paste("Random Forest (AUC = ", round(auc_rf, 3), ", CI: ", 
     round(auc_rf_ci[1], 3), "-", round(auc_rf_ci[3], 3),"; % Accuracy = ", as.numeric(round(accuracy_rf, 3)*100), ", CI: ",
      as.numeric(round(accuracy_ci_rf[1], 3)*100),"-" ,as.numeric(round(accuracy_ci_rf[2], 3)*100), ")", 
                       sep = ""),
paste("GBM (AUC = ", round(auc_gbm, 3), ", CI: ", round(auc_gbm_ci[1], 3), "-", 
      round(auc_gbm_ci[3], 3), "; % Accuracy = ",as.numeric(round(accuracy_gbm, 3)*100), ", CI: ",
      as.numeric(round(accuracy_ci_gbm[1], 3)*100),"-" ,as.numeric(round(accuracy_ci_gbm[2], 3)*100),")",
   
       sep = ""),
paste("XGBoost (AUC = ", round(auc_xgb, 3), ", CI: ", round(auc_xgb_ci[1], 3),
      "-", round(auc_xgb_ci[3], 3), "; % Accuracy = ", as.numeric(round(accuracy_xgb, 3)*100), ", CI: ",
      as.numeric(round(accuracy_ci_xgb[1], 3)*100),"-" ,as.numeric(round(accuracy_ci_xgb[2], 3)*100),")",
      sep = ""),
paste("SVM (AUC = ", round(auc_svm, 3), ", CI: ", round(auc_svm_ci[1], 3), "-", round(auc_svm_ci[3], 3), 
      "; % Accuracy = ", as.numeric(round(accuracy_svm, 3)*100), ", CI: ",
      as.numeric(round(accuracy_ci_svm[1], 3)*100),"-" ,as.numeric(round(accuracy_ci_svm[2], 3)*100),")", 
      sep = ""),
 paste("KNN (AUC = ", round(auc_knn, 3), ", CI: ", round(auc_knn_ci[1], 3), "-", round(auc_knn_ci[3], 3),"; % Accuracy = ", as.numeric(round(accuracy_knn, 3)*100), ", CI: ",
      as.numeric(round(accuracy_ci_knn[1], 3)*100),"-" ,as.numeric(round(accuracy_ci_knn[2], 3)*100), ")", 
       sep = ""),
 paste("GNB (AUC = ", round(auc_gnb, 3), ", CI: ", round(auc_gnb_ci[1], 3), "-", round(auc_gnb_ci[3], 3), "; % Accuracy = ", as.numeric(round(accuracy_gnb, 3)*100), ", CI: ",
      as.numeric(round(accuracy_ci_gnb[1], 3)*100),"-" ,as.numeric(round(accuracy_ci_gnb[2], 3)*100),")", 
       sep = ""),
paste("Logistic Regression (AUC = ", round(auc_logreg, 3), ", CI: ",
      round(auc_logreg_ci[1], 3), "-", round(auc_logreg_ci[3], 3), "; % Accuracy = ", as.numeric(round(accuracy_logistic, 3)*100), ", CI: ",
      as.numeric(round(accuracy_ci_logistic[1], 3)*100),"-" ,as.numeric(round(accuracy_ci_logistic[2], 3)*100)
      ,")", 
      sep = "")
    # ,paste("Stacked ML models (AUC = ", round(auc_ensemble, 3), ", CI: ", 
      # round(auc_ensemble_ci[1], 3), "-", round(auc_ensemble_ci[3], 3), "; % Accuracy = ", as.numeric(round(accuracy_stack, 3)*100), ", CI: ",
      # as.numeric(round(accuracy_ci_stack[1], 3)*100),"-" ,as.numeric(round(accuracy_ci_stack[2], 3)*100),") ",
          # sep = "")     
                )



# Create the legend with AUC and CI values
legend("bottomright", 
       legend = legend_labels,
       col = colors_model, 
       lwd = 3, 
       lty = c(1, 2, 3, 4, 5, 6,9), 
       cex = 0.65)  # Adjust text size for better fit


dev.off()




############## Secondary Analysis of Perinatal Demise ##############



set.seed(1234)

    # Create a stratified split based on the target variable
    split <- createDataPartition(dataset$Demised, p = 0.8, list = FALSE)

    # Create the training and test sets
    train_data <- dataset[split, ]
    test_data <- dataset[-split, ]

    # Check proportions in the target variable
    cat("Proportions in Full Dataset:\n")
     table(dataset$Demised)
    print(prop.table(table(dataset$Demised)))

    cat("\nProportions in Training Set:\n")
    table(train_data$adverseoutcome)
    print(prop.table(table(train_data$Demised)))

    cat("\nProportions in Test Set:\n")
    table(test_data$Demised)    
    print(prop.table(table(test_data$Demised)))


train_data <- na.omit(train_data)
test_data <- na.omit(test_data)


############## Secondary Analysis: Fitting the Logistic regression model ##############

logistic_model2 <- glm(Demised ~ EFW + GA + UADcode + DVcode + deliverycode, data = na.omit(train_data), family = binomial)
# null_model <- glm(Demised ~ 1, data = train_data, family = binomial)
# stepwise_model <- stepAIC(null_model, direction = "both",  scope = list(lower = null_model, upper = full_model), trace = FALSE)
# epiDisplay::logistic.display(logistic_model2, decimal = 4)

# varImp(logistic_model2)

#Assessing goodness-of-fit
predictions_logit2 <-  predict(logistic_model2, newdata = na.omit(test_data))
predictions_logit_class2<- ifelse(as.numeric(predictions_logit2)>0.5, "1", "0")

# Create a confusion matrix to assess model fit/performance on test data
conf_matrix_logistic2<- caret::confusionMatrix(data=factor(predictions_logit_class2), factor(test_data$Demised))
conf_matrix_logistic2

# Extract accuracy
accuracy_logistic2 <- conf_matrix_logistic2$overall['Accuracy']
accuracy_logistic2

# Extract accuracy confidence interval
accuracy_ci_logistic2 <- conf_matrix_logistic2$overall[c('AccuracyLower', 'AccuracyUpper')]
accuracy_ci_logistic2

roc_curve_logistic2 <- roc( test_data$Demised, as.numeric(predictions_logit2))
plot(smooth(roc_curve_logistic2) , main = "Logistic ROC Curve", col = "blue", lwd = 2)
auc(roc_curve_logistic2 )
print(ci_auc_logistic2  <- ci.auc(roc_curve_logistic2))

library(randomForest)
library(mlbench)
library(caret)
library(e1071)



############## Secondary Analysis: Fitting the Random Forest model ##############

set.seed(123)
options(warn=-1)
#10 folds repeat 3 times
# Random search
 control <- trainControl(method='repeatedcv',    number=10, repeats=3, classProbs=TRUE, search = 'grid')
# Hyperparameter Tuning
# Define the tuning grid



# control <- trainControl(method = "cv", number = 5,classProbs=TRUE)

#Metric compare model is Accuracy
metric <- "ROC"
set.seed(123)
#Number randomly variable selected is mtry
mtry <- 1 #sqrt(ncol(x))


tunegrid <- expand.grid(.mtry=mtry)
rf_model2<- train(Demised  ~ GA + DVcode + UADcode + EFW + deliverycode, 
                      data=train_data, 
                      method='rf', 
                      metric='accuracy', 
                      tuneGrid=tunegrid, 
                      trControl=control,tuneLength = 5)
print(rf_model2)

# Getting model predictions
predicted_rf2 <- predict(rf_model2, newdata = test_data)
predicted_classes2<- ifelse(as.numeric(predicted_rf2  )>0.5, "1", "0")

# Compute Confusion Matrix
conf_matrix_rf2 <- confusionMatrix(factor(predicted_classes2), factor(test_data$Demised))
print(conf_matrix_rf2)

# Extract accuracy
accuracy_rf2 <- conf_matrix_rf2$overall['Accuracy']
accuracy_rf2

# Extract accuracy confidence interval
accuracy_ci_rf2<- conf_matrix_rf2$overall[c('AccuracyLower', 'AccuracyUpper')]
accuracy_ci_rf2

library(pROC)

# Compute the ROC curve
roc_curve_rf2 <- roc(response = test_data$Demised, predictor = predicted_rf2 )

# Plot the original ROC curve
plot(smooth(roc_curve_rf2), main = "ROC Curve for Random Forest")

# Apply smoothing
# smoothed_roc_rf <- smooth(roc_curve_rf)

# Plot the smoothed ROC curve in red
# plot(smoothed_roc_rf, col = "red", add = TRUE)

# Compute AUC
original_auc_rf2 <- auc(roc_curve_rf2)
print(paste("AUC for original model:", original_auc_rf2))

# Confidence interval for AUC
print(ci_auc_rf2<- ci.auc(roc_curve_rf2))

# Extract variable importance
var_imp_rf2 <- rf_model2$finalModel$importance
var_imp_df_rf2 <- data.frame(Variable = rownames(var_imp_rf2), Importance = var_imp_rf2[, 1])
var_imp_df_rf2 <- var_imp_df_rf2[order(-var_imp_df_rf2$Importance),]








############## Secondary Analysis: Fitting the GBM model ##############


# Load necessary libraries
library(caret)
library(gbm)
library(pROC)
library(ggplot2)

set.seed(1234)
train_control <- trainControl(method = "cv", number = 10, classProbs = TRUE, summaryFunction = twoClassSummary)

train_data$Demised<- factor(train_data$Demised, levels = c(0, 1), labels = c("No", "Yes"))
test_data$Demised <- factor(test_data$Demised, levels = c(0, 1), labels = c("No", "Yes"))

#Tuning for hyperparameters
tune_grid <- expand.grid(
  interaction.depth = c(1, 3, 5),  
  n.trees = seq(50, 300, by = 50), 
  shrinkage = c(0.01, 0.1, 0.3),   
  n.minobsinnode = c(10, 20)       
)

#Creating the model
gbm_model2 <- train(Demised ~ GA + DVcode + UADcode + EFW + deliverycode, data = train_data, method = "gbm", trControl = train_control,
  metric = "ROC",verbose = FALSE,  tuneGrid = tune_grid)

print(gbm_model2$bestTune)

predictions_gbm2 <- predict(gbm_model2, newdata = test_data, type = "prob")[, 2]  # Probabilities for "Yes"
predicted_classes2 <- ifelse(predictions_gbm2>0.5 ,"Yes","No")

# Ensure factor levels match before confusion matrix
# predicted_classes <- factor(predicted_classes, levels = levels(test_data$adverseoutcome))

# Compute Confusion Matrix
conf_matrix_gbm2 <- confusionMatrix(factor(predicted_classes2), factor(test_data$Demised))
print(conf_matrix_gbm2)

# Extract accuracy
accuracy_gbm2 <- conf_matrix_gbm2$overall['Accuracy']
accuracy_gbm2

# Extract accuracy confidence interval
accuracy_ci_gbm2<- conf_matrix_gbm2$overall[c('AccuracyLower', 'AccuracyUpper')]
accuracy_ci_gbm2

# ROC Curve and AUC
roc_curve_gbm2 <- roc(response = test_data$Demised, predictor = predictions_gbm)
# Apply smoothing

plot(smooth(roc_curve_gbm2), main = "ROC Curve for GBM Model")
gbm_auc2 <- auc(roc_curve_gbm2)
print(paste("AUC for GBM model:", gbm_auc2))
print(ci_auc_gbm2 <- ci.auc(roc_curve_gbm2))

# Extract feature importance
var_imp_gbm2 <- varImp(gbm_model2, scale = FALSE)
# print(var_imp_gbm2)




############## Secondary Analysis: Fitting the XGBoost model ##############


# Specify type of training method and number of folds
control <- trainControl(method = "cv",  # Cross-validation
                        number = 10,    # 10 folds
                        classProbs = TRUE,  # Needed for AUC/ROC
                        summaryFunction = twoClassSummary,  # For binary classification
                        verboseIter = TRUE)

# Define hyperparameter grid for xgboost
grid <- expand.grid(
  nrounds = c(100, 200),      # Number of boosting iterations
  max_depth = c(3, 6, 9),     # Maximum depth of a tree
  eta = c(0.01, 0.1, 0.3),    # Learning rate
  gamma = 0,                  # Minimum loss reduction
  colsample_bytree = 0.8,     # Subsample ratio of columns when constructing each tree
  min_child_weight = 1,       # Minimum sum of instance weight (hessian) needed in a child
  subsample = 0.8             # Subsample ratio of training instances
)



# Set the outcome to be a factor (binary classification)
train_data$Demised<- factor(train_data$Demised, levels = c(0, 1), labels = c("No", "Yes"))

# Fit XGBoost model
set.seed(123)
xgb_model2 <- train(Demised ~ EFW + GA + UADcode + DVcode + deliverycode,
                   data = train_data,
                   method = "xgbTree",
                   trControl = control,
                   tuneGrid = grid,
                   metric = "ROC")  # Use ROC for binary classification

# Get predictions
xgb_predictions2 <- predict(xgb_model2, newdata = test_data, type = "prob")[, 2]

# Convert probabilities to class predictions
xgb_class_predictions2 <- ifelse(xgb_predictions2 > 0.5, "1", "0")

# Confusion Matrix
conf_matrix_xgb2<- confusionMatrix(factor(xgb_class_predictions2), factor(test_data$Demised))
conf_matrix_xgb2

# Get predictions
xgb_predictions2 <- predict(xgb_model2, newdata = test_data, type = "prob")[, 2]

# Convert probabilities to class predictions
xgb_class_predictions2 <- ifelse(xgb_predictions2 > 0.5, "1", "0")

# Confusion Matrix
conf_matrix_xgb2<- confusionMatrix(factor(xgb_class_predictions2), factor(test_data$Demised))
conf_matrix_xgb2

# Extract accuracy
accuracy_xgb2<- conf_matrix_xgb2$overall['Accuracy']
accuracy_xgb2

# Extract accuracy confidence interval
accuracy_ci_xgb2<- conf_matrix_xgb2$overall[c('AccuracyLower', 'AccuracyUpper')]
accuracy_ci_xgb2

roc_curve_xgb2 <- roc(response = test_data$Demised, predictor = xgb_predictions2)
plot(smooth(roc_curve_xgb2, method="density", bandwidth=0.1), main = "ROC Curve for XGBoost Model")
xgboost_auc2 <- auc(roc_curve_xgb2)
print(paste("AUC for XGBoost model:", xgboost_auc2))
print(ci_auc_xgb2 <- ci.auc(roc_curve_xgb2))

# Extract variable importance
var_imp_xgb2 <-varImp( xgb_model2)
var_imp_xgb2 <- var_imp_xgb2$importance
var_imp_df_xgb2 <- data.frame(Variable = rownames(var_imp_xgb2), Importance = var_imp_xgb2[, 1])
var_imp_df_xgb2 <- var_imp_df_xgb2[order(-var_imp_df_xgb2$Importance),]
var_imp_df_xgb2




############## Secondary Analysis: Fitting the KNN model ##############
set.seed(1)

# Set up training control for cross-validation with class probabilities
train_control <- trainControl(method = "cv", 
                               number = 10, 
                               classProbs = TRUE,   
                               summaryFunction = twoClassSummary)  

# Define tuning grid for kNN
tuneGrid <- expand.grid(k = seq(1, 20, by = 1)) 

# Train kNN model
knn_model2 <- train(Demised ~ GA + DVcode + UADcode + EFW + deliverycode, data = train_data, method = "knn", 
                   trControl = train_control, metric = "ROC", tuneGrid = tuneGrid)

# Output the best tuning parameters
print(knn_model2$bestTune)

# Predict probabilities on test data
knnPredict2<-  predict(knn_model2, newdata = test_data, type = "prob")[, 2] #predict(knn_model2, newdata = test_data)
# Predict class labels
knn_classes2 <- ifelse( knnPredict2 >0.5, 1, 0)

# Evaluate accuracy
conf_matrix_knn2<- confusionMatrix(factor(knn_classes2), factor(test_data$Demised))
conf_matrix_knn2

# Extract accuracy
accuracy_knn2<- conf_matrix_knn2$overall['Accuracy']
accuracy_knn2

# Extract accuracy confidence interval
accuracy_ci_knn2<- conf_matrix_knn2$overall[c('AccuracyLower', 'AccuracyUpper')]
accuracy_ci_knn2

# Compute ROC curve

roc_curve_knn2 <- roc(test_data$Demised, knnPredict2)

# Plot ROC curve
options(repr.plot.width=8, repr.plot.height=8,repr.plot.res = 300) #Setting plot size
plot(smooth(roc_curve_knn2),
  main="ROC Curve for k-NN Model") 
# Reset to default plot settings
options(repr.plot.width = 8, repr.plot.height = 8, repr.plot.res = 300)

# Print AUC
auc_value_knn2 <- auc(roc_curve_knn2)
print(paste("AUC:", auc_value_knn2))
print(ci_auc_knn2 <- ci.auc(auc_value_knn2))




############## Secondary Analysis: Fitting the SVM model ##############
# Load libraries
library(caret)
library(kernlab)


# Specify type of training method and number of folds
control <- trainControl(method = "cv",        # Cross-validation
                        number = 10,          # 10 folds
                        classProbs = TRUE,    # Needed for AUC/ROC
                        summaryFunction = twoClassSummary)  # For binary classification


# Ensure the outcome is a factor (binary classification)
train_data$Demised <- factor(train_data$Demised, levels = c(0, 1), labels = c("No", "Yes"))


# Fit SVM linear model
# Define hyperparameter grid for Polynomial kernel SVM  (NB: The SVM Linear wasn't good enough)
grid <- expand.grid(degree = c(2, 3, 4), 
                     scale = c(0.1, 0.5, 1), 
                     C = c(0.01, 0.1, 1, 10))  # Uppercase C!

# Fit SVM linear model
set.seed(123)
#  (NB: The SVM Linear wasn't good enough)
svm_model2 <- train(Demised~ EFW + GA + UADcode + DVcode + deliverycode,
                          data = train_data,
                          method = "svmPoly",  # Polynomial kernel SVM  
                          trControl = control,
                          tuneGrid = grid,
                          preProcess = c("center", "scale"),  
                          metric = "ROC")  

 # Make predictions on the test data
# predictions_svm <- predict(svm_model, newdata = test_data)

predictions_svm2 <- predict(svm_model2 , newdata = test_data,type="prob")[,2]
positive_class_probs2 <- as.numeric(predictions_svm2 )


svm1_class_predictions2 <- ifelse(positive_class_probs2  > 0.5, 1, 0)
#Get the confusion matrix to see accuracy value and other parameter values
conf_matrix_svm2<- confusionMatrix(factor(svm1_class_predictions2), factor(test_data$Demised ))
conf_matrix_svm2

roc_curve_svm2 <- roc(factor(test_data$Demised),positive_class_probs2)
# Plot ROC curve
options(repr.plot.width=8, repr.plot.height=8,repr.plot.res = 300) #Setting plot size
plot(smooth(roc_curve_svm2),main="ROC Curve for SVM Linear Model") 
# Reset to default plot settings
options(repr.plot.width = 8, repr.plot.height = 8, repr.plot.res = 300)

# Print AUC
auc_value_svm2 <- auc(roc_curve_svm2 )
print(paste("AUC:", auc_value_svm2))
print(ci_auc_svm2  <- ci.auc(auc_value_svm2))

# Extract accuracy
accuracy_svm2<- conf_matrix_svm2$overall['Accuracy']
accuracy_svm2

# Extract accuracy confidence interval
accuracy_ci_svm2<- conf_matrix_svm2$overall[c('AccuracyLower', 'AccuracyUpper')]
accuracy_ci_svm2





############## Secondary Analysis: Fitting the Guassian Naive Bayes (GNB) model ##############

# Load necessary libraries
library(caret)
library(pROC)
library("klaR")

train_data$Demised <- factor(train_data$Demised, levels = c(0, 1), labels = c("No", "Yes"))
 test_data$Demised <- factor(test_data$Demised, levels = c(0, 1), labels = c("No", "Yes"))
 
# Define predictor variables
train_x <- train_data[, c("GA", "DVcode", "UADcode", "EFW", "deliverycode")]
test_x <- test_data[, c("GA", "DVcode", "UADcode", "EFW", "deliverycode")]
 
# Define outcome variable
train_y <- train_data$Demised
test_y <- test_data$Demised
 
# Set up cross-validation (optional)
ctrl <- trainControl(method = "cv", number = 10)  # 10-fold cross-validation
 
# Train Gaussian Naive Bayes model using caret
set.seed(1)  # For reproducibility
gnb_model2 <- train(
  x = train_x,                # Predictor variables
  y = train_y,                # Outcome variable
  method = "nb",              # Use Naive Bayes algorithm
  trControl = ctrl,           # Cross-validation settings
  tuneGrid = data.frame(
    fL = 0,                   # Laplace smoothing (default is 0)
    usekernel = FALSE,        # Use Gaussian Naive Bayes (no kernel smoothing)
    adjust = 1                # Bandwidth adjustment (default is 1)
  )
)

# Print the model
# print(gnb_model2)
 
# Make predictions on the test set
predicted_classes2 <- predict(gnb_model2, newdata = test_x)
 
# Compute Confusion Matrix
conf_matrix_gnb2 <- confusionMatrix(predicted_classes2, test_y)
print(conf_matrix_gnb2)
 
# Convert test labels to numeric for ROC curve (0 = No, 1 = Yes)
test_y_numeric <- as.numeric(test_y) - 1  # Convert "No" to 0, "Yes" to 1
 
# Get predicted probabilities for the positive class
predicted_gnb2 <- predict(gnb_model2, newdata = test_x, type = "prob")[, "Yes"]
 
# Compute ROC curve
roc_curve_gnb2  <- roc(test_y_numeric, predicted_gnb2)

# Extract accuracy
accuracy_gnb2<- conf_matrix_gnb2$overall['Accuracy']
accuracy_gnb2

# Extract accuracy confidence interval
accuracy_ci_gnb2<- conf_matrix_gnb2$overall[c('AccuracyLower', 'AccuracyUpper')]
accuracy_ci_gnb2

# Plot ROC curve
plot(smooth(roc_curve_gnb2), main= "ROC Curve for Gaussian Naive Bayes Model (caret)") 
 
# Print AUC
auc_value_gnb2 <- auc(roc_curve_gnb2)
print(paste("AUC:", auc_value_gnb2 ))
 
# Print AUC confidence interval
ci_auc_gnb2  <- ci.auc(auc_value_gnb2 )
print(ci_auc_gnb2)




# Function to convert importance scores into percentages
percent_importance_func<- function(x){
        prop<- (x/sum(x))*100
    return(prop)

    }

# Models: Logistic, RF, GBM, KNN, XGBoost, SVM and GNB. 
# 1. Variable Importance - Random Forest
rf_importance2 <- varImp(rf_model2, scale = FALSE)
rf_imp2 <- as.data.frame(rf_importance2$importance)
rf_imp2$Feature <- rownames(rf_imp2)
colnames(rf_imp2) <- c("RF_Importance", "Feature")
rf_imp2$RF_Importance_Percent<- percent_importance_func(x=rf_imp2$RF_Importance)
 # rf_imp2

# 2. Variable Importance - GBM
gbm_importance2 <- varImp(gbm_model2, scale = FALSE)
gbm_imp2<- gbm_importance2$importance
gbm_imp2$Feature <- rownames(gbm_imp2)
gbm_imp2$Feature<- c('GA','DVcode', 'UADcode',"EFW",'deliverycode')
colnames(gbm_imp2) <- c("GBM_Importance","Feature")
gbm_imp2$GBM_Importance_Percent<- percent_importance_func(x=gbm_imp2$GBM_Importance)
  # gbm_imp2


# 3. Variable Importance - XGBoost
xgb_importance2 <- varImp(xgb_model2)
xgb_imp2 <- xgb_importance2$importance
xgb_imp2$Feature<- rownames(xgb_imp2)
colnames(xgb_imp2) <- c("XGB_Importance", "Feature")
xgb_imp2$XGB_Importance_Percent<- percent_importance_func(x=xgb_imp2$XGB_Importance)
 # xgb_imp2

# 4. Variable Importance - SVM

# Use caret's varImp function to get feature importance
svm_importance2 <- varImp(svm_model2, scale = FALSE)
svm_imp2 <- as.data.frame(svm_importance2$importance)
svm_imp2$Feature <- rownames(svm_imp2)
svm_imp2=svm_imp2[, -1]
colnames(svm_imp2) <- c("SVM_Importance", "Feature")
svm_imp2$SVM_Importance_Percent<- percent_importance_func(x=svm_imp2$SVM_Importance)
 # svm_imp2


# 5. Variable Importance - Logistic Regression
logistic_importance2<- varImp(logistic_model2, scale = FALSE)
logistic_imp2<- as.data.frame(logistic_importance2$Overall)
logistic_imp2$Feature<- rownames(varImp(logistic_model2, scale = FALSE))
colnames(logistic_imp2) <- c("Logistic_Importance", "Feature")
logistic_imp2$Logistic_Importance_Percent<- percent_importance_func(x=logistic_imp2$Logistic_Importance)
  # logistic_imp2


# 6 Variable Importance - KNN
knn_importance2 <-  varImp(knn_model2, scale = FALSE)
knn_imp2 <- as.data.frame(knn_importance2$importance)
knn_imp2$Feature <- rownames(knn_imp2)
 knn_imp2<- knn_imp2[, -2]
 colnames(knn_imp2) <- c("KNN_Importance", "Feature")
 knn_imp2$KNN_Importance_Percent<- percent_importance_func(x=knn_imp2$KNN_Importance)
 # knn_imp2


#  7. Variable Importance - GNB
gnb_importance2<- varImp(gnb_model2, scale = FALSE)
gnb_imp2<- as.data.frame(gnb_importance2$importance)
gnb_imp2$Feature<- rownames(gnb_imp2)
gnb_imp2<- gnb_imp2[, -2]
colnames(gnb_imp2) <- c("GNB_Importance", "Feature")
gnb_imp2$GNB_Importance_Percent<- percent_importance_func(x=gnb_imp2$GNB_Importance)
 # gnb_imp2


# Merge importance scores
# Assuming each importance dataframe has columns: "Feature" and "Importance"
importance_combined2 <- Reduce(function(x, y) merge(x, y, by = "Feature", all = TRUE), 
                               list(rf_imp, gbm_imp, xgb_imp, svm_imp, knn_imp,logistic_imp,gnb_imp))

# importance_combined2



# Rename columns for clarity (optional)
 colnames(importance_combined2) <- c("Feature", "RF_Importance_Percent", "GBM_Importance_Percent", "XGB_Importance_Percent",
             "SVM_Importance_Percent", "KNN_Importance_Percent","Logistic_Importance_Percent","GNB_Importance_Percent")

# Handle any NAs (if some features were not present in all models)
 importance_combined2[is.na(importance_combined2)] <- 0  # Replace NA with 0 (assuming missing means no importance)

 importance_combined2$Mean_Importance_Percent <- rowMeans(importance_combined2[, c("RF_Importance_Percent", "GBM_Importance_Percent", 
                                    "XGB_Importance_Percent", "SVM_Importance_Percent", "KNN_Importance_Percent",
                                    "Logistic_Importance_Percent","GNB_Importance_Percent")])


# Setting the importance score in descending order
rf_imp2 <- rf_imp2[order(-rf_imp2$RF_Importance_Percent), ]
gbm_imp2 <- gbm_imp2[order(-gbm_imp2$GBM_Importance_Percent), ]
xgb_imp2 <- xgb_imp2[order(-xgb_imp2$XGB_Importance_Percent), ]
svm_imp2 <- svm_imp2[order(-svm_imp2$SVM_Importance_Percent), ]
knn_imp2 <- knn_imp2[order(-knn_imp2$KNN_Importance_Percent), ]

logistic_imp2<- logistic_imp2[order(-logistic_imp2$Logistic_Importance_Percent), ]
gnb_imp2<- gnb_imp2[order(-gnb_imp2$GNB_Importance_Percent), ]
# stack_imp<- stack_imp[order(-stack_imp$Stack_Importance), ]





# Sort combined importance by Mean Importance
 importance_combined2$Mean_Importance_Percent <- rowMeans(importance_combined2[, -1], na.rm = TRUE)
 importance_combined2 <- importance_combined2[order(-importance_combined2$Mean_Importance_Percent), ]

# Sort by importance
 print(importance_combined2)

# Plot all variable importance together
par(mfrow = c(4, 2), mar=c(5,4,2,1))  # 3x2 layout, adjusted margins for better spacing

colors_model<- c("lightblue",  "pink",
               "orange", "purple","cyan", "red","brown", "lightgreen")

barplot(rf_imp2$RF_Importance, names.arg = rf_imp2$Feature, las = 1, 
        main = "a) Random Forest (RF)", col = colors_model[1], ylab = "Importance Score (%)")

barplot(gbm_imp2$GBM_Importance, names.arg = gbm_imp2$Feature, las = 1, 
        main = "b) Gradient Boosting Machine (GBM)", col = colors_model[2], ylab = "Importance Score (%)")

barplot(xgb_imp2$XGB_Importance, names.arg = xgb_imp2$Feature, las = 1, 
        main = "c) Extreme Gradient Boosting (XGBoost)", col = colors_model[3], ylab = "Importance Score (%)")

barplot(svm_imp2$SVM_Importance, names.arg = svm_imp2$Feature, las = 1, 
        main = "d) Support Vector Machine (SVM)", col = colors_model[4], ylab = "Importance Score (%)")

barplot(knn_imp2$KNN_Importance, names.arg = knn_imp2$Feature, las = 1, 
        main = "e) K-Nearest Neighbour (KNN)", col = colors_model[5], ylab = "Importance Score (%)")


 barplot(gnb_imp2$GNB_Importance, names.arg = gnb_imp2$Feature, las = 1, 
          main = "g) Gaussian Naive Bayes (GNB)", col = colors_model[6], ylab = "Importance Score (%)")


 barplot(logistic_imp2$Logistic_Importance, names.arg = logistic_imp2$Feature, las = 1, 
          main = "f) Logistic Regression (LR)", col = colors_model[7], ylab = "Importance Score (%)")

barplot(importance_combined2$Mean_Importance, names.arg = importance_combined2$Feature, las = 1,
        main = "h) Pooled Feature Importance ", col = colors_model[8], ylab = "Mean Importance Score (%)")
# barplot(stack_imp2$Stack_Importance, names.arg = stack_imp2$Feature, las = 1,
        # main = "h) Stacked ML models", col = colors_model[8], ylab = "Importance Score")

library(pROC)
options(repr.plot.width=8, repr.plot.height=8,repr.plot.res=300) #Setting plot size


# Function to safely smooth the ROC curve, only if possible
safe_smooth <- function(roc_curve) {
  tryCatch({
    smoothed_roc <- smooth(roc_curve, method = "binormal")
      
    return(smoothed_roc)
  }, error = function(e) {
     message("Smoothing failed: ", e$message)
    return(roc_curve)  # Return the original curve if smoothing fails
  })
}

# Smooth the ROC curves, if possible
roc_curve_smooth_rf2 <- safe_smooth(roc_curve_rf2)
roc_curve_smooth_gbm2 <- safe_smooth(roc_curve_gbm2)
roc_curve_smooth_xgb2 <- smooth(roc_curve_xgb2, method="density", bandwidth=.1)
roc_curve_smooth_svm2 <- safe_smooth(roc_curve_svm2)
roc_curve_smooth_knn2 <- safe_smooth(roc_curve_knn2)  # Smooth Logistic Regression ROC curve
roc_curve_smooth_logistic2 <- safe_smooth(roc_curve_logistic2)  # Smooth SVM ROC curve
roc_curve_smooth_gnb2 <- safe_smooth(roc_curve_gnb2) 

# roc_curve_smooth_ensemble2<- smooth(roc_curve_stack2, method = "density", bandwidth = 0.1)


# Calculate AUC and its 95% CI for each model
auc_rf2 <- auc(roc_curve_rf2)
auc_rf_ci2 <- ci.auc(roc_curve_rf2)

auc_knn2 <- auc(roc_curve_knn2)
auc_knn_ci2 <- ci.auc(roc_curve_knn2)

auc_gbm2 <- auc(roc_curve_gbm2)
auc_gbm_ci2 <- ci.auc(roc_curve_gbm2)

auc_xgb2 <- auc(roc_curve_xgb2)
auc_xgb_ci2 <- ci.auc(roc_curve_xgb2)

# auc_ensemble2 <- auc(roc_curve_stack2)
# auc_ensemble_ci2 <- ci.auc(roc_curve_stack2)

auc_logreg2 <- auc(roc_curve_logistic2)  # AUC for Logistic Regression
auc_logreg_ci2 <- ci.auc(roc_curve_logistic2)  # CI for Logistic Regression

auc_svm2 <- auc(roc_curve_svm2)  # AUC for SVM
auc_svm_ci2 <- ci.auc(roc_curve_svm2)  # CI for SVM

auc_gnb2 <- auc(roc_curve_gnb2)  # AUC for SVM
auc_gnb_ci2 <- ci.auc(roc_curve_gnb2)  # CI for SVM



# Plot the first ROC curve (Random Forest) with reversed x-axis

#Exporting the combined/comparative ROC curves for the secondary analysis

jpeg("roc_curve_plot_SecondaryAnalysis.jpg", width = 4300, height = 3000, res = 500)

colors_model<- c("lightblue",  "pink",
               "orange", "purple","cyan", "red","brown")

plot(roc_curve_smooth_rf2, col = colors_model[1], type = "l", main = "", 
     xlim = c(1, 0), ylim = c(0, 1), lwd = 3, 
     xlab = "Specificity", ylab = "Sensitivity", 
     lty = 1)  # Line type for smooth curve

# Add ROC curve for Gradient Boosting Machine
 lines(roc_curve_smooth_gbm2, col = colors_model[2], lwd = 3, lty = 2)  # Dashed line for GBM

# Add ROC curve for XGBoost
lines(roc_curve_smooth_xgb2, col = colors_model[3], lwd = 3, lty = 3)  # Dotted line for XGBoost


# Add ROC curve for Support Vector Machine
 lines(roc_curve_smooth_svm2, col = colors_model[4], lwd = 3, lty = 4)  # Solid line for SVM


# Add ROC curve for KNN
lines(roc_curve_smooth_knn2, col = colors_model[5], lwd = 3, lty = 5)  # Solid line for knn


# Add ROC curve for GNB
lines(roc_curve_smooth_gnb2, col = colors_model[6], lwd = 3, lty = 9)  # Solid line for knn

# Add ROC curve for Logistic Regression
lines(roc_curve_smooth_logistic2, col = colors_model[7], lwd = 3, lty = 6)  # Solid line for Logistic Regression

# Add ROC curve for Ensemble Stack
# lines(roc_curve_smooth_ensemble2, col = colors_model[8], lwd = 3, lty = 8)  # Dash-dot line for Ensemble



legend_labels<-c(paste("Random Forest (AUC = ", round(auc_rf2, 3), ", CI: ", 
     round(auc_rf_ci2[1], 3), "-", round(auc_rf_ci2[3], 3),"; % Accuracy = ", as.numeric(round(accuracy_rf2, 3)*100), ", CI: ",
      as.numeric(round(accuracy_ci_rf2[1], 3)*100),"-" ,as.numeric(round(accuracy_ci_rf2[2], 3)*100), ")", 
                       sep = ""),
paste("GBM (AUC = ", round(auc_gbm2, 3), ", CI: ", round(auc_gbm_ci2[1], 3), "-", 
      round(auc_gbm_ci2[3], 3), "; % Accuracy = ",as.numeric(round(accuracy_gbm2, 3)*100), ", CI: ",
      as.numeric(round(accuracy_ci_gbm2[1], 3)*100),"-" ,as.numeric(round(accuracy_ci_gbm2[2], 3)*100),")",
   
       sep = ""),
paste("XGBoost (AUC = ", round(auc_xgb2, 3), ", CI: ", round(auc_xgb_ci2[1], 3),
      "-", round(auc_xgb_ci2[3], 3), "; % Accuracy = ", as.numeric(round(accuracy_xgb2, 3)*100), ", CI: ",
      as.numeric(round(accuracy_ci_xgb2[1], 3)*100),"-" ,as.numeric(round(accuracy_ci_xgb2[2], 3)*100),")",
      sep = ""),
paste("SVM (AUC = ", round(auc_svm2, 3), ", CI: ", round(auc_svm_ci2[1], 3), "-", round(auc_svm_ci2[3], 3), 
      "; % Accuracy = ", as.numeric(round(accuracy_svm2, 3)*100), ", CI: ",
      as.numeric(round(accuracy_ci_svm2[1], 3)*100),"-" ,as.numeric(round(accuracy_ci_svm2[2], 3)*100),")", 
      sep = ""),
paste("KNN (AUC = ", round(auc_knn2, 3), ", CI: ", round(auc_knn_ci2[1], 3), "-", round(auc_knn_ci2[3], 3),
       "; % Accuracy = ", as.numeric(round(accuracy_knn2, 3)*100), ", CI: ",
      as.numeric(round(accuracy_ci_knn2[1], 3)*100),"-" ,as.numeric(round(accuracy_ci_knn2[2], 3)*100), ")", 
       sep = ""),
 paste("GNB (AUC = ", round(auc_gnb2, 3), ", CI: ", round(auc_gnb_ci2[1], 3), "-", round(auc_gnb_ci2[3], 3),
       "; % Accuracy = ", as.numeric(round(accuracy_gnb, 3)*100), ", CI: ",
      as.numeric(round(accuracy_ci_gnb2[1], 3)*100),"-" ,as.numeric(round(accuracy_ci_gnb2[2], 3)*100),")", 
       sep = ""), 
paste("Logistic Regression (AUC = ", round(auc_logreg2, 3), ", CI: ",
      round(auc_logreg_ci2[1], 3), "-", round(auc_logreg_ci2[3], 3), "; % Accuracy = ", as.numeric(round(accuracy_logistic2, 3)*100), ", CI: ",
      as.numeric(round(accuracy_ci_logistic2[1], 3)*100),"-" ,as.numeric(round(accuracy_ci_logistic2[2], 3)*100)
      ,")", 
      sep = "")
    # paste("Stacked ML models (AUC = ", round(auc_ensemble2, 3), ", CI: ", 
      # round(auc_ensemble_ci2[1], 3), "-", round(auc_ensemble_ci2[3], 3), "; % Accuracy = ", 
          # as.numeric(round(accuracy_stack2, 3)*100), ", CI: ",
      # as.numeric(round(accuracy_ci_stack2[1], 3)*100),"-" ,as.numeric(round(accuracy_ci_stack2[2], 3)*100),") ",
          # sep = "")     
                )



# Create the legend with AUC and CI values
legend("bottomright", 
       legend = legend_labels,
       col = colors_model, 
       lwd = 3, 
       lty = c(1, 2, 3, 4, 5, 9,6), 
       cex = 0.65)  # Adjust text size for better fit
dev.off()
