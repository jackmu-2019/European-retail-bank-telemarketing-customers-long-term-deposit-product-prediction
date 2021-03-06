#Sample the data by random shuffling
bank.full <- bank.full[sample(nrow(bank.full)),]
head(bank.full)

# Encoding the target feature as numerical data "0" & "1" for XGBoost
bank.full$Deposit <- ifelse(bank.full$Deposit %in% c("1"), 1, 0)
str(bank.full)

# Encoding categorical data
bank.full$job = as.numeric(factor(bank.full$job,
                                  levels = c("admin.","unemployed","management","housemaid","entrepreneur","student","blue-collar","self-employed","retired","technician","services","unknown"),
                                  labels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11,12)))

bank.full$marital = as.numeric(factor(bank.full$marital,
                                      levels = c("married","divorced","single","unknown"),
                                      labels = c(1, 2, 3, 4)))

bank.full$education = as.numeric(factor(bank.full$education,
                                        levels = c("basic.4y","basic.6y","basic.9y","high.school","illiterate","professional.course","university.degree","unknown"),
                                        labels = c(1, 2,3,4,5,6,7,8)))

bank.full$housing = as.numeric(factor(bank.full$housing,
                                      levels = c('yes','no','unknown'),
                                      labels = c(1, 2,3)))

bank.full$loan = as.numeric(factor(bank.full$loan,
                                   levels = c('yes','no','unknown'), 
                                   labels = c(1, 2, 3)))

bank.full$contact = as.numeric(factor(bank.full$contact,
                                      levels = c("telephone","cellular"),
                                      labels = c(2, 3)))

bank.full$month = as.numeric(factor(bank.full$month, 
                                    levels = c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"),
                                    labels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)))

bank.full$poutcome = as.numeric(factor(bank.full$poutcome,
                                       levels = c("failure","nonexistent","success"),
                                       labels = c(1, 2,3)))

bank.full$day_of_week = as.numeric(factor(bank.full$day_of_week,
                                          levels = c("mon","tue","wed","thu","fri"),
                                          labels = c(1, 2,3,4,5)))

#Check the class of each variable
str(bank.full)

# Check any missing data in full dataset
sum(is.na(bank.full))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(bank.full$Deposit, SplitRatio = 0.8)
training_set = subset(bank.full, split == TRUE)
test_set = subset(bank.full, split == FALSE)

# Check any missing data in training_set and test_set
sum(is.na(training_set))
sum(is.na(test_set))

# Check randomization of training_set and test_set
prop.table(table(training_set$Deposit))
prop.table(table(test_set$Deposit))


# Fitting XGBoost to the Training set
# install.packages('xgboost')
library(xgboost)
xgboost_classifier = xgboost(data = as.matrix(training_set[-20]), label = training_set$Deposit, nrounds = 10)

# Predicting the Test set results
xgboost_pred = predict(xgboost_classifier, newdata = as.matrix(test_set[-20]))
xgboost_pred = ifelse(xgboost_pred >= 0.5,1,0)

# Making the Confusion Matrix
cm = table(test_set[, 20], xgboost_pred)

# Test for accuracy, precision, recall, F1 score
accuracy_Test <- sum(diag(cm)) / sum(cm)
print(paste('Accuracy for test is', accuracy_Test))

precision_Test <- cm[1,1] / sum(cm[,1])
print(paste('Precision for test is', precision_Test))

recall_Test <- cm[1,1] / sum(cm[1,])
print(paste('Recall for test is', recall_Test))

F1score_Test = (2*precision_Test*recall_Test)/(precision_Test+recall_Test)
print(paste('F1 score for test is', F1score_Test ))

# Applying k-Fold Cross Validation
# install.packages('caret')
library(caret)
folds = createFolds(training_set$Deposit, k = 10)
cv = lapply(folds, function(x) {
  training_fold = training_set[-x, ]
  test_fold = training_set[x, ]
  classifier = xgboost(data = as.matrix(training_set[-20]), label = training_set$Deposit, nrounds = 10)
  y_pred = predict(classifier, newdata = as.matrix(test_fold[-20]))
  y_pred = (y_pred >= 0.5)
  cm = table(test_fold[, 20], y_pred)
  accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
  return(accuracy)
})
accuracy = mean(as.numeric(cv))
