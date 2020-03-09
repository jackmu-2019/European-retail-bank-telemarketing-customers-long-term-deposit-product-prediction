# Fitting Random Forest Classification to the Training set
# install.packages('randomForest')
library(randomForest)
set.seed(123)
forest_classifier = randomForest(x = training_set[-20],
                          y = training_set$Deposit,
                          ntree = 500)

summary(forest_classifier)

# Predicting the Test set results
forest_pred = predict(forest_classifier, newdata = test_set[-20])
forest_pred = predict(forest_classifier, newdata = test_set[-20], type = 'prob')

# Making the Confusion Matrix
cm = table(test_set[, 20], forest_pred)

# Test for accuracy, precision, recall, F1 score
accuracy_Test <- sum(diag(cm)) / sum(cm)
print(paste('Accuracy for test is', accuracy_Test))

precision_Test <- cm[1,1] / sum(cm[,1])
print(paste('Precision for test is', precision_Test))

recall_Test <- cm[1,1] / sum(cm[1,])
print(paste('Recall for test is', recall_Test))

F1score_Test = (2*precision_Test*recall_Test)/(precision_Test+recall_Test)
print(paste('F1 score for test is', F1score_Test ))

# Applying Grid Search to find the best parameters
# install.packages('caret')
library(caret)
classifier = train(form = Deposit ~ ., data = training_set, method = 'rf')
classifier
classifier$bestTune
print(classifier$bestTune)

# Use the best-tuned parameter to fit back into model
forest_classifier = rpart(formula = Deposit ~ .,
                          data = training_set, method = "class",
                          control = rpart.control(cp = 0.05424126))

summary(forest_classifier)

# Predicting the Test set results
forest_pred = predict(forest_classifier, newdata = test_set[-20], type = 'class')

# Making the Confusion Matrix
cm = table(test_set[, 20], y_pred)

# Test for accuracy, precision, recall, F1 score
accuracy_Test <- sum(diag(cm)) / sum(cm)
print(paste('Accuracy for test is', accuracy_Test))

precision_Test <- cm[1,1] / sum(cm[,1])
print(paste('Precision for test is', precision_Test))

recall_Test <- cm[1,1] / sum(cm[1,])
print(paste('Recall for test is', recall_Test))

F1score_Test = (2*precision_Test*recall_Test)/(precision_Test+recall_Test)
print(paste('F1 score for test is', F1score_Test ))