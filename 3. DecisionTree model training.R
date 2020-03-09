# Fitting Decision Tree Classification to the Training set
library(rpart)
tree_classifier = rpart(formula = Deposit ~ .,
                   data = training_set, method = "class")

summary(tree_classifier)

# To compare the relative importance of input variables in Decision Tree Model
tree_classifier$variable.importance

library(ggplot2)
vi <- tibble(x=tree_classifier$variable.importance, attri=attr(tree_classifier$variable.importance, 'name'))
vi <- vi %>% dplyr::arrange(desc(x)) 
vi %>%
  ggplot(aes(x=reorder(attri, x), y=x, fill =x)) +
  geom_col() +
  coord_flip() +
  ggtitle("Variable Importance") +
  labs(x="Variables",y="Importance") +
  theme(plot.title = element_text(hjust=0.5)) 


# To understand the trained Decision Tree Model by Decision Tree diagram
library(rpart.plot)
rpart.plot(tree_classifier, extra = 106)

# Predicting the Test set results
tree_pred = predict(tree_classifier, newdata = test_set[-20], type = 'class')
tree_pred = predict(tree_classifier, newdata = test_set[-20], type = 'prob')

# Making the Confusion Matrix
cm = table(test_set[, 20], tree_pred)

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
tree_classifier = train(form = Deposit ~ ., data = training_set, method = 'rpart')
tree_classifier
tree_classifier$bestTune
print(tree_classifier$bestTune)

# Use the best-tuned parameter to fit back into model and plot
tree_classifier = rpart(formula = Deposit ~ .,
                        data = training_set, method = "class",
                        control = rpart.control(cp = 0.05605578))

summary(tree_classifier)

rpart.plot(tree_classifier, extra = 106)

# Predicting the Test set results
tree_pred = predict(tree_classifier, newdata = test_set[-20], type = 'class')

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