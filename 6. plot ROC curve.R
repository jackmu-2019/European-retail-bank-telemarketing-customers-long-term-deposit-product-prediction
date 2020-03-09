# List of predictions by models
tree_pred = ifelse(tree_pred == "1", 1, 0)
forest_pred = ifelse(forest_pred == "1", 1, 0)
xgboost_pred = ifelse(xgboost_pred == "1", 1, 0)

preds_list <- list(tree_pred[,1], forest_pred[,1], xgboost_pred)

# List of actual values (same for all)
m <- length(preds_list)
actual = ifelse(test_set$Deposit== "1", 1, 0)
actuals_list <- rep(list(actual), m)

# Plot the ROC curves
library(ROCR)
pred <- prediction(preds_list, actuals_list)
rocs <- performance(pred, "tpr", "fpr")
plot(rocs, col = as.list(1:m), main = "Test Set ROC Curves")

legend(x = "bottomright", 
       legend = c("Decision Tree","Random Forest","XGBoost"),
       fill = 1:m)

# Generate the test set AUCs using the model predictions & compare
library(pROC)
tree_auc <- roc(actual, tree_pred[,1])
auc(tree_auc)

forest_auc <- roc(actual, forest_pred[,1])
auc(forest_auc)

xgboost_auc <- roc(actual, xgboost_pred)
auc(xgboost_auc)
