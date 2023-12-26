library(tidyverse)
library(randomForest)
library(caret)
library(caTools)

# Import data

tear = read.delim("C:/Users/Lucas Ribeiro/Dropbox/MBA/tear.txt")

# Enter the identifier of each line, that is, the column containing the name of the genes
# as the name of each line

tear1 = tear %>% remove_rownames %>% column_to_rownames(var="Gene")

#Transpose data

df_tear = t(tear1)
df_tear = as.data.frame(df_tear)

#Creates a column containing the group each row belongs to

df_tear$groups = "CT"
df_tear[28:54, "groups"] = "PD"

# Create training and testing dataset
# Where the training set is 70%.

split = createDataPartition(df_tear$groups, p=0.7, list = FALSE)
train = df_tear[split,] # training set
test = df_tear[-split,] # test set

#make the column where the groups are as a categorical variable.

train$groups = as.factor(train$groups)
test$groups = as.factor(test$groups)

# Applying Random Forest to training data

set.seed(20230102)

classifier_RF = randomForest(x = train,
                             y = train$groups,
                             mtry = 75,
                             ntree = 3000)

classifier_RF

#Predicting training set results

pred_train = predict(classifier_RF, train)
confusionMatrix(pred_train, train$groups)

# Predicting test set results

pred_test = predict(classifier_RF, test)
confusionMatrix(pred_test, test$groups)

# Suggestion of parameters to tune algorithm

t = tuneRF(train[,-5], train[,5],
            stepFactor = 0.5,
            plot = TRUE,
            ntreeTry = 180,
            trace = TRUE,
            improve = 0.05)


# Classifier with new parameters


tuned_classifier_RF = randomForest(x = train,
                             y = train$groups,
                             mtry = 90,
                             ntree = 3000)

tuned_classifier_RF


# Prevendo os resultados do conjunto de treinamento após melhoramento

pred_train_tuned = predict(tuned_classifier_RF, train)
confusionMatrix(pred_train_tuned, train$groups)

# Predicting training set results after improvement

pred_test_tuned = predict(tuned_classifier_RF, test)
confusionMatrix(pred_test_tuned, test$groups)
pred_test_tuned

# Graphical plot of the error rate of the new classifier

error_RF = plot(tuned_classifier_RF)

#Plot the number of nodes for the decision tree

Node_tree = hist(treesize(tuned_classifier_RF),
      main = "No. of Nodes for the Trees",
      col = "green")

#Most important variables for algorithm classification

tbl_importante_RF = importance(tuned_classifier_RF)
tbl_importante_RF
head(tbl_importante_RF)

# Plot of the influence profile of each gene in the classifier

partialPlot(tuned_classifier_RF, train,SPB3, "PD")

#plot with the most important variables
#varImpPlot(classifier_RF)


######################################################################
####################################################################

#SVM 

library(caret)
library(tidyverse)		 

# Create a data training control
# using 5-fold cross-validation method

SVMtrain_control = trainControl(method = "cv", number = 5,
                                classProbs = T,
                                summaryFunction = twoClassSummary)

# Create the classifier using SVM using the training data
# using the linear method

classifier_SVM = train(groups~., data = train,
                       method = "svmLinear", 
                       trControl = SVMtrain_control,
                       preProcess = c("center","scale"),
                       metric = "ROC",
                       tuneLength = 50)
classifier_SVM
classifier_SVM$results

# Show the confusion matrix for the training data

confusionMatrix(classifier_SVM)

# Uses the trained SVM classifier to predict
# the test data

pred_SVM = predict(classifier_SVM, test)
pred_SVM

# Confusion matrix for the test set

results_SVM = confusionMatrix(data = pred_SVM, test$groups)

results_SVM

tbl_importance_SVM = varImp(classifier_SVM, scale = FALSE)
tbl_importance_SVM



######### 
#ROC curve for RF model

library(pROC)
rf.roc = roc(train$groups,classifier_RF$votes[,2], print.auc = T)
plot(rf.roc, print.auc = T, col = "blue")

# ROC curve for SVM model

roc.svm <- roc(response = test$groups, predictor =as.numeric(pred_SVM))
plot(roc.svm, add = TRUE,col = "red", print.auc=TRUE, print.auc.x = 0.5, print.auc.y = 0.3)
legend("bottomright", legend = c("Random Forest", "Support Vector Machine"), col = c("blue","red"), lty = 1)

