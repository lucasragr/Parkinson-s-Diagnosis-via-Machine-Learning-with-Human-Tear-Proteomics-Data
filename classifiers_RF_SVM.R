library(tidyverse)
library(randomForest)
library(caret)
library(caTools)

# Importa dados

tear = read.delim("C:/Users/Lucas Ribeiro/Dropbox/MBA/tear.txt")

# Coloca o identificador de cada linha, ou seja, a coluna contendo o nome dos genes  
# como o nome de cada linha

tear1 = tear %>% remove_rownames %>% column_to_rownames(var="Gene")

#Transpõe os dados 

df_tear = t(tear1)
df_tear = as.data.frame(df_tear)

#Cria uma coluna contendo o grupo ao qual cada linha pertence

df_tear$groups = "CT"
df_tear[28:54, "groups"] = "PD"

# Fazer o conjunto de dados de treinamento e de teste
# Onde o o conjunto de treinamento é de 70%. 

split = createDataPartition(df_tear$groups, p=0.7, list = FALSE)
train = df_tear[split,] # dados de treinamento
test = df_tear[-split,] # dados de teste

#tornar a coluna onde tem os grupos como variável do tipo categórica.

train$groups = as.factor(train$groups)
test$groups = as.factor(test$groups)

# Aplicando o Random Forest ao dados de treinamento

set.seed(20230102)

classifier_RF = randomForest(x = train,
                             y = train$groups,
                             mtry = 75,
                             ntree = 3000)

classifier_RF

# Prevendo os resultados do conjunto de treinamento

pred_train = predict(classifier_RF, train)
confusionMatrix(pred_train, train$groups)

# Prevendo os resultados do conjunto de test

pred_test = predict(classifier_RF, test)
confusionMatrix(pred_test, test$groups)

# Sugestão de parametros para melhoramento do algoritmo 

t = tuneRF(train[,-5], train[,5],
            stepFactor = 0.5,
            plot = TRUE,
            ntreeTry = 180,
            trace = TRUE,
            improve = 0.05)


# Classificador com novos parâmetros 

tuned_classifier_RF = randomForest(x = train,
                             y = train$groups,
                             mtry = 90,
                             ntree = 3000)

tuned_classifier_RF


# Prevendo os resultados do conjunto de treinamento após melhoramento

pred_train_tuned = predict(tuned_classifier_RF, train)
confusionMatrix(pred_train_tuned, train$groups)

# Prevendo os resultados do conjunto de test após melhoramento

pred_test_tuned = predict(tuned_classifier_RF, test)
confusionMatrix(pred_test_tuned, test$groups)
pred_test_tuned

# Plotagem gráfica da taxa de erro do novo classificador

error_RF = plot(tuned_classifier_RF)

# Plotagem gráfica do número de nós para a árvore de decisão

Node_tree = hist(treesize(tuned_classifier_RF),
      main = "No. of Nodes for the Trees",
      col = "green")

#Variáveis mais importantes para a classificação do algoritmo

tbl_importante_RF = importance(tuned_classifier_RF)
tbl_importante_RF
head(tbl_importante_RF)

# Plotagem do perfil de influência de cada gene no classificador

partialPlot(tuned_classifier_RF, train,SPB3, "PD")

#plot com as variaveis mais importantes 
#varImpPlot(classifier_RF)

######################################################################
####################################################################

#SVM 

library(caret)
library(tidyverse)		 

# Cria um controle de treinamento dos dados 
# utilizando método de validação cruzada de 5 vezes

SVMtrain_control = trainControl(method = "cv", number = 5,
                                classProbs = T,
                                summaryFunction = twoClassSummary)

# Cria o classificador utilizando SVM utilizando os dados de treinamento
# utilizando o método linear

classifier_SVM = train(groups~., data = train,
                       method = "svmLinear", 
                       trControl = SVMtrain_control,
                       preProcess = c("center","scale"),
                       metric = "ROC",
                       tuneLength = 50)
classifier_SVM
classifier_SVM$results

# Mostra a matriz de confusão para os dados de treinamento

confusionMatrix(classifier_SVM)


# Usa o classificador SVM treinado para predizer 
# os dados de teste

pred_SVM = predict(classifier_SVM, test)
pred_SVM

# Matriz de confusão para o conjunto de teste

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

