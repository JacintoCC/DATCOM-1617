# Sin preprocesamiento

require(unbalanced)
require(dplyr)
require(caret)
source("Functions-Models.R")
source("Functions-Cross-validation.R")
source("Functions-Performance.R")
source("Functions-Preprocesamiento.R")


# Lectura de datos
data.tra <- read.csv("Data/pv1math-tra.csv")
data.tst <- select(read.csv("Data/pv1math-tst.csv"), -ID)


labels.tra <- as.factor(data.tra$PV1MATH)
data.tra <- select(data.tra, -PV1MATH)



# Creación de folds
set.seed(3141592)
folds <- createFolds(data.tra$PV1MATH, k = 5)


auc.measures <- list()

# SVM
auc.measures$SVM <- cross.validation(data = data.tra,
                                     labels = labels.tra,
                                     folds = folds,
                                     model.function = model.svm, 
                                     predict.function = pred.svm,
                                     params = expand.grid(kernel = c("linear", "polynomial", "sigmoid", "radial")),
                                     measure = AUC)


# RF
auc.measures$RF <- cross.validation(data = data.tra,
                                    labels = labels.tra,
                                    folds = folds,
                                    model.function = model.rf, 
                                    predict.function = pred.rf,
                                    params =  expand.grid(ntree = c(400, 500, 600)),
                                    measure = AUC)


# KNN
auc.measures$KNN <- cross.validation(data = data.tra,
                                     labels = labels.tra,
                                     folds = folds,
                                     model.function = model.knn, 
                                     predict.function = pred.knn,
                                     params = expand.grid(k = c(10, 50)),
                                     measure = AUC)

# XGB
auc.measures$XGB <- cross.validation(data = data.tra,
                                     labels = labels.tra,
                                     folds = folds,
                                     model.function = model.xgb, 
                                     predict.function = pred.xgb,
                                     params = expand.grid(nrounds = seq(40,100,by=20)),
                                     preproc.function = function(d, l){return(list(X=d, Y=l))}, 
                                     measure = AUC)

# Bagging
auc.measures$BAG <-  cross.validation(data = data.tra,
                                      labels = labels.tra,
                                      folds = folds,
                                      model.function = model.bag, 
                                      predict.function = pred.bag,
                                      params = expand.grid(mfinal = c(75,100)),
                                      measure = AUC)

print("Sin P")
print(auc.measures)


model.SVM <- model.svm(data.tra, labels.tra, params = c(kernel = "radial"))
preds.SVM <- pred.svm(model = model.SVM, test = data.tst)
writePredictions(preds.SVM, "SVM")


model.RF <- model.rf(data.tra, labels.tra, params = c(ntree = 500))
preds.RF <- pred.rf(model = model.RF, test = data.tst)
writePredictions(preds.RF, "RF")

poll <- apply(matrix(c(preds.SVM, preds.RF),ncol=2), 1, mean)
writePredictions(poll, "Poll")
