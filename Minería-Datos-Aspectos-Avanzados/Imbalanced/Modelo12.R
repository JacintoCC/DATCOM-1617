# Filtrado
# SMOTE programado

require(unbalanced)
require(dplyr)
require(caret)
require(NoiseFiltersR)
require(e1071)
require(parallelSVM)
source("Functions-Models.R")
source("Functions-Cross-validation.R")
source("Functions-Performance.R")
source("Functions-Preprocesamiento.R")
source("SMOTE.R")

# Guardaremos los resultados obtenidos en esta lista
auc.measures <- list()


# Lectura de datos
load("Data/Data.RData")


# SVM
set.seed(3141592)
df <- as.data.frame(lapply(data.tra, as.numeric))
cross.validation(data = data.tra,
                 labels = labels.tra,
                 folds = folds,
                 model.function = model.svm, 
                 predict.function = pred.svm,
                 params = expand.grid(kernel = c("radial"),
                                      class.weights = list(c("0" = 1, "1" = 1),
                                                           c("0" = 0.8, "1" = 1.2),
                                                           c("0" = 0.5, "1" = 1.8))),
                 preproc.function = function(d,l) 
                    IPFub(d,l, ubNCL,
                          ub.args = list(n)
                          ipf.args = list(p = 0.05)),
                 measure = AUC)


   # Model
df <- IPFub(data.tra, labels.tra, SMOTE)

seeds <- c(3141592, 5701, 1618, 42590, 992693)
sapply(seeds, function(s){
   set.seed(s)
   ub.ipf.df <- IPFub(df, labels.tra, ubNCL, ipf.args = list("consensus" = T, 
                                                             p = 0.05))
   model.SVM <- model.svm(ub.ipf.df$X,
                          ub.ipf.df$Y,
                          params = list("kernel"="radial",
                                        class.weights= c("0"=0.8,"1"=1.2)))
   preds.SVM <- pred.svm(model.SVM, as.data.frame(lapply(data.tst, as.numeric)))
}) %>% apply(1, mean) -> preds.SVM
   
writePredictions(preds.SVM, "IPF.NCL.Cons.P")

## POLL

poll.predictions <- data.frame("Best" = read.csv("Predictions/SVM.Clean.csv",
                                                 row.names = "id"), 
                               "RF.Imp"  = read.csv("Predictions/SVM.Clean.NCL.csv",
                                                    row.names = "id"), 
                               "A" = read.csv("Predictions/SVM.Clean.OSS.Cons.csv",
                                              row.names = "id"))

predictions <- apply(poll.predictions, 1, mean)

predictions <- data.frame(id = 1:nrow(data.tst),
                          prediction = predictions)
write.csv(predictions, file = "Predictions/predictions.poll.csv",  
          quote = F, row.names = F)
   
