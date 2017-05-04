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
cross.validation(data = df,
                 labels = labels.tra,
                 folds = folds,
                 model.function = model.xgb, 
                 predict.function = model.xgb,
                 params = expand.grid(nrounds=50),
                 preproc.function = function(d,l){
                    ubIPF(d,l, preproc.function = function(data,labels,ub.args){
                       return(data.frame(X=data, Y=labels))
                    })
                 },
                 measure = AUC)


# Model
ub.ipf.df <- IPFub(df, labels.tra, ubNCL, 
                   ipf.args = c(consensus=T), 
                   ub.args = c(k = 5))

seeds <- c(3141592, 11570, 271, 168)
pred.SVM <- sapply(seeds, function(s){
      set.seed(s)
      model.SVM <- model.svm(ub.ipf.df$X,
                             ub.ipf.df$Y,
                             params = list("kernel"="radial",
                                           class.weights= c("0"=0.5,"1"=1.8)))
      preds.SVM <- pred.svm(model.SVM, as.data.frame(lapply(data.tst, as.numeric)))
   }) %>%
   apply(1, mean)
writePredictions(pred.SVM, "IPF.NCL.Pars")

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

