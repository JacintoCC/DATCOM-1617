# Oversampling
# Sobremuestreo

require(unbalanced)
require(dplyr)
require(caret)
require(NoiseFiltersR)
source("Functions-Models.R")
source("Functions-Cross-validation.R")
source("Functions-Performance.R")
source("Functions-Preprocesamiento.R")

# Guardaremos los resultados obtenidos en esta lista
auc.measures <- list()


# Lectura de datos
data.tra <- read.csv("Data/pv1math-tra.csv")
data.tra <- transform(data.tra,
                      PV1MATH = as.factor(PV1MATH))
data.tst <- select(read.csv("Data/pv1math-tst.csv"), -ID)

# Separamos en datos y etiquetas
labels.tra <- data.tra$PV1MATH
data.tra <- select(data.tra, -PV1MATH)


# Creación de folds
set.seed(3141592)
folds <- createFolds(labels.tra, k = 2)

# Preprocesamiento básico de los datos
#' Considerearemos las variables enteras como categóricas
data.tra <- data.tra %>%
   transform(ST04Q01 = as.numeric(ST04Q01==1), 
             ST15Q01 = factor(ST15Q01, 
                              levels = unique(ST15Q01)[order(unique(ST15Q01))], 
                              ordered = T),
             ST19Q01 = factor(ST19Q01, 
                              levels = unique(ST19Q01)[order(unique(ST19Q01))], 
                              ordered = T),
             misced = factor(misced, 
                              levels = unique(misced)[order(unique(misced))], 
                              ordered = T),
             fisced = factor(fisced, 
                              levels = unique(fisced)[order(unique(fisced))], 
                              ordered = T),
             FAMSTRUC = factor(FAMSTRUC, 
                              levels = unique(FAMSTRUC)[order(unique(FAMSTRUC))], 
                              ordered = T),
             CLCUSE1 = factor(CLCUSE1, 
                               levels = unique(CLCUSE1)[order(unique(CLCUSE1))], 
                               ordered = T))


IPFub <- function(data, labels, preproc.function, ...){
   ub.data <- preproc.function(data,
                               labels)
   df <- data.frame(ub.data$X, PV1MATH = ub.data$Y)
   out <- IPF(PV1MATH ~ ., df, ...)
   return(list(X=select(out$cleanData, -PV1MATH),
               Y=out$cleanData$PV1MATH))
}
# SVM
set.seed(3141592)
cross.validation(data = data.tra,
                 labels = labels.tra,
                 folds = folds,
                 model.function = model.svm, 
                 predict.function = pred.svm,
                 params = expand.grid(kernel = c("radial")),
                 preproc.function = function(d,l) IPFub(d,l,ubOver),
                 measure = AUC)
set.seed(3141592)

cross.validation(data = data.tra,
                 labels = labels.tra,
                 folds = folds,
                 model.function = model.svm, 
                 predict.function = pred.svm,
                 params = expand.grid(kernel = c("radial")),
                 preproc.function = function(d,l) IPFub(d,l,SMOTE),
                 measure = AUC)

# Transformación de test
data.tst <- data.tst %>%
   transform(ST04Q01 = as.numeric(ST04Q01==1)) %>%
   mutate_each(funs(. / length(unique(.))), 
               c(ST15Q01, ST19Q01, misced,
                 fisced, CLCUSE1, FAMSTRUC))

smoted.data <- SMOTE(data = data.tra, labels.tra,proportion = 1,k = 5)
smoted.df <- data.frame(smoted.data$X, PV1MATH = smoted.data$Y)
smoted.df <- as.data.frame(lapply(smoted.df, as.numeric))
smoted.df <- transform(smoted.df, 
                       PV1MATH = factor(PV1MATH - 1, levels = c(0,1)))
prepr <- IPF(PV1MATH ~ ., smoted.df)

set.seed(3141592)
model.SVM <- model.svm(select(prepr$cleanData, -PV1MATH), 
                       prepr$cleanData$PV1MATH, 
                       params = c(kernel = "radial"))
   
preds.SVM <- pred.svm(model = model.SVM, test = data.tst)
writePredictions(preds.SVM, "SMOTE.Clean")

## POLL



poll.predictions <- data.frame("Best" = read.csv("Predictions/SVM.Clean.csv",
                                                 row.names = "id"), 
                               "RF.Imp"  = read.csv("Predictions/SVM.Clean.NCL.csv",
                                                    row.names = "id"), 
                               "Clean" = read.csv("Predictions/SMOTE.Clean.csv",
                                                  row.names = "id"))

predictions <- apply(poll.predictions, 1, mean)

predictions <- data.frame(id = 1:nrow(data.tst),
                          prediction = predictions)
write.csv(predictions, file = "Predictions/predictions.poll.csv",  
          quote = F, row.names = F)
   
