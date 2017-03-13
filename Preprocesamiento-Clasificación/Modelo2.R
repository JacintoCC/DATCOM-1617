# MODELO 2

# Sin columnas CARRETERA y ACOND_CALZADA
# Imputación de valores perdidos. 
# XGB

library(xgboost)
library(Matrix)
library(dplyr)

source("Functions-Preprocesamiento.R")
source("Functions-Cross-validation.R")

data.tra.imp <- readRDS("Data/data.tra.imp.Rda")
data.tst.imp <- readRDS("Data/data.tst.imp.Rda")

folds <- readRDS("Data/folds.Rda")


data.tra.cat <- convertFactorIntoNumeric(data.tra.imp, c(2,4,5,6,7,13:27))
data.tst.cat <- convertFactorIntoNumeric(data.tst.imp, c(2,4,5,6,7,13:27))
colnames(data.tra.cat)[ncol(data.tra.cat)] <- "TIPO_ACCIDENTE" 

matrix.xgb <- xgb.DMatrix(data = as.matrix(data.tra.cat[ ,-ncol(data.tra.cat)]),
                          label = prueba)

model.xgb.cat <- xgb.train(data = matrix.xgb,
                           num_class = 6, 
                           nrounds = 100,
                           params = list(objective = "multi:softmax"))

   
predicted.xgb.cat <- predict(model.xgb.cat, newdata = data.matrix(data.tst.cat))
predicted.xgb.cat <- data.frame(id = 1:length(predicted.xgb.cat), 
                                prediction = as.factor(predicted.xgb.cat))
levels(predicted.xgb.cat$prediction) <- c("Atropello","Colision_Obstaculo","Colision_Vehiculos",
                               "Otro","Salida_Via","Vuelco")
write.csv(predicted.xgb.cat, file = "Predictions/predictions.xgb.cat.csv",  
          quote = F, row.names = F)

