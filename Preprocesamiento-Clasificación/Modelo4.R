# MODELO 4

# Sin columnas CARRETERA y ACOND_CALZADA
# Imputación de valores perdidos. 
# KNN

source("Functions-Preprocesamiento.R")
source("Functions-Cross-validation.R")
source("Functions-Models.R")

data.tra.cat <- readRDS("Data/data.tra.cat.Rda")
data.tst.cat <- readRDS("Data/data.tst.cat.Rda")

folds <- readRDS("Data/folds.Rda")

require(kknn)

model <- kknn(TIPO_ACCIDENTE ~ ., data.tra.cat, data.tst.cat)

predictions <- data.frame(id = 1:nrow(data.tst.cat),
                          prediction = predict(model))
write.csv(predictions, file = "Predictions/predictions.knn.csv",  
          quote = F, row.names = F)
