# MODELO 3

# Sin columnas CARRETERA y ACOND_CALZADA
# Imputación de valores perdidos. 
# Validación cruzada sobre parámetros
# XGB

source("Functions-Preprocesamiento.R")
source("Functions-Cross-validation.R")
source("Functions-Models.R")

data.tra.imp <- readRDS("Data/data.tra.imp.Rda")
data.tst.imp <- readRDS("Data/data.tst.imp.Rda")

data.tra.cat <- convertFactorIntoNumeric(data.tra.imp, c(2,4,5,6,7,13:27))
data.tst.cat <- convertFactorIntoNumeric(data.tst.imp, c(2,4,5,6,7,13:27))

folds <- readRDS("Data/folds.Rda")

cross.validation(data   = data.tra.cat[ ,-ncol(data.tra.cat)],
                 labels = data.tra.cat[ ,ncol(data.tra.cat)],
                 model.function = model.xgb,
                 folds = folds,
                 params = list(nrounds = 75))


results <- cross.validation(data   = data.tra.cat[ ,-ncol(data.tra.cat)],
                            labels = data.tra.cat[ ,ncol(data.tra.cat)],
                            model.function = model.xgb,
                            folds = folds,
                            params = expand.grid(nrounds = seq(50, 70, by = 10), 
                                                 eta = seq(0.1, 0.3, by= 0.1),
                                                 gamma = seq(0, 0.01, by = 0.005)))

model.3 <- model.xgb(data   = data.tra.cat[ ,-ncol(data.tra.cat)],
                     labels = as.numeric(data.tra.cat[ ,ncol(data.tra.cat)])-1,
                     params = c(nrounds = 50, eta = 0.3, gamma = 0, max_depth = 6))

predictions <- as.factor(predict(model.3, as.matrix(data.tst.cat)))
levels(predictions) <- c("Atropello","Colision_Obstaculo","Colision_Vehiculos",
                         "Otro","Salida_Via","Vuelco")
predictions <- data.frame(id = 1:length(predictions),
                          prediction = predictions)
write.csv(predictions, file = "Predictions/predictions.xgb.cat.vc.csv",  
          quote = F, row.names = F)
