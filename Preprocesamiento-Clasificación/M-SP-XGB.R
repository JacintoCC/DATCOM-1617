#' Modelo 1
#' 
#' Sin preprocesamiento
#' Usando variables numéricas
#' NAs se traducen en False para todas los niveles del atributo
#' 
#' XGB

# Cargamos los datos antes de ser preprocesados (datos y particiones)
load("Data/Basic.RData")

# Cargamos las funciones 
source("Functions-Preprocesamiento.R")
source("Functions-Models.R")
source("Functions-Cross-validation.R")

# Cargamos las bibliotecas necesarias
require(dplyr)


#' Transformaciones realizadas:
#' - Descartamos la variable CARRETERA
#' - Pasamos a numérico las variables MES, HORA, DIASEMANA
#' - Pasamos a variables dummy el resto de variables, categóricas
#' 
#' 

# Datos de entrenamiento
data.tra.an <- data.tra %>%
   select(-CARRETERA) %>%
   transform(MES = month.names.2.numeric[MES],
             HORA = round(as.numeric(levels(HORA)[HORA])) %% 24,
             DIASEMANA = week.names.2.numeric[DIASEMANA],
             TIPO_ACCIDENTE = as.numeric(TIPO_ACCIDENTE) - 1) %>%
   convertFactorIntoNumeric(c(5:7,13:28))

# Datos de test
data.tst.an <- data.tst %>% 
   select(-CARRETERA) %>%
   transform(MES = month.names.2.numeric[MES],
             HORA = round(as.numeric(levels(HORA)[HORA])) %% 24,
             DIASEMANA = week.names.2.numeric[DIASEMANA]) %>%
   convertFactorIntoNumeric(c(5:7,13:28))


#' Experimentación mediante validación cruzada
#' XGB
#' Parámetros probados: nrounds = 75

results <- cross.validation(data   = data.tra.an[ ,-ncol(data.tra.an)],
                            labels = data.tra.an[ ,ncol(data.tra.an)],
                            model.function = model.xgb,
                            folds = folds,
                            params = expand.grid(nrounds = 75))

print(results)

#' Generación del modelo con los datos de entrenamiento
set.seed(3141592)
model <- model.xgb(data.tra.an[ ,-ncol(data.tra.an)],
                   data.tra.an[ ,ncol(data.tra.an)],
                   params = c(nrounds = 75))

#' Predicción sobre datos de test
#' Escritura de los datos
predictions <- predict(model, as.matrix(data.tst.an))
writePredictions(predictions, "predictions.an")
