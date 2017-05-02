#' Modelo 1
#' 
#' Imputación usando MICE
#' Usando variables numéricas
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
require(mice)


#' Transformaciones realizadas:
#' - Descartamos la variable CARRETERA
#' - Pasamos a numérico las variables MES, HORA, DIASEMANA
#' - Buscamos con el paquete mice los valores perdidos
#' - Completamos los valores perdidos
#' 
#' Finalmente para usar XGB:
#' - Convertimos las variables categóricas a variables dummies.
#' - Convertimos la variable de salida a tipo numérico.

# Datos de entrenamiento
data.tra.imp <- data.tra %>%
   select(-CARRETERA) %>%
   transform(MES = month.names.2.numeric[MES],
             HORA = round(as.numeric(levels(HORA)[HORA])) %% 24,
             DIASEMANA = week.names.2.numeric[DIASEMANA]) %>%
   mice::mice(m=5, method="pmm") %>%
   mice::complete()

data.tra.imp.an <- data.tra.imp %>%
   convertFactorIntoNumeric(c(5:7,13:28)) %>%
   transform(TIPO_ACCIDENTE = as.numeric(TIPO_ACCIDENTE) - 1)


# Datos de test
#' Para los datos de test la imputación se realiza incluyendo los datos de train ya imputados.
data.tst.imp <- data.tst %>% 
   select(-CARRETERA) %>%
   transform(MES = month.names.2.numeric[MES],
             HORA = round(as.numeric(levels(HORA)[HORA])) %% 24,
             DIASEMANA = week.names.2.numeric[DIASEMANA]) %>%
   rbind(data.tra.imp[ ,-ncol(data.tra.imp)]) %>%
   cbind("Is.Test" = c(rep(T, nrow(data.tst)),
                       rep(F, nrow(data.tra)))) %>%
   mice::mice(m=5, method="pmm") %>%
   mice::complete() %>%
   filter(Is.Test) %>%
   select(-Is.Test)
   
data.tst.imp.an <- data.tst.imp %>%
   convertFactorIntoNumeric(c(5:7,13:28))


#' Experimentación mediante validación cruzada
#' XGB
#' Parámetros probados: nrounds = 75

results <- cross.validation(data   = select(data.tra.imp.an, -TIPO_ACCIDENTE),
                            labels = data.tra.imp.an$TIPO_ACCIDENTE,
                            model.function = model.xgb,
                            folds = folds,
                            params = expand.grid(nrounds = 75))

print(results)

#' Generación del modelo con los datos de entrenamiento
set.seed(3141592)
model <- model.xgb(select(data.tra.imp.an, -TIPO_ACCIDENTE),
                   data.tra.imp.an$TIPO_ACCIDENTE,
                   params = c(nrounds = 75))

#' Predicción sobre datos de test
#' Escritura de los datos
predictions <- predict(model, as.matrix(data.tst.imp.an))
writePredictions(predictions, "predictions.imp.an")
