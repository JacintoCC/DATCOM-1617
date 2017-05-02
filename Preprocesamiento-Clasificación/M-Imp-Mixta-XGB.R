#' Modelo 3
#' 
#' Imputación Mixta
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
#' - Imputamos los valores perdidos de ACOND_CALZADA según TIPO_VIA
#' - 
#' 
#' Finalmente para usar XGB:
#' - Convertimos las variables categóricas a variables dummies.
#' - Convertimos la variable de salida a tipo numérico.

# Datos de entrenamiento
data.tra.imp <- data.tra %>%
   select(-CARRETERA) %>%
   transform(MES = month.names.2.numeric[MES],
             HORA = round(as.numeric(levels(HORA)[HORA])) %% 24,
             DIASEMANA = week.names.2.numeric[DIASEMANA],
             ACOND_CALZADA = as.factor(ifelse(is.na(ACOND_CALZADA) & 
                                                 TIPO_VIA == "AUTOPISTA" |
                                                 TIPO_VIA == "AUTOVIA",
                                              "OTRO TIPO",
                                              levels(ACOND_CALZADA)[ACOND_CALZADA])),
             TIPO_ACCIDENTE = as.numeric(TIPO_ACCIDENTE)-1) %>%
   convertFactorIntoNumeric(c(5:7,13:28)) %>%
   apply(2, function(x) ifelse(is.na(x), "FALSE", x))
   # mice::mice(m=1, method="pmm") %>%
   # mice::complete() 



# Datos de test
#' Para los datos de test la imputación se realiza incluyendo los datos de train ya imputados.

data.tst.imp <- data.tst %>% 
   select(-CARRETERA) %>%
   transform(MES = month.names.2.numeric[MES],
             HORA = round(as.numeric(levels(HORA)[HORA])) %% 24,
             DIASEMANA = week.names.2.numeric[DIASEMANA],
             ACOND_CALZADA = as.factor(ifelse(is.na(ACOND_CALZADA) & 
                                                 TIPO_VIA == "AUTOPISTA" |
                                                 TIPO_VIA == "AUTOVIA",
                                              "OTRO TIPO",
                                              levels(ACOND_CALZADA)[ACOND_CALZADA]))) %>%
   convertFactorIntoNumeric(c(5:7,13:28)) %>%
   apply(2, function(x) ifelse(is.na(x), "FALSE", x))

#' Experimentación mediante validación cruzada
#' XGB
#' Parámetros probados: nrounds = 75

results <- cross.validation(data   = select(data.tra.an, -TIPO_ACCIDENTE),
                            labels = data.tra.an$TIPO_ACCIDENTE,
                            model.function = model.xgb,
                            folds = folds,
                            params = expand.grid(nrounds = 75))

print(results)

#' Generación del modelo con los datos de entrenamiento
set.seed(3141592)
model <- model.xgb(select(data.tra.an, -TIPO_ACCIDENTE),
                   data.tra.an$TIPO_ACCIDENTE,
                   params = c(nrounds = 75))

#' Predicción sobre datos de test
#' Escritura de los datos
predictions <- predict(model, as.matrix(data.tst.an))
writePredictions(predictions, "predictions.imp.F")
