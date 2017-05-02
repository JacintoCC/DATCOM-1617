#' Imputación Manual 
#' Usando variables numéricas
#' Filtro instancias ruidosas
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
require(NoiseFiltersR)
require(caret)


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
na.by.instance <- apply(data.tra, 1, function(x) sum(is.na(x)))
data.tra.imp <- data.tra %>%
   select(-CARRETERA) %>%
   mutate(MES = month.names.2.numeric[MES],
          HORA = round(as.numeric(levels(HORA)[HORA])) %% 24,
          DIASEMANA = !(week.names.2.numeric[DIASEMANA] %in% c(6,7)),
          ACOND_CALZADA = as.factor(ifelse(is.na(ACOND_CALZADA),
                                           ifelse(TIPO_VIA == "AUTOPISTA" | TIPO_VIA == "AUTOVIA",
                                                  "OTRO TIPO",
                                                  ifelse(TIPO_VIA == "VIA CONVENCIONAL","NADA ESPECIAL", NA)),
                                           levels(ACOND_CALZADA)[ACOND_CALZADA])),
          NUMBER.NA = na.by.instance) %>%
   
   # mice::mice(m=5, method="pmm") %>%
   # mice::complete() %>%
   convertFactorIntoNumeric(c(5:7,13:28))


# Distribución de NAs
apply(data.tra, 2, function(x) mean(is.na(x)))
most.na <- data.tra[na.by.instance ==4 , ]   



# Datos de test
na.by.instance <- apply(data.tst, 1, function(x) sum(is.na(x)))
data.tst.imp <- data.tst %>% 
   select(-CARRETERA) %>%
   mutate(MES = month.names.2.numeric[MES],
          HORA = round(as.numeric(levels(HORA)[HORA])) %% 24,
          DIASEMANA = !(week.names.2.numeric[DIASEMANA] %in% c(6,7)),
          ACOND_CALZADA = as.factor(ifelse(is.na(ACOND_CALZADA),
                                           ifelse(TIPO_VIA == "AUTOPISTA" | TIPO_VIA == "AUTOVIA",
                                                  "OTRO TIPO",
                                                  ifelse(TIPO_VIA == "VIA CONVENCIONAL","NADA ESPECIAL", NA)),
                                           levels(ACOND_CALZADA)[ACOND_CALZADA])),
          NUMBER.NA = na.by.instance) %>%
   convertFactorIntoNumeric(c(5:7,13:28))
   # rbind(data.tra.imp[ ,-ncol(data.tra.imp)]) %>%
   # cbind("Is.Test" = c(rep(T, nrow(data.tst)),
   #                     rep(F, nrow(data.tra)))) %>%
   # filter(Is.Test) %>%
   # select(-Is.Test)
   # mice::mice(m=5, method="pmm") %>%
   # mice::complete() %>%


#' Experimentación mediante validación cruzada
#' XGB
#' Parámetros probados: nrounds = 75
set.seed(3141592)
print(cross.validation(data = select(data.tra.imp, -TIPO_ACCIDENTE),
                       labels = as.numeric(data.tra.imp$TIPO_ACCIDENTE)-1,
                       model.function = model.xgb,
                       folds = createFolds(data.tra.imp$TIPO_ACCIDENTE, 5),
                       params = expand.grid(nrounds = 75)))

print(results)
#' Generación del modelo con los datos de entrenamiento
set.seed(3141592)
model <- model.xgb(select(data.tra.imp, -TIPO_ACCIDENTE),
                   as.numeric(data.tra.imp$TIPO_ACCIDENTE)-1,
                   params = c(nrounds = 75))

#' Predicción sobre datos de test
#' Escritura de los datos
predictions <- predict(model, as.matrix(data.tst.imp))
writePredictions(predictions, "predictions.imp.m.numberna")
