#'
#' Imputación de valores directamente a las categorías "OTRO"
#' 

# Cargamos los datos antes de ser preprocesados (datos y particiones)
load("Data/Basic.RData")

# Cargamos las funciones 
source("Functions-Preprocesamiento.R")
source("Functions-Models.R")
source("Functions-Cross-validation.R")

# Cargamos las bibliotecas necesarias
require(dplyr)
require(mice)


#' Transformación inicial
#' - Eliminamos carretera
#' - Transformamos en numéricas las categorías de fecha y hora
#' - La variable DIASEMANA pasa a ser T si es día entre semana, F si sábado o domingo
data.tra <- data.tra %>%
   select(-CARRETERA) %>%
   transform(MES = month.names.2.numeric[MES],
             HORA = round(as.numeric(levels(HORA)[HORA])) %% 24,
             DIASEMANA = !(week.names.2.numeric[DIASEMANA] %in% c(6,7)))


#' Observación de la proporción de valores perdidos para la categoría acera según el tipo de vía. 
sapply(levels(data.tra$TIPO_VIA),
       function(l) mean(is.na(select(filter(data.tra,
                                            TIPO_VIA == l),
                                     ACERAS))))

#' Asignación a las categorías "OTRO" de los valores 
data.tra.oth <- transform(data.tra,
                          ACOND_CALZADA = as.factor(ifelse(is.na(ACOND_CALZADA), "OTRO TIPO", 
                                                           levels(ACOND_CALZADA)[ACOND_CALZADA])),
                          PRIORIDAD = as.factor(ifelse(is.na(PRIORIDAD), "NINGUNA (SOLO NORMA)", 
                                                       levels(PRIORIDAD)[PRIORIDAD])),
                          VISIBILIDAD_RESTRINGIDA = as.factor(ifelse(is.na(VISIBILIDAD_RESTRINGIDA), "OTRA_CAUSA", 
                                                                     levels(VISIBILIDAD_RESTRINGIDA)[VISIBILIDAD_RESTRINGIDA])),
                          OTRA_CIRCUNSTANCIA = as.factor(ifelse(is.na(OTRA_CIRCUNSTANCIA), "OTRA", 
                                                                levels(OTRA_CIRCUNSTANCIA)[OTRA_CIRCUNSTANCIA])),
                          MEDIDAS_ESPECIALES = as.factor(ifelse(is.na(MEDIDAS_ESPECIALES), "OTRA MEDIDA", 
                                                                levels(MEDIDAS_ESPECIALES)[MEDIDAS_ESPECIALES])),
                          ACERAS = as.factor(ifelse(is.na(ACERAS) & (TIPO_VIA == "AUTOVIA" | TIPO_VIA == "AUTOPISTA"),
                                                    "NO HAY ACERA",
                                                    levels(ACERAS)[ACERAS])))

#' Realizamos la transformación necesaria para el método en cuestión.
data.tra.oth <- data.tra.oth %>%
   convertFactorIntoNumeric(c(5:7,13:28)) %>%
   transform(TIPO_ACCIDENTE = as.numeric(TIPO_ACCIDENTE) - 1)


results <- cross.validation(data   = select(data.tra.oth, -TIPO_ACCIDENTE),
                            labels = data.tra.oth$TIPO_ACCIDENTE,
                            model.function = model.xgb,
                            folds = folds,
                            params = expand.grid(nrounds = 75))


#' Transformación de datos de test
data.tst.oth <- data.tst %>%
   select(-CARRETERA) %>%
   transform(MES = month.names.2.numeric[MES],
             HORA = round(as.numeric(levels(HORA)[HORA])) %% 24,
             DIASEMANA = !(week.names.2.numeric[DIASEMANA] %in% c(6,7))) %>%
   transform(ACOND_CALZADA = as.factor(ifelse(is.na(ACOND_CALZADA), "OTRO TIPO", 
                                              levels(ACOND_CALZADA)[ACOND_CALZADA])),
             PRIORIDAD = as.factor(ifelse(is.na(PRIORIDAD), "NINGUNA (SOLO NORMA)", 
                                          levels(PRIORIDAD)[PRIORIDAD])),
             VISIBILIDAD_RESTRINGIDA = as.factor(ifelse(is.na(VISIBILIDAD_RESTRINGIDA), "OTRA_CAUSA", 
                                                        levels(VISIBILIDAD_RESTRINGIDA)[VISIBILIDAD_RESTRINGIDA])),
             OTRA_CIRCUNSTANCIA = as.factor(ifelse(is.na(OTRA_CIRCUNSTANCIA), "OTRA", 
                                                   levels(OTRA_CIRCUNSTANCIA)[OTRA_CIRCUNSTANCIA])),
             MEDIDAS_ESPECIALES = as.factor(ifelse(is.na(MEDIDAS_ESPECIALES), "OTRA MEDIDA", 
                                                   levels(MEDIDAS_ESPECIALES)[MEDIDAS_ESPECIALES])),
             ACERAS = as.factor(ifelse(is.na(ACERAS) & (TIPO_VIA == "AUTOVIA" | TIPO_VIA == "AUTOPISTA"),
                                       "NO HAY ACERA",
                                       levels(ACERAS)[ACERAS]))) %>%
   convertFactorIntoNumeric(c(5:7,13:28))


#' Generación del modelo con los datos de entrenamiento
set.seed(3141592)
model <- model.xgb(select(data.tra.oth, -TIPO_ACCIDENTE),
                   data.tra.oth$TIPO_ACCIDENTE,
                   params = c(nrounds = 75))

#' Predicción sobre datos de test
#' Escritura de los datos
predictions <- predict(model, as.matrix(data.tst.oth))
writePredictions(predictions, "predictions.imp.others")


