# Subida 2
# 29/03 - Filtro de ruido


#' Carga de datos de entrenamiento y test, particiones para CV
#'  - Las variables categóricas tienen todos los valores posibles.
load("Data/Basic.RData")

# Cargamos las funciones 
source("Functions-Preprocesamiento.R")
source("Functions-Models.R")
source("Functions-Cross-validation.R")

#' - Descartamos variable de Carretera
#' - Mes a dato de tipo numérico
#' - Hora a tipo numérico y redondeado a las horas exactas
#' - Día Semana según si es Fin de semana o no
#' - Acondicionamiento de calzada
#'   - TIPO VIA es Autopista, Autovía -> Otro tipo
#' - Las variables categóricas son transformadas a tipo dummy
data.tra <- data.tra %>%
   select(-CARRETERA) %>%
   mutate(MES = month.names.2.numeric[MES],
          HORA = round(as.numeric(levels(HORA)[HORA])) %% 24,
          DIASEMANA = !(week.names.2.numeric[DIASEMANA] %in% c(6,7)),
          ACOND_CALZADA = as.factor(ifelse(is.na(ACOND_CALZADA) & 
                                              TIPO_VIA == "AUTOPISTA" |
                                              TIPO_VIA == "AUTOVIA",
                                           "OTRO TIPO",
                                           levels(ACOND_CALZADA)[ACOND_CALZADA]))) %>%
   mice::mice(m=5, method="pmm") %>%
   mice::complete() %>%
   convertFactorIntoNumeric(c(5:7,13:28))

# Filtro de ruido
ipf <- IPF(formula = TIPO_ACCIDENTE ~ .,
           data = data.tra.imp)
clean <- ipf$cleanData

# Transformación del test
data.tst <- data.tst %>% 
   select(-CARRETERA) %>%
   mutate(MES = month.names.2.numeric[MES],
          HORA = round(as.numeric(levels(HORA)[HORA])) %% 24,
          DIASEMANA = !(week.names.2.numeric[DIASEMANA] %in% c(6,7)),
          ACOND_CALZADA = as.factor(ifelse(is.na(ACOND_CALZADA) & 
                                              TIPO_VIA == "AUTOPISTA" |
                                              TIPO_VIA == "AUTOVIA",
                                           "OTRO TIPO",
                                           levels(ACOND_CALZADA)[ACOND_CALZADA]))) %>%
   convertFactorIntoNumeric(c(5:7,13:28))


#' Generación del modelo con los datos de entrenamiento
set.seed(3141592)
model <- model.xgb(select(clean, -TIPO_ACCIDENTE),
                   as.numeric(clean$TIPO_ACCIDENTE)-1,
                   params = c(nrounds = 75))


#' Predicción sobre datos de test
#' Escritura de los datos
predictions <- predict(model, as.matrix(data.tst.imp))
writePredictions(predictions, "31-03-Imputacion")