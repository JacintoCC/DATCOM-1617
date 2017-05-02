# Subida 3
# 30/03 - Numero NA


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
#' - Añadimos el número de variables NA
#' - Las variables categóricas son transformadas a tipo dummy
data.tra <- data.tra %>%
   select(-CARRETERA) %>%
   mutate(MES = month.names.2.numeric[MES],
          HORA = round(as.numeric(levels(HORA)[HORA])) %% 24,
          DIASEMANA = !(week.names.2.numeric[DIASEMANA] %in% c(6,7)),
          NUMBER.NA = apply(., 1,  function(x) sum(is.na(x))))%>%
   mice::mice(m=5, method="pmm") %>%
   mice::complete()

data.tst <- data.tst %>% 
   select(-CARRETERA) %>%
   mutate(MES = month.names.2.numeric[MES],
          HORA = round(as.numeric(levels(HORA)[HORA])) %% 24,
          DIASEMANA = !(week.names.2.numeric[DIASEMANA] %in% c(6,7)),
          NUMBER.NA = apply(., 1,  function(x) sum(is.na(x)))) %>%
   convertFactorIntoNumeric(c(5:7,13:28)) %>%
   rbind(data.tra[ ,-ncol(data.tra)]) %>%
   cbind("Is.Test" = c(rep(T, nrow(data.tst)),
                       rep(F, nrow(data.tra)))) %>%
   mice::mice(m=5, method="pmm") %>%
   mice::complete() %>%
   filter(Is.Test) %>%
   select(-Is.Test) %>%
   convertFactorIntoNumeric(c(5:7,13:28))


data.tra <- data.tra %>%
   convertFactorIntoNumeric(c(5:7,13:28))


set.seed(3141592)
model <- model.xgb(select(data.tra, -TIPO_ACCIDENTE),
                   as.numeric(data.tra$TIPO_ACCIDENTE)-1,
                   params = c(nrounds = 75, eta = 0.3))

#' Predicción sobre datos de test
#' Escritura de los datos
predictions <- predict(model, as.matrix(data.tst.imp))
writePredictions(predictions, "30-03-NumeroNA")