#' Imputación Manual 
#' Usando variables numéricas
#' Filtro instancias ruidosas
#' 
#' XGB


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

mean.tipo.acc <- function(x){sapply(levels(data.tra.imp$TIPO_ACCIDENTE), function(l) mean(l==x))}

# Cargamos los datos antes de ser preprocesados (datos y particiones)
load("Data/Basic.RData")
# Datos de entrenamiento
data.tra.imp <- data.tra %>%
   select(-CARRETERA) %>%
   mutate(MES = month.names.2.numeric[MES],
          HORA = round(as.numeric(levels(HORA)[HORA])) %% 24,
          DIASEMANA = !(week.names.2.numeric[DIASEMANA] %in% c(6,7)),
          # ACOND_CALZADA = as.factor(ifelse(is.na(ACOND_CALZADA),
          #                                  ifelse(TIPO_VIA == "AUTOPISTA" | TIPO_VIA == "AUTOVIA", "OTRO TIPO",
          #                                         ifelse(TIPO_VIA == "VIA CONVENCIONAL","NADA ESPECIAL", NA)),
          #                                  levels(ACOND_CALZADA)[ACOND_CALZADA])),
          NUMBER.NA = apply(., 1,  function(x) sum(is.na(x))))

# NAs antes de seguir tocando
na <- data.frame(apply(data.tra.imp, 2, function(x) ifelse(rep(anyNA(x),nrow(data.tra.imp)), is.na(x), x)))
apply(data.tra.imp, 2, function(x) mean(is.na(x)))
apply(filter(na, NUMBER.NA==1), 2, function(x) mean(x=="TRUE"))







apply(data.tra.imp, 2, function(x){
   sapply(levels(as.factor(x)), function(l) mean(x==l, na.rm = T))
}) -> dist.priori
apply(data.tra.imp, 2, function(x){
   sapply(levels(as.factor(x)), 
          function(l){
             sum(data.tra.imp$TIPO_ACCIDENTE == "Salida_Via" & x==l, na.rm = T)/
                sum(data.tra.imp$TIPO_ACCIDENTE == "Salida_Via" )
          })
}) -> dist.condic
apply(data.tra.imp, 2, function(x){
   sapply(levels(as.factor(x)), 
          function(l){
             sum(data.tra.imp$TIPO_VIA == "VIA CONVENCIONAL" & x==l, na.rm = T)/
                sum(data.tra.imp$TIPO_VIA == "VIA CONVENCIONAL")
          })
}) -> dist.condic2

sapply(1:length(dist.condic), function(i){
    dist.condic[[i]] - dist.priori[[i]]
}) -> dist.diff
print(dist.diff)

sapply(1:length(dist.condic), function(i){
   dist.condic2[[i]] - dist.priori[[i]]
}) -> dist.diff2
print(dist.diff2)


apply(data.tra.imp, 2, function(x){
   sapply(levels(as.factor(x)), function(l) mean(x==l, na.rm = T))
})


sapply(1:ncol(data.tra.imp), function(i) levels(as.factor(data.tra.imp[ ,i]))[w.m[i]])


data.tra.imp <- data.tra.imp %>%
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

results <- cross.validation(data   = select(data.tra.imp, -TIPO_ACCIDENTE),
                            labels = as.numeric(data.tra.imp$TIPO_ACCIDENTE)-1,
                            model.function = model.xgb,
                            folds = createFolds(data.tra.imp$TIPO_ACCIDENTE, 5),
                            params = expand.grid(nrounds = 75))

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
