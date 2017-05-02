#' Modelo 3
#' 
#' Imputación Manual 
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
data.tra.an <- data.tra %>%
   select(-CARRETERA) %>%
   transform(MES = month.names.2.numeric[MES],
             HORA = round(as.numeric(levels(HORA)[HORA])) %% 24,
             DIASEMANA = week.names.2.numeric[DIASEMANA]) %>%
   transform(ACOND_CALZADA = as.factor(ifelse(is.na(ACOND_CALZADA) & 
                                                 TIPO_VIA == "AUTOPISTA" |
                                                 TIPO_VIA == "AUTOVIA",
                                       "OTRO TIPO",
                                       levels(ACOND_CALZADA)[ACOND_CALZADA]))) %>%
   convertFactorIntoNumeric(c(5:7,13:28)) %>%
   select(- ISLA.HIERRO) %>%
   select(- `MEDIDAS_ESPECIALES.HABILITACION ARCEN`) %>%
   apply(2, function(x) ifelse(is.na(x), F, x)) %>%
   as.data.frame()

# data.tra.imp.m <- data.tra.imp.m %>%
#    mice::mice(m=5, method="pmm") %>%
#    mice::complete()


# Datos de test
#' Para los datos de test la imputación se realiza incluyendo los datos de train ya imputados.



data.tst.an  <- data.tst %>% 
   select(-CARRETERA) %>%
   transform(MES = month.names.2.numeric[MES],
             HORA = round(as.numeric(levels(HORA)[HORA])) %% 24,
             DIASEMANA = week.names.2.numeric[DIASEMANA],
             ACOND_CALZADA = as.factor(ifelse(is.na(ACOND_CALZADA) & 
                                                 TIPO_VIA == "AUTOPISTA" |
                                                 TIPO_VIA == "AUTOVIA",
                                              "OTRO TIPO",
                                              levels(ACOND_CALZADA)[ACOND_CALZADA]))) %>%
   convertFactorIntoNumeric(c(5:7,13:28))%>%
   select(- ISLA.HIERRO) %>%
   select(- `MEDIDAS_ESPECIALES.HABILITACION ARCEN`) %>%
   apply(2, function(x) factor(ifelse(is.na(x), F, x), levels = c("FALSE","TRUE"))) %>%
   as.data.frame()


colnames(data.tst.an) <- sapply(colnames(data.tst.an), function(x) gsub(" ",".",x,fixed = T))
colnames(data.tst.an) <- sapply(colnames(data.tst.an), function(x) gsub("/",".",x,fixed = T))
colnames(data.tst.an) <- sapply(colnames(data.tst.an), function(x) gsub(",",".",x,fixed = T))
colnames(data.tst.an) <- sapply(colnames(data.tst.an), function(x) gsub("-",".",x,fixed = T))
colnames(data.tst.an) <- sapply(colnames(data.tst.an), function(x) gsub("(",".",x,fixed = T))
colnames(data.tst.an) <- sapply(colnames(data.tst.an), function(x) gsub(")",".",x,fixed = T))
colnames(data.tst.an) <- sapply(colnames(data.tst.an), function(x) gsub("+",".",x,fixed = T))
colnames(data.tst.an) <- sapply(colnames(data.tst.an), function(x) gsub(":",".",x,fixed = T))

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

results <- cross.validation(data   = select(data.tra.an, -TIPO_ACCIDENTE),
                            labels = data.tra.an$TIPO_ACCIDENTE,
                            model.function = model.svm,
                            folds = folds,
                            params = expand.grid("kernel" = c("linear", "polynomial", "radial basis", "sigmoid")))

print(results)

#' Generación del modelo con los datos de entrenamiento
set.seed(3141592)
model <- model.svm(select(data.tra.an, -TIPO_ACCIDENTE),
                   data.tra.an$TIPO_ACCIDENTE)

df <- data.frame(select(data.tra.an, -TIPO_ACCIDENTE),
                 labels = data.tra.an$TIPO_ACCIDENTE)

#' Predicción sobre datos de test
#' Escritura de los datos
predictions <- predict(model, data.tst.an, decision.values = F)
writePredictions(predictions, "predictions.svm")
