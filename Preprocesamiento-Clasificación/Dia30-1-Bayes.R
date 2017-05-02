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

mean.tipo.acc <- function(x){sapply(levels(data.tra$TIPO_ACCIDENTE), function(l) mean(l==x))}

# Cargamos los datos antes de ser preprocesados (datos y particiones)
load("Data/Basic.RData")
# Datos de entrenamiento
data.tra <- data.tra %>%
   select(-CARRETERA) %>%
   mutate(MES = month.names.2.numeric[MES],
          HORA = round(as.numeric(levels(HORA)[HORA])) %% 24,
          DIASEMANA = !(week.names.2.numeric[DIASEMANA] %in% c(6,7)),
          ACOND_CALZADA = as.factor(ifelse(is.na(ACOND_CALZADA),
            ifelse(TIPO_VIA %in% c("AUTOPISTA", "AUTOVIA", "VIA CONVENCIONAL"),
                   "OTRO TIPO",
                   ifelse(TIPO_VIA == "OTRO TIPO", "NADA ESPECIAL", NA)),
            levels(ACOND_CALZADA)[ACOND_CALZADA])),
          NUMBER.NA = apply(., 1,  function(x) sum(is.na(x))))%>%
   convertFactorIntoNumeric(c(5:7,13:28))

# NAs antes de seguir tocando
data.na <- list()
data.na$df <- data.frame(apply(data.tra, 2, function(x) ifelse(rep(anyNA(x),nrow(data.tra)), is.na(x), x)))
data.na$dist <- apply(data.tra, 2, function(x) mean(is.na(x)))

# Dist para instancias con un valor
rows.1.NA <- apply(filter(data.na$df, NUMBER.NA==1), 2, function(x) mean(x=="TRUE")) 
Filter(function(x) x > 0,
       rows.1.NA)


# Distribución a priori de los parámetros por variable
dist.priori <-  apply(data.tra, 2, function(x){
   sapply(levels(as.factor(x)), function(l) mean(x==l, na.rm = T))
})
# Marginalización 1
dist.condicionada.1 <- apply(data.tra, 2, function(x){
   sapply(levels(as.factor(x)), 
          function(l){
             sum(data.tra$TIPO_ACCIDENTE == "Salida_Via" & x==l, na.rm = T) /
                sum(data.tra$TIPO_ACCIDENTE == "Salida_Via", na.rm = T)
          })
})
dist.diff.1 <- sapply(1:length(dist.condicionada.1), function(i){
   dist.condicionada.1[[i]] - dist.priori[[i]]
})
dist.diff.1 <- filter(data.frame(Name.var = colnames(data.tra),
                          Name.value = sapply(dist.diff.1, function(x) names(x)[which.max(x)]),
                          diff = sapply(dist.diff.1, max)), diff > 0.25)
dist.diff.1[which.max(dist.diff.1$diff[-c(29,30)]), ]




# Dist para instancias con dos valores perdidos
rows.2.NA <- apply(filter(data.na$df, NUMBER.NA==2), 2, function(x) mean(x=="TRUE")) 
Filter(function(x) x > 0,
       rows.2.NA)


# Distribución a priori de los parámetros por variable
dist.priori <-  apply(data.tra, 2, function(x){
   sapply(levels(as.factor(x)), function(l) mean(x==l, na.rm = T))
})
# Marginalización 1
dist.condicionada.1 <- apply(data.tra, 2, function(x){
   sapply(levels(as.factor(x)), 
          function(l){
             sum(data.tra$TIPO_ACCIDENTE == "Atropello" & x==l, na.rm = T) /
                sum(data.tra$TIPO_ACCIDENTE == "Atropello", na.rm = T)
          })
})
dist.diff.1 <- sapply(1:length(dist.condicionada.1), function(i){
   dist.condicionada.1[[i]] - dist.priori[[i]]
})
dist.diff.1 <- filter(data.frame(Name.var = colnames(data.tra),
                                 Name.value = sapply(dist.diff.1, function(x) names(x)[which.max(x)]),
                                 diff = sapply(dist.diff.1, max)), diff > 0.25)
dist.diff.1[which.max(dist.diff.1$diff[-c(29,30)]), ]


# Marginalización 2
dist.condicionada.2 <- apply(data.tra, 2, function(x){
   sapply(levels(as.factor(x)), 
          function(l){
             sum(data.tra$TIPO_VIA == "OTRO TIPO" & x==l, na.rm = T) /
                sum(data.tra$TIPO_VIA == "OTRO TIPO", na.rm = T)
          })
})
dist.diff.2 <- sapply(1:length(dist.condicionada.2), function(i){
   dist.condicionada.2[[i]] - dist.priori[[i]]
})
dist.diff.2 <- data.frame(Name.var = colnames(data.tra),
                          Name.value = sapply(dist.diff.2, function(x) names(x)[which.max(x)]),
                          diff = sapply(dist.diff.2, max))
dist.diff.2[which(rows.1.NA > 0), ]








## 
apply(data.tra, 2, function(x){
   sapply(levels(as.factor(x)), function(l) mean(x==l, na.rm = T))
})


sapply(1:ncol(data.tra), function(i) levels(as.factor(data.tra[ ,i]))[w.m[i]])



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
             ifelse(TIPO_VIA %in% c("AUTOPISTA", "AUTOVIA", "VIA CONVENCIONAL"),
                    "OTRO TIPO",
                    ifelse(TIPO_VIA == "OTRO TIPO", "NADA ESPECIAL", NA)),
             levels(ACOND_CALZADA)[ACOND_CALZADA])),
          NUMBER.NA = apply(., 1,  function(x) sum(is.na(x))))%>%
   convertFactorIntoNumeric(c(5:7,13:28))
   # rbind(data.tra[ ,-ncol(data.tra)]) %>%
   # cbind("Is.Test" = c(rep(T, nrow(data.tst)),
   #                     rep(F, nrow(data.tra)))) %>%
   # filter(Is.Test) %>%
   # select(-Is.Test)
   # mice::mice(m=5, method="pmm") %>%
   # mice::complete() %>%


#' Experimentación mediante validación cruzada
#' XGB
#' Parámetros probados:
#' nrounds = c(30,50,75),
#' eta = c(0.1, 0.3)

results <- cross.validation(data   = select(data.tra, -TIPO_ACCIDENTE),
                            labels = as.numeric(data.tra$TIPO_ACCIDENTE)-1,
                            model.function = model.xgb,
                            folds = createFolds(data.tra$TIPO_ACCIDENTE, 5),
                            params = expand.grid(nrounds = c(50,75),
                                                 eta = c(0.1, 0.3)))

print(results)
#' Generación del modelo con los datos de entrenamiento
set.seed(3141592)
model <- model.xgb(select(data.tra, -TIPO_ACCIDENTE),
                   as.numeric(data.tra$TIPO_ACCIDENTE)-1,
                   params = c(nrounds = 75, eta = 0.3))

#' Predicción sobre datos de test
#' Escritura de los datos
predictions <- predict(model, as.matrix(data.tst.imp))
writePredictions(predictions, "31-1-Bayes")
