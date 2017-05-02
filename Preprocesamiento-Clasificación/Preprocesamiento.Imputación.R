

#' ELIMINACIÓN DE VARIABLES CON MAYORÍA DE NA
#' En la inspección de los datos se observa cómo la mayoría de los valores
#' de las variables de acondicionamiento de la calzada tenemos un valor 
#' perdido, con lo que consideramos esta variable como poco relevante.
#' Para el valor de la carretera nos encontramos con una gran cantidad
#' de elementos, y puesto que ya se incluye en los datos una categoría
#' con el tipo de carretera, el nombre exacto de la carretera será poco
#' relevante, además de que hay una gran cantidad de valores perdidos.

# Mostramos la proporción de valores perdidos en el conjunto de entrenamiento por variable
apply(data.tra, 2, function(x) mean(is.na(x)))


results <- cross.validation(data   = data.tra.cat[ ,-ncol(data.tra.cat)],
                            labels = data.tra.cat[ ,ncol(data.tra.cat)],
                            model.function = model.xgb,
                            folds = folds,
                            params = expand.grid(nrounds = seq(50,100, by = 10), 
                                                 eta = seq(0.1, 0.5, by= 0.1),
                                                 gamma = seq(0, 0.01, by = 0.005),
                                                 max_depth = c(4,6)))

#' Observamos como en acondicionamiento de calzada casi un 80 por ciento
#' de valores perdidos y un 55 por ciento para el valor de carretera. 

#' Transformación 1: Eliminación de las variables con mayoría de valores
#' perdidos. 
#' Código de transformación: NA.1

#data.tra.NA.1 <- data.tra %>% select(-ACOND_CALZADA,-CARRETERA)
#data.tst.NA.1 <- data.tst %>% select(-ACOND_CALZADA,-CARRETERA)

# IMPUTACIÓN DE VALORES

data.tra.imp <- mice::mice(data.tra.wo.NA, m=5, method="pmm")
data.tra.imp <- complete(data.tra.imp)

data.tst.imp <- mice::mice(data.tst.wo.NA, m=5, method="pmm")
data.tst.imp <- complete(data.tst.imp)