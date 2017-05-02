
# PREPROCESAMIENTO Y FILTRADO DE VALORES PERDIDOS
# IMPUTACIÓN DE VALORES


# LECTURA DE DATOS

data.tra <- read.csv("Data/accidentes-kaggle.csv")
data.tst  <- read.csv("Data/accidentes-kaggle-test.csv")


# ADECUACIÓN DE LA VARIABLE HORA FORMATO NUMÉRICO

levels(data.tra$HORA) <- sub(",",".",levels(data.tra$HORA))
levels(data.tst$HORA) <- sub(",",".",levels(data.tst$HORA))

# SIMPLIFICACIÓN DE VALORES DE TIPO NUMÉRICO
#' Discretizamos el valor de la variable hora para simplificar la entrada de los datos

data.tra$HORA <- round(as.numeric(levels(data.tra.wo.NA$HORA)[data.tra.wo.NA$HORA]))
data.tst$HORA <- round(as.numeric(levels(data.tst.wo.NA$HORA)[data.tst.wo.NA$HORA]))

# INCLUSIÓN DE NIVELES NO DISPONIBLES EN LOS DATOS DE TRAIN Y DE TEST

sapply(1:ncol(data.tst.wo.NA),
       function(i){
          l.tst <- levels(data.tst.wo.NA[[i]])
          
          return(l.tst[!(l.tst %in% levels(data.tra.wo.NA[[i]]))])
       })

sapply(1:ncol(data.tst.wo.NA),
       function(i){
          l.tst <- levels(data.tra.wo.NA[[i]])
          
          return(l.tst[!(l.tst %in% levels(data.tst.wo.NA[[i]]))])
       })
levels(data.tra$ISLA) <- c(levels(data.tra$ISLA),"HIERRO") 
data.tra$ISLA <- factor(data.tra$ISLA, 
                            levels(data.tra$ISLA)[order(levels(data.tra$ISLA))])

levels(data.tst$MEDIDAS_ESPECIALES) <- c(levels(data.tst$MEDIDAS_ESPECIALES),"HABILITACION ARCEN") 
data.tst$MEDIDAS_ESPECIALES <- 
   factor(data.tst$MEDIDAS_ESPECIALES,
          levels(data.tst$MEDIDAS_ESPECIALES)[order(levels(data.tst$MEDIDAS_ESPECIALES))])

# CREACIÓN DE PARTICIONES
library(caret)
set.seed(3141592)
folds <- createDataPartition(data.tra$TIPO_ACCIDENTE, 5)
save.image("Data/Basic.RData")
