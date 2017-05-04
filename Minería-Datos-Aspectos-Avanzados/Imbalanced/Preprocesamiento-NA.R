
# PREPROCESAMIENTO Y FILTRADO DE VALORES PERDIDOS
# IMPUTACIÓN DE VALORES

library(ggplot2)
library(Hmisc)
library(mice)
library(robCompositions)
library(VIM)
library(caret)
library(randomForest)


# LECTURA DE DATOS

data.tra <- read.csv("Data/accidentes-kaggle.csv")
data.tst  <- read.csv("Data/accidentes-kaggle-test.csv")


# ADECUACIÓN DE LA VARIABLE HORA FORMATO NUMÉRICO

levels(data.tra$HORA) <- sub(",",".",levels(data.tra$HORA))
levels(data.tst$HORA) <- sub(",",".",levels(data.tst$HORA))


# ELIMINACIÓN DE VARIABLES CON MAYORÍA DE NA

data.tra.wo.NA <- subset(data.tra, select= -c(ACOND_CALZADA, CARRETERA))
data.tst.wo.NA <- subset(data.tst, select= -c(ACOND_CALZADA, CARRETERA))


# SIMPLIFICACIÓN DE VALORES DE TIPO NUMÉRICO

data.tra.wo.NA$HORA <- round(as.numeric(levels(data.tra.wo.NA$HORA)[data.tra.wo.NA$HORA]))
data.tst.wo.NA$HORA <- round(as.numeric(levels(data.tst.wo.NA$HORA)[data.tst.wo.NA$HORA]))

# IMPUTACIÓN DE VALORES

data.tra.imp <- mice::mice(data.tra.wo.NA, m=5, method="pmm")
data.tra.imp <- complete(data.tra.imp)

data.tst.imp <- mice::mice(data.tst.wo.NA, m=5, method="pmm")
data.tst.imp <- complete(data.tst.imp)


# BUSCAR NIVELES DE LOS FACTORES EN TEST NO DISPONIBLES EN TRAIN

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


levels(data.tra.imp$ISLA) <- c(levels(data.tra.imp$ISLA),"HIERRO") 
data.tra.imp$ISLA <- factor(data.tra.imp$ISLA, 
                            levels(data.tra.imp$ISLA)[order(levels(data.tra.imp$ISLA))])

levels(data.tst.imp$MEDIDAS_ESPECIALES) <- c(levels(data.tst.imp$MEDIDAS_ESPECIALES),"HABILITACION ARCEN") 
data.tra.imp$MEDIDAS_ESPECIALES <- 
   factor(data.tra.imp$MEDIDAS_ESPECIALES,
          levels(data.tra.imp$MEDIDAS_ESPECIALES)[order(levels(data.tra.imp$MEDIDAS_ESPECIALES))])


# GUARDAR DATOS

saveRDS(data.tra.imp, file="Data/data.tra.imp.Rda")
saveRDS(data.tst.imp, file="Data/data.tst.imp.Rda")
