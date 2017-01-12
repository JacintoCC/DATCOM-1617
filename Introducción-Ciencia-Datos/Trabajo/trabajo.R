# "Trabajo integrador - Introducción a la Ciencia de Datos"
# Jacinto Carrasco Castillo

setwd("~/Documentos/DATCOM-1617/Introducción-Ciencia-Datos/Trabajo")
require(ggplot2)
require(tidyr)
require(ggbiplot)
require(kknn)
require(class)
require(caret)
require(moments)
require(MASS)

# Análisis de datos

## Análisis de los datos de regresión.

forestFires <- read.csv("forestFires/forestFires.dat", 
                        header = F, comment.char = "@")
names.forestFires <- c("X", "Y", "Month", "Day", "FFMC", "DMC", "DC", "ISI", "Temp", 
                       "RH", "Wind", "Rain", "Area")
colnames(forestFires) <- names.forestFires
attach(forestFires)

### Trabajo con el conjunto de datos

  
str(forestFires)

summary(forestFires)
sapply(forestFires, sd)

anyNA(forestFires)


ggplot(forestFires, aes(X,Y)) + 
   geom_point(color = densCols(data.frame(X,Y), 
                               colramp = colorRampPalette(heat.colors(100)))) 
ggplot(forestFires, aes(Month)) + geom_histogram(bins = 12)
ggplot(forestFires, aes(Day)) + geom_histogram(bins = 7)
ggplot(forestFires, aes(FFMC)) + geom_histogram()
ggplot(forestFires, aes(DMC)) + geom_histogram()
ggplot(forestFires, aes(DC)) + geom_histogram()
ggplot(forestFires, aes(ISI)) + geom_histogram()
ggplot(forestFires, aes(Temp)) + geom_histogram()
ggplot(forestFires, aes(RH)) + geom_histogram()
ggplot(forestFires, aes(Wind)) + geom_histogram()
ggplot(forestFires, aes(Rain)) + geom_histogram()
ggplot(forestFires, aes(Area)) + geom_histogram()
forestFires[Area < 300, ] %>%
  gather(- Area, key = "var", value = "value") %>% 
  ggplot(aes(x = value, y = Area)) +
    geom_point() +
    facet_wrap(~ var, scales = "free") +
    theme_bw()

forestFires$Summer <- Month %in% 6:9

calculate_BUI <- function(DMC, DC){
   0.8*DMC*DC / (DMC + 0.4 * DC)
}

forestFires$BUI <- calculate_BUI(DMC, DC)

calculate_FWI <- function(BUI, ISI){
   D <- ifelse(BUI < 80, 0.626*BUI^0.809 + 2, 1000/(25 + 108.64 * exp(-0.023*BUI)))
   arg.log <- 0.1 * ISI * D
   
   log.FWI <- 2.72 * (0.434 * log(arg.log))^0.647
   FWI <- ifelse(arg.log < 1, arg.log, exp(log.FWI))
}

forestFires$FWI <- calculate_FWI(forestFires$BUI, forestFires$ISI)

forestFires[Area < 300,c("BUI","FWI","Summer","Area") ] %>%
  gather(-Area, key = "var", value = "value") %>% 
  ggplot(aes(x = value, y = Area)) +
    geom_point() +
    facet_wrap(~ var, scales = "free") +
    theme_bw()

detach(forestFires)

## Análisis de los datos de clasificación

contraceptive <- read.csv("contraceptive/contraceptive.dat", 
                          header = F, comment.char = "@")
names.contraceptive <- c("Age", "W_Education", "H_Education", "Children", 
                         "W_Religion", "W_Working", "H_Occupation", 
                         "SoL", "Exposure", "Method")
colnames(contraceptive) <- names.contraceptive
contraceptive <- transform(contraceptive, 
                           W_Education = factor(W_Education, ordered = T),
                           H_Education = factor(H_Education, ordered = T),
                           W_Religion = as.factor(W_Religion),
                           W_Working = as.factor(W_Working),
                           H_Occupation = as.factor(H_Occupation),
                           SoL = as.factor(SoL),
                           Exposure = as.factor(Exposure),
                           Method = as.factor(Method))
attach(contraceptive)
str(contraceptive)

summary(contraceptive)
sapply(contraceptive[ ,c("Age", "Children")], sd)
### Trabajo con el conjunto de datos

anyNA(contraceptive)

ggplot(contraceptive, aes(Age)) + geom_histogram(bins = 30)
ggplot(contraceptive, aes(W_Education)) + geom_bar()
ggplot(contraceptive, aes(H_Education)) + geom_bar()
ggplot(contraceptive, aes(Children)) + geom_histogram(bins = 15)
ggplot(contraceptive, aes(W_Religion)) + geom_bar()
ggplot(contraceptive, aes(H_Occupation)) + geom_bar()
ggplot(contraceptive, aes(SoL)) + geom_bar()
ggplot(contraceptive, aes(Exposure)) + geom_bar()
ggplot(contraceptive, aes(Method)) + geom_bar()

ks.test(Age, "punif", min(Age), max(Age))

### Feature engineering 

quantile(Children)
contraceptive$Children.cat <- cut(Children, c(0,1,3,5,16), include.lowest = T,
                                  labels =  c("No", "Few", "Some", "Many"),
                                  right = F)
ggplot(contraceptive, aes(Children.cat)) + geom_bar()

detach(contraceptive)

# Regresión

attach(forestFires)

#' @description Get the associated MSE for predicted values
#' 
#' @name MSE MSE for data
#' @param values Original values
#' @param fitted.values Fitted values of data
#' @return MSE 
MSE <- function(values, fitted.values){  
   return(mean((values - fitted.values)^2))
} 

#' @description Get the associated RMSE for predicted values
#' 
#' @name RMSE RMSE for data
#' @param values Original values
#' @param fitted.values Fitted values of data
#' @return RMSE 
RMSE <- function(values, fitted.values){  
   return(sqrt(MSE(values,fitted.values)))
} 

#' @description Get the adjusted R squared measure of error 
#' 
#' @name R.squared R squared
#' @param values Original values
#' @param fitted.values Fitted values of data
#' @return R squared
R.squared <- function(values, fitted.values){
   return(1 - sum((values - fitted.values)^2) / sum((values - mean(values))^2) )
}

#' @description Read KEEL partitions 
#' 
#' @name file.name Name file
#' @param num.CV Number of partitions
#' @param colnames Column names
#' @return List with training partitions in a list and test partitions in 
#'         another list.
load_KEEL_CV_files <- function(file.name, num.CV, colnames){
   partition <- list()
   partition$tra <- list()
   partition$tst <- list()
   
   for(i in 1:num.CV){
      # Read train file
      file <- paste(file.name, "-", num.CV, "-", i, "tra.dat", sep="")
      partition$tra[[i]] <- read.csv(file, comment.char="@", header = F,
                                     col.names = colnames)
      
      # Read test file
      file <- paste(file.name, "-", num.CV, "-", i, "tst.dat", sep="")
      partition$tst[[i]] <- read.csv(file, comment.char="@", header = F, 
                                     col.names = colnames)
   }
   
   return(partition)
}

forestFires.partition <- load_KEEL_CV_files("forestFires/forestFires", 5,
                                            names.forestFires)

transf.forestFires <- function(x){ 
   x$Summer <- x$Month %in% 6:9
   x$BUI <- calculate_BUI(x$DMC, x$DC)
   x$FWI <- calculate_FWI(x$BUI, x$ISI)
   new.names.forestFires <- c(names.forestFires[-13], "Summer", "BUI", 
                              "FWI", "Area")
   x <- x[ , new.names.forestFires]
   colnames(x) <- new.names.forestFires
   return(x)
}

forestFires.partition$tra <- lapply(forestFires.partition$tra, transf.forestFires)
forestFires.partition$tst <- lapply(forestFires.partition$tst, transf.forestFires)

#' @name Run KNN model in a fold
#' 
#' @description Run KNN in a fold and get MSE error and R^2
#' @param i Fold number
#' @param part.dataset Partitioned dataset
#' @param model Model to be applied
#' @param k k for KNN
#' @return list with train and test MSE and R squared
run_knn_fold <- function(i, part.dataset, model = "Y ~ .", k = 7){
   x_tra <- part.dataset$tra[[i]]
   x_tst <- part.dataset$tst[[i]]
   
   # Number of attributes
   n_att <- ncol(x_tra)
   
   fitMulti <- kknn(model, x_tra, x_tra, k = k)
   predict.train <- fitMulti$fitted.value
   fitMulti <- kknn(model, x_tra, x_tst, k = k)
   predict.test <-fitMulti$fitted.value
   
   # MSE train

   return(c("MSE train" = MSE(x_tra[ ,n_att], predict.train),
            "R.squared train" = R.squared(x_tra[ ,n_att], predict.train),
            "MSE test" = MSE(x_tst[ ,n_att], predict.test),
            "R.squared test" = R.squared(x_tst[ ,n_att], predict.test)))
}

run_knn_CV <- function(part.dataset, model, k = 7){
   apply(sapply(1:5, run_knn_fold, part.dataset, model, k), 1, mean)
}

#' @name Run linear model in a fold
#' 
#' @description Run linear model in a fold and get MSE error and R^2
#' @param i Fold number
#' @param part.dataset Partitioned dataset
#' @param model Model to be applied
#' @return list with train and test MSE and R squared
run_lm_fold <- function(i, part.dataset, model = "Y ~ ."){
   x_tra <- part.dataset$tra[[i]]
   x_tst <- part.dataset$tst[[i]]
   
   # Number of attributes
   n_att <- ncol(x_tra)
   
   # Train linear model
   fitMulti <- lm(model, data = x_tra)
   predict.train <- predict(fitMulti,x_tra)
   predict.test <- predict(fitMulti, x_tst)

   # MSE train

   return(c("MSE train" = MSE(x_tra[ ,n_att], predict.train),
            "R.squared train" = R.squared(x_tra[ ,n_att], predict.train),
            "MSE test" = MSE(x_tst[ ,n_att], predict.test),
            "R.squared test" = R.squared(x_tst[ ,n_att], predict.test)))
}

run_lm_CV <- function(part.dataset, model){
   apply(sapply(1:5, run_lm_fold, part.dataset, model), 1, mean)
}

error.measures <- data.frame("MSE train" = numeric(0),
                             "R.squared train" = numeric(0),
                             "MSE test" = numeric(0),
                             "R.squared test" = numeric(0))

fit.DMC <-lm(Area ~ DMC, data = forestFires)
summary(fit.DMC)

fit.BUI <-lm(Area ~ BUI, data = forestFires)
summary(fit.BUI)

fit.FWI <-lm(Area ~ FWI, data = forestFires)
summary(fit.FWI)

fit.RH <-lm(Area ~ RH, data = forestFires)
summary(fit.RH)

fit.Temp <-lm(Area ~ Temp, data = forestFires)
summary(fit.Temp)

error.measures["lm.DMC", ]  <- run_lm_CV(forestFires.partition, 
                                         model = "Area ~ DMC")
error.measures["lm.BUI", ]  <- run_lm_CV(forestFires.partition,
                                         model = "Area ~ BUI")
error.measures["lm.FWI", ]  <- run_lm_CV(forestFires.partition, 
                                         model = "Area ~ FWI")
error.measures["lm.Temp", ] <- run_lm_CV(forestFires.partition, 
                                         model = "Area ~ Temp")  
error.measures["lm.RH", ]   <- run_lm_CV(forestFires.partition, 
                                         model = "Area ~ RH")
error.measures

## Regresión lineal múltiple

fit.all <-lm(Area ~ ., data = forestFires)
summary(fit.all)
error.measures["lm.all", ] <- run_lm_CV(forestFires.partition, model = "Area ~ .")

fit.mostrelevant <- lm(Area ~ Temp + RH + DMC, data = forestFires)
summary(fit.mostrelevant)
error.measures["lm.mostrelevant", ] <- run_lm_CV(forestFires.partition,
                                                 model = Area ~ Temp + RH + DMC)

### Interacciones

fit.inter.1 <- lm(Area ~ Temp + Rain + Temp*Rain, data = forestFires)
summary(fit.inter.1)
error.measures["lm.inter.1", ] <- run_lm_CV(forestFires.partition, 
                                            model = Area ~ Temp + Rain + Temp*Rain)

fit.inter.2 <- lm(Area ~ BUI + RH + RH*BUI + Temp + Temp*BUI + Temp*RH + 
                     I(BUI^2) + I(RH^2) , data = forestFires)
summary(fit.inter.2)
error.measures["lm.inter.2", ] <- run_lm_CV(forestFires.partition,
               model = Area ~ BUI + RH + RH*BUI + Temp + Temp*BUI + 
                                       Temp*RH + I(BUI^2) + I(RH^2))
## k-NN

scaled.forestFires <- forestFires.partition
ncol.forestFires <- ncol(forestFires.partition$tra[[1]])

for(i in 1:5){
   scale.factor <- scale(forestFires.partition$tra[[i]][ ,-ncol.forestFires ])
   scaled.forestFires$tra[[i]][ ,-ncol.forestFires] <- as.data.frame(scale.factor)
   scaled.forestFires$tst[[i]][ ,-ncol.forestFires] <- 
      as.data.frame(scale(forestFires.partition$tst[[i]][ ,-ncol.forestFires],
                          center = attr(scale.factor,"scaled:center"),
                          scale = attr(scale.factor,"scaled:scale")))
}
err.k <-unname(sapply(3:30, function(k)  run_knn_CV(scaled.forestFires, 
                                                    model = Area ~ ., k = k)[3]))
ggplot(data.frame(K = 3:30, err.k), aes(x = K, y = err.k)) + geom_line() + 
   labs(title = "Evolución del MSE en test para K", y = "MSE")

k = 20


error.measures["knn.all", ] <- run_knn_CV(scaled.forestFires, 
                                          model = Area ~ ., k)
error.measures["knn.Temp", ] <- run_knn_CV(scaled.forestFires, 
                                           model = Area ~ Temp, k)
error.measures["knn.mostrelevant", ] <- run_knn_CV(scaled.forestFires, 
                                       model = Area ~ Temp + RH + DMC, k)
error.measures["knn.inter.1", ] <- run_knn_CV(scaled.forestFires,
                           model = Area ~ Temp + Rain + Temp*Rain, k)
error.measures

## Comparación 


regr.train <- read.csv("regr_train_alumnos.csv")
error.train <- c(error.measures["lm.all", "MSE.train"],
                 error.measures["knn.all", "MSE.train"])
error.original.train <- regr.train[11, c("out_train_lm", "out_train_kknn")]
regr.train[11, c("out_train_lm", "out_train_kknn")] <- error.train

print(error.original.train)
print(error.train)

regr.test <- read.csv("regr_test_alumnos.csv")
error.test <- c(error.measures["lm.all", "MSE.test"],
                error.measures["knn.all", "MSE.test"])
error.original.test <- regr.test[11, c("out_test_lm", "out_test_kknn")]
regr.test[11, c("out_test_lm", "out_test_kknn")] <- error.train

print(error.original.test)
print(error.test)

### Comparación de KNN y lm

wilcox.test(x = regr.train$out_train_lm, y = regr.train$out_train_kknn)
wilcox.test(x = regr.test$out_test_lm, y = regr.test$out_test_kknn)


### Comparación de los tres métodos



friedman.test(as.matrix(regr.train[ , -1]))
friedman.test(as.matrix(regr.test[ , -1]))

tam <- dim(regr.train[ ,-1])
groups <- rep(1:tam[2], each=tam[1])
pairwise.wilcox.test(as.matrix(regr.train[ ,-1]), groups, 
                     p.adjust = "holm", paired = TRUE)
pairwise.wilcox.test(as.matrix(regr.test[ ,-1]),  groups, 
                     p.adjust = "holm", paired = TRUE)


detach(forestFires)

# Clasificación


attach(contraceptive)

contraceptive.partition <- load_KEEL_CV_files("contraceptive/contraceptive", 10,
                                              names.contraceptive)

transf.contraceptive <- function(x){ 
   new.names.contraceptive <- c(names.contraceptive[-10],"Children_cat", "Method")
   x$Children_cat <- cut(x$Children, c(0,1,3,5,16), include.lowest = T,
                         labels =  c("No", "Few", "Some", "Many"),
                         right = F)
   x <- x[ , new.names.contraceptive]
   colnames(x) <- new.names.contraceptive
   return(x)
}

contraceptive.partition$tra <- lapply(contraceptive.partition$tra, transf.contraceptive)
contraceptive.partition$tst <- lapply(contraceptive.partition$tst, transf.contraceptive)

## $k$-NN

#' @name Run KNN model in a fold
#' 
#' @description Run KNN in a fold and get Accuracy and Kappa
#' @param i Fold number
#' @param part.dataset Partitioned dataset
#' @param model Model to be applied
#' @param k k for KNN
#' @return list with train and test Acc and Kappa
run_knn_class_fold <- function(i, part.dataset, model = "Y ~ .", k = 7){
   x_tra <- part.dataset$tra[[i]]
   x_tst <- part.dataset$tst[[i]]
   
   # Number of attributes
   n_att <- ncol(x_tra)
   
   x_tra[ ,n_att] <- as.factor(x_tra[ ,n_att])
   fitMulti <- kknn(model, x_tra, x_tra, k = k)
   predict.train <- fitted(fitMulti)
   fitMulti <- kknn(model, x_tra, x_tst, k = k)
   predict.test <-fitted(fitMulti)
   
   # Accuracy
   acc.tra <- mean(predict.train == x_tra[ ,n_att])
   acc.tst <- mean(predict.test == x_tst[ ,n_att]) 
   
   # Kappa
   kappa.tra <- (acc.tra - 1/length(levels(x_tra[ ,n_att]))) / 
      (1 - 1/length(levels(x_tra[ ,n_att])))
   kappa.tst <- (acc.tst - 1/length(levels(x_tra[ ,n_att]))) / 
      (1 - 1/length(levels(x_tra[ ,n_att])))
   
   return(c("Acc train" = acc.tra,
            "Kappa train" = kappa.tra,
            "Acc test" = acc.tst,
            "Kappa test" = kappa.tst))
}

run_knn_clas_CV <- function(part.dataset, model, k = 7){
   apply(sapply(1:10, run_knn_class_fold, part.dataset, model, k), 1, mean)
}

transf.contraceptive.knn <- function(ds){
   occ <- ds[ ,"H_Occupation"]
   
   # Creación variables dummy
   dummy.occ <- sapply(1:4, function(i) return(i == occ))
   
   #Acoplamiento de variables dummy
   ds <- cbind(ds,  dummy.occ)
   colnames(ds)[12:15] <- paste("H_Occupation_dummy",1:4,sep = "_")
   
   # Reordenamiento de columnas
   ds <- ds[ ,c(1:6, 8,9,12:15,11)]
   return(ds)
}

numeric.contraceptive.partition <-list()
numeric.contraceptive.partition$tra <- lapply(contraceptive.partition$tra, 
                                              transf.contraceptive.knn)
numeric.contraceptive.partition$tst <- lapply(contraceptive.partition$tst, 
                                              transf.contraceptive.knn)

scaled.contraceptive <- numeric.contraceptive.partition
ncol.contraceptive <- ncol(numeric.contraceptive.partition$tra[[1]])

for(i in 1:10){
   scale.factor <- scale(numeric.contraceptive.partition$tra[[i]][ ,c(1:4,7)])
   scaled.contraceptive$tra[[i]][ ,c(1:4,7)] <- as.data.frame(scale.factor)
   scaled.contraceptive$tst[[i]][ ,c(1:4,7)] <- 
      as.data.frame(scale(contraceptive.partition$tst[[i]][ ,c(1:4,7)],
                          center = attr(scale.factor,"scaled:center"),
                          scale = attr(scale.factor,"scaled:scale")))
}

knn.results <- as.data.frame(t(sapply(1:35, function(k){
   run_knn_clas_CV(scaled.contraceptive, Method ~ ., k)
})))

ggplot(data.frame(k=1:35,knn.results), aes(k)) + 
   geom_line(aes(y=knn.results$`Acc train`,colour = "Train"))+
   geom_line(aes(y=knn.results$`Acc test`, colour = "Test")) +
   labs(y = "Accuracy") +
   scale_x_continuous(breaks = seq(1,35, by = 2)) +
   scale_y_continuous(breaks = seq(0.5, 1, by = 0.05)) 

## LDA

numeric.contraceptive <- as.matrix(as.data.frame(
   contraceptive[ ,c("Age", "Children", "W_Education", 
                     "H_Education", "SoL", "Method")]
))
class(numeric.contraceptive) <- "numeric"


apply(numeric.contraceptive, 2, function(x) shapiro.test(x)$p.value)

#' @name Run LDA model in a fold
#' 
#' @description Run KNN in a fold and get Accuracy and Kappa
#' @param i Fold number
#' @param part.dataset Partitioned dataset
#' @param model Model to be applied
#' @return list with train and test Acc and Kappa
run_lda_fold <- function(i, part.dataset, model = "Y ~ ."){
   x_tra <- part.dataset$tra[[i]]
   x_tst <- part.dataset$tst[[i]]
   
   # Number of attributes
   n_att <- ncol(x_tra)
   
   x_tra[ ,n_att] <- as.factor(x_tra[ ,n_att])
   fitMulti <- lda(model, data = x_tra)
   
   predict.train <- predict(fitMulti, x_tra)$class
   predict.test <- predict(fitMulti, x_tst)$class
   
   # Accuracy
   acc.tra <- mean(predict.train == x_tra[ ,n_att])
   acc.tst <- mean(predict.test == x_tst[ ,n_att]) 
   
   # Kappa
   kappa.tra <- (acc.tra - 1/length(levels(x_tra[ ,n_att]))) / 
      (1 - 1/length(levels(x_tra[ ,n_att])))
   kappa.tst <- (acc.tst - 1/length(levels(x_tra[ ,n_att]))) / 
      (1 - 1/length(levels(x_tra[ ,n_att])))
   
   return(c("Acc train" = acc.tra,
            "Kappa train" = kappa.tra,
            "Acc test" = acc.tst,
            "Kappa test" = kappa.tst))
}

run_lda_CV <- function(part.dataset, model){
   apply(sapply(1:10, run_lda_fold, part.dataset, model), 1, mean)
}

lda.results <- run_lda_CV(contraceptive.partition, Method ~ .)
lda.results


## QDA

cor(numeric.contraceptive)
qda.condition <- sapply(numeric.contraceptive.partition$tra,
    function(x){
       num_att <- ncol(x)
       output <- x[ ,num_att]
       cond.partition <- apply(x[ ,-num_att], 2,
          # Para cada columna y
          function(y){
             # Comprobamos si es una variable factor
             niveles <- unique(y)
             num_niveles <- length(niveles)
             
             if(num_niveles > 4)
                return(T)
             # Si es un factor, comprobamos que en la salida haya tres clases
             else{
                cond <- sapply(niveles, 
                   function(i){
                     return(length(unique(output[y == i])) == 3) 
                   })
                return(all(cond)) 
             }
          })
       return(all(cond.partition))
    })
all(qda.condition)

#' @name Run QDA model in a fold
#' 
#' @description Run KNN in a fold and get Accuracy and Kappa
#' @param i Fold number
#' @param part.dataset Partitioned dataset
#' @param model Model to be applied
#' @return list with train and test Acc and Kappa
run_qda_fold <- function(i, part.dataset, model = "Y ~ ."){
   x_tra <- part.dataset$tra[[i]]
   x_tst <- part.dataset$tst[[i]]
   
   # Number of attributes
   n_att <- ncol(x_tra)
   
   x_tra[ ,n_att] <- as.factor(x_tra[ ,n_att])
   fitMulti <- qda(model, data = x_tra)
   
   predict.train <- predict(fitMulti, x_tra)$class
   predict.test <- predict(fitMulti, x_tst)$class
   
   # Accuracy
   acc.tra <- mean(predict.train == x_tra[ ,n_att])
   acc.tst <- mean(predict.test == x_tst[ ,n_att]) 
   
   # Kappa
   kappa.tra <- (acc.tra - 1/length(levels(x_tra[ ,n_att]))) / 
      (1 - 1/length(levels(x_tra[ ,n_att])))
   kappa.tst <- (acc.tst - 1/length(levels(x_tra[ ,n_att]))) / 
      (1 - 1/length(levels(x_tra[ ,n_att])))
   
   return(c("Acc train" = acc.tra,
            "Kappa train" = kappa.tra,
            "Acc test" = acc.tst,
            "Kappa test" = kappa.tst))
}

run_qda_CV <- function(part.dataset, model){
   apply(sapply(1:10, run_qda_fold, part.dataset, model), 1, mean)
}

transf.contraceptive.qda <- function(ds){
   # Transformación del tipo de dato
   ds <- ds[ ,-c(7,10)]
   ds[-11] <- apply(ds[-11], 2, as.numeric)
   return(ds)
}

numeric.contraceptive.partition <-list()
numeric.contraceptive.partition$tra <- lapply(contraceptive.partition$tra, 
                                              transf.contraceptive.qda)
numeric.contraceptive.partition$tst <- lapply(contraceptive.partition$tst, 
                                              transf.contraceptive.qda)

qda.results <- run_qda_CV(numeric.contraceptive.partition, Method ~ .)
qda.results

## Comparación


detach(contraceptive)

clas.train <- read.csv("clasif_train_alumnos.csv")
print(clas.train[5, c("out_train_knn", "out_train_lda", "out_train_qda")])
print(c(knn.results[13, "Acc train"],lda.results["Acc train"],qda.results["Acc train"]))
clas.train[5, "out_train_knn"] <- knn.results[13, "Acc train"]
clas.train[5, "out_train_lda"] <- lda.results["Acc train"]
clas.train[5, "out_train_qda"] <- qda.results["Acc train"] 
   
clas.test <- read.csv("clasif_test_alumos.csv")
print(clas.test[5, c("out_test_knn", "out_test_lda", "out_test_qda")])
print(c(knn.results[13, "Acc test"],lda.results["Acc test"],qda.results["Acc test"]))
clas.test[5, "out_test_knn"] <- knn.results[13, "Acc test"]
clas.test[5, "out_test_lda"] <- lda.results["Acc test"]
clas.test[5, "out_test_qda"] <- qda.results["Acc test"]


#### Comparación de los tres métodos


friedman.test(as.matrix(clas.train[ , -1]))
friedman.test(as.matrix(clas.test[ , -1]))

tam <- dim(clas.train[ ,-1])
groups <- rep(1:tam[2], each=tam[1])
pairwise.wilcox.test(as.matrix(clas.train[ ,-1]), groups, 
                     p.adjust = "holm", paired = TRUE)
pairwise.wilcox.test(as.matrix(clas.test[ ,-1]),  groups, 
                     p.adjust = "holm", paired = TRUE)
