# Minería de datos: Aspectos avanzados
# Clasificación ordinal
# Jacinto Carrasco Castillo

library(RWeka)
library(foreign)

# Modelos múltiples para el dataset esl.arff

set.seed (2)
dataset <- read.arff(file = "esl.arff")
dataset$out1 <- as.factor(dataset$out1)

# Selección de las instancias de test
test.index <- sample(1:nrow(dataset), 100)

dataset.train <- dataset[-test.index, ]
dataset.test <- dataset[test.index, ]


###
#     CREACIÓN DE FUNCIONES PARA LA AUTOMATIZACIÓN
###



#'@function getBinaryClassesFromCategoric
#'@description Obtención de una lista de data.frame con valores binarios en la columna de salida por cada clase
#'
#'@param data data.frame de entrada. Se espera la variable de salida en la última columna
#'@return Lista de data.frame. 
getBinaryClassesFromCategoric <- function(data){
   n.column <- ncol(data)
   levels.output <- levels(data[ ,n.column])
   n.levels <- length(levels.output)
   
   # Para cada nivel l (menos el último), sustituimos la variable de salida
   # por una binaria.
   dataframes.list <- lapply(levels.output[-n.levels], 
        function(l){
           cbind(data[ ,-n.column],
                 data[ , n.column] == l)
        })
   
   return(dataframes.list)
}



#'@function ordinalClassification
#'@description Aplicación de un modelo de clasificación de manera ordinal
#'
#'@param data data.frame de entrada. Se espera la variable de salida en la última columna
#'@param classificator Modelo de clasificación a aplicar
#'@return Lista de modelos. 
ordinalClassification <- function(data, classificator, ...){
   # Realizamos la transformación del dataset en los distintos conjuntos 
   binary.datasets <- getBinaryClassesFromCategoric(data)
   
   # Para cada conjunto de datos transformado (para cada clase de salida)
   # aprendemos un modelo
   models <- lapply(binary.datasets, 
                    function(single.dataset){
                       return(classificator(single.dataset, ...))
                    })
   return(models)
}

#'@function predictOrdinalClassification 
#'@description Clasificación ordinal de nuevas instancias a partir de los modelos obtenidos
#'
#'@param models Modelos aprendidos
#'@param test.data Datos de test
#'@return Predicciones
predictOrdinalClassification <- function(models, test.data){
   # Lanzamos la predicción para las nuevas clases
   all.predictions <- sapply(models, predict, newdata = test.data, type = "prob")
   all.predictions <- cbind(all.predictions[1:nrow(test.data), ], 
                            all.predictions[(nrow(test.data)+1):(2*nrow(test.data)), ])
   
   # Para cada dato
   class.predictions <- apply(all.predictions, 1, 
      function(probs){
         # Tomamos las probabilidades calculadas y le damos forma
         # de matriz 2x(Número de clases) 
         probs <- matrix(probs, nrow = 2, byrow = T)

         # Para cada columna (clase) la probabilidad es el producto de
         # las probabilidades de no haber tomado las clases antariores
         # por la probabilidad de tomar esa clase.
         probs <- c(sapply(1:ncol(probs),
                         function(j){
                            ifelse(j == 1, 
                                   probs[2,1], 
                                   prod(probs[1, 1:(j-1)]) * probs[2,j])
                            }
                         ),
                    prod(probs[1, ]))
         
         return(probs)
         })

   return(t(class.predictions))
}

###
#     USO DE LAS FUNCIONES CREADAS PARA EL DATASET  els
###

library(randomForest)

#'@function Model Random Forest
#'
#'@description Modelado de la función random forest para usarla en las funciones creadas
model.random.forest <- function(dataset){
   randomForest(x = dataset[ ,-ncol(dataset)],
                y = as.factor(dataset[ , ncol(dataset)]))
}

# Obtención de los modelos
rf.models <- ordinalClassification(dataset.train,
                                   model.random.forest)

# Obtención de las predicciones
prediction.rf <- predictOrdinalClassification(rf.models, 
                                              dataset.test[ ,-ncol(dataset.test)])

# Asignación de la clase con mayor probabilidad
prediction.rf.class <- apply(prediction.rf, 1, which.max)

# Accuracy 
mean(factor(prediction.rf.class, 1:9) == dataset.test[ ,ncol(dataset.test)])
 
# Por la implementación realizada podemos usar cualquier función de clasificación 
# sin más que pasar la función de clasificación. 
# Obtenemos un Accuracy de 0.64