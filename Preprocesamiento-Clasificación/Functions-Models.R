
##
#  Funciones para dotar de igual formato a las funciones de los modelos y hacer CV
##

require(xgboost)

model.xgb <- function(data,
                      labels,
                      params){
   num_class <- length(unique(labels))

   matrix.xgb <- xgb.DMatrix(data = as.matrix(data),
                             label = labels)

   nrounds <- params["nrounds"]
   params <- append(params[-which(names(params)=="nrounds")],
                    list(objective = "multi:softmax"))
   
   model <- xgb.train(data = matrix.xgb,
                      num_class = num_class, 
                      nrounds = nrounds,
                      params = params)
}

require(kknn)

model.knn <- function(data,
                      labels,
                      params){
   
   model <- train.kknn(formula = labels ~ .,
                       data = cbind(data, labels = labels),
                       params)
}


#' SVM model

require(e1071)

model.svm <- function(data,
                      labels,
                      params){
   
   df <- data.frame(data, labels = as.factor(labels))
   
   model <- svm(labels ~ ., 
                df,
                kernel = "linear")
}
