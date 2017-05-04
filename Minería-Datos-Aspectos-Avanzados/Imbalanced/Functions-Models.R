##
#  Funciones para dotar de igual formato a las funciones de los modelos y hacer CV
##

####
#     XGB
###

require(xgboost)

#'@function Model XGB
#'
#'@description Create Model of XGB
#'@param data Data Matrix
#'@param labels Labels
#'@param params Params of the model
model.xgb <- function(data,
                      labels,
                      params){
   matrix.xgb <- xgb.DMatrix(data = as.matrix(data),
                             label = as.numeric(labels)-1)

   nrounds <- params["nrounds"]
   params <- append(params[-which(names(params)=="nrounds")],
                    list(eval_metric = "auc",
                         objective = "binary:logistic"))
   print(params)
   model <- xgb.train(data = matrix.xgb,
                      nrounds = nrounds,
                      params = params)
}

pred.xgb <- function(model, test){
   predict(model, as.matrix(test))
}


####
#     KNN
###

require(kknn)


#'@function Model KNN
#'
#'@description Create Model of KNN
#'@param data Data Matrix
#'@param labels Labels
#'@param params Params of the model
model.knn <- function(data,
                      labels,
                      params){
   
   df <- data.frame(data, l = as.factor(labels))
   model <- train.kknn(l ~ .,
                       data = df)
}

pred.knn <- function(model, test){
   predict(model, test, type="prob")[ ,2]
}

####
#     SVM
###

require(e1071)

#'@function Model SVM
#'
#'@description Create Model of SVM
#'@param data Data Matrix
#'@param labels Labels
#'@param params Params of the model
model.svm <- function(data,
                      labels,
                      params){
   
   df <- data.frame(data, labels = labels)
   model <-do.call(svm,
                   append(list(formula = labels ~ .,
                               data = df,
                               probability=T),
                          params))
}

pred.svm <- function(model, test){
   attr(predict(model, test,
                probability=T,
                decision.values=F),
        "probabilities")[,1]
      
}

####
#     RF
###

require(randomForest)
#'@function Model RF
#'
#'@description Create Model of RF
#'@param data Data Matrix
#'@param labels Labels
#'@param params Params of the model
model.rf <- function(data,
                      labels,
                      params){
   
   model <-do.call("randomForest",
                   append(list(x = data,
                               y = as.factor(labels)),
                          params))
}

pred.rf <- function(model, test){
   predict(model, test,
           type="prob")[ ,2]
}

###
#  BAG
###

require(adabag)
#'@function Model Bagging
#'
#'@description Create Model of Bagging
#'@param data Data Matrix
#'@param labels Labels
#'@param params Params of the model
model.bag <- function(data,
                      labels,
                      params){
   
   df <- data.frame(data, labels = factor(labels))
   model <-do.call(bagging,
                   append(list(formula = labels ~ .,
                               data = df),
                          params))
}

pred.bag <- function(model, test){
   predict(model, test,type="prob")$prob[ ,2]
}
