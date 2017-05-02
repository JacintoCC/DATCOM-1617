source("Functions-Performance.R")

#' @function Make cross validation
#' 
#' @param data
#' @param labels
#' @param model.function
#' @param partitions
#' @param params
#' @param measures
#' 
#' @return Mean performance measure accross partitions
cross.validation <- function(data,
                             labels,
                             folds,
                             model.function,
                             predict.function = predict,
                             params = NULL,
                             preproc.function = function(d, l){return(list(X=d, Y=l))}, 
                             measure = AUC,
                             ...){
   
   # Aplicamos la función por las particiones del modelo 
   results <- apply(params, 1, function(param.combination){
      print(param.combination)
      
      cv.measures <-sapply(1:length(folds), 
         function(i){
            index.tst <- unlist(folds[i])
            index.tra <- unlist(folds[-i])
            
            data.fold <- preproc.function(data[index.tra, ],
                                          labels[index.tra],
                                          ...)
            model <- model.function(data.fold$X, 
                                    data.fold$Y, 
                                    param.combination,
                                    ...)
         
            predictions <- predict.function(model, data[index.tst,])
            partition.measure <- measure(predictions,
                                         labels[index.tst])
      
            return(partition.measure)
         })
      return(mean(cv.measures))
   })
   
   return(cbind(params, results))
}
predict.bag <- function(m, test){
   as.numeric(predict(m, test)$class)
}