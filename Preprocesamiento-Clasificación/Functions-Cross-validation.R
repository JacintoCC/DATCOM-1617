#' @function Make cross validation
#' 
#' @param predictions
#' @param labels
#' 
#' @return Accuracy
accuracy <- function(predictions, 
                     labels){
   mean(predictions == labels)
}

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
                             model.function,
                             folds,
                             params,
                             measure = accuracy){
   
   labels <- as.numeric(labels) - 1
   params <- as.data.frame(params)
   
   # Aplicamos la función por las particiones del modelo 
   results <- apply(params, 1, function(param.combination){
      print(param.combination)
      cv.measures <-sapply(1:length(folds), 
                           function(i){
                              index.tst <- unlist(folds[i])
                              index.tra <- unlist(folds[-i])
                              
                              model <- model.function(data[index.tra, ], 
                                                      labels[index.tra], 
                                                      param.combination)
                              
                              predictions <- predict(model, as.matrix(data[index.tst, ]))
                              partition.measure <- measure(predictions,
                                                           labels[index.tst])
                              return(partition.measure)
                           })
      return(mean(cv.measures))
   })
   
   return(cbind(params, results))
}
