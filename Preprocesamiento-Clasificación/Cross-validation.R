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
                             partitions,
                             params,
                             measure = accuracy){
   
   # Aplicamos la función por las particiones del modelo 
   cv.measures <- sapply(1:length(partitions), 
                         function(i){
                            index.tst <- partitions[i]
                            index.tra <- partitions[-i]
                            
                            model <- model.function(data[index.tra], 
                                                    labels[index.tra], 
                                                    params)
                  
                            predictions <- predict(model, index.tst)
                            partition.measure <- measure(predictions,
                                                         labels[index.tst])
                            return(partition.measure)
                         })
   
   return(mean(cv.measures))
   
}