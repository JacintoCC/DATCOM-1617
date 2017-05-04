#' @function Accuracy
#' 
#' @param predictions
#' @param labels
#' 
#' @return Accuracy
accuracy <- function(predictions, 
                     labels){
   mean(predictions == labels)
}

require(pROC)
#' @function AUC
#' 
#' @param predictions
#' @param labels
#' 
#' @return AUC
AUC <- function(predictions, 
                     labels){
   auc(roc(labels,predictions))
}
