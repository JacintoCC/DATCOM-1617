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

#' @function AUC
#' 
#' @param predictions
#' @param labels
#' 
#' @return AUC
AUC <- function(predictions, 
                     labels){
   t <- table(predictions, labels)
   AUC <- (1+t[1,1]-t[1,2])
   
   return(AUC)
}
