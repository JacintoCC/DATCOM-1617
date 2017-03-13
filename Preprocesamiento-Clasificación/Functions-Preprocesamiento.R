#'@function Transform Factor
#'@description Transform a factor column into a T/F matrix by factor level
#'
#'@param Column
#'@return T/F matrix by factor level
transformFactor <- function(column){
   levels.col <- levels(column)
   sapply(levels.col, function(l){column==l})
}

#'@function Convert Factor Into Numeric
#'@description Transform data frame using T/F for factor levels
#'
#'@param data data.frame
#'@param cols Factor cols 
#'@return Data Frame
convertFactorIntoNumeric <- function(data, cols){
   categorical.mat <- lapply(cols, function(i){
      transformFactor(data[ ,i])
   })
   
   result.mat <- NULL
   
   for(i in 1:ncol(data)){
      if(i %in% cols){
         result.mat <- data.frame(cbind(result.mat, categorical.mat[[which(i==cols)]]))
      }
      else{
         result.mat <- data.frame(cbind(result.mat, data[ ,i]))
      }
   }
   
   return(result.mat)
}