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

   col.names <- colnames(data)
   categorical.mat <- lapply(cols, function(i){
      transformFactor(data[ ,i])
   })
   
   result.mat <- NULL
   result.names <- NULL
   
   for(i in 1:ncol(data)){
      if(i %in% cols){
         result.mat <- data.frame(cbind(result.mat, categorical.mat[[which(i==cols)]]))
         result.names <- c(result.names, 
                           paste(col.names[i], 
                                 colnames(categorical.mat[[which(i==cols)]]),
                                 sep = "."))
      }
      else{
         result.mat <- data.frame(cbind(result.mat, data[ ,i]))
         result.names <- c(result.names,  col.names[i])
      }
   }
   colnames(result.mat) <- result.names
   
   return(result.mat)
}

month.names.2.numeric <- c(4,8,12,1,2,7,6,3,5,11,10,9)
week.names.2.numeric <- c(7,4,1,2,3,6,5)

#'@function Write Predictions
#'@description Transform Predictions into correct format and write a file valid for Kaggle platform
#'
#'@param predictions Vector of predictions
#'@param file.name File name to save the predictions
writePredictions <- function(predictions, file.name = "predictions"){
   
   predictions <- as.factor(predictions)
   levels(predictions) <- c("Atropello","Colision_Obstaculo","Colision_Vehiculos",
                            "Otro","Salida_Via","Vuelco")
   predictions <- data.frame(id = 1:length(predictions),
                             prediction = predictions)
   write.csv(predictions, file = paste("Predictions/", file.name, ".csv", sep = ""),  
             quote = F, row.names = F)
}