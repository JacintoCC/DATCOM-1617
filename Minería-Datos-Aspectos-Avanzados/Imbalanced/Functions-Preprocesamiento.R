#'@function Transform Factor
#'@description Transform a factor column into a T/F matrix by factor level
#'
#'@param Column
#'@return T/F matrix by factor level
transformFactor <- function(column){
   levels.col <- unique(column)
   transf.mat <- sapply(levels.col, function(l){column <= l})
   colnames(transf.mat) <- as.character(levels.col)
   return(transf.mat)
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



#'@function Write Predictions
#'@description Transform Predictions into correct format and write a file valid for Kaggle platform
#'
#'@param predictions Vector of predictions
#'@param file.name File name to save the predictions
writePredictions <- function(predictions, file.name = "predictions"){
   predictions <- data.frame(id = 1:length(predictions),
                             prediction = predictions)
   write.csv(predictions, file = paste("Predictions/", file.name, ".csv", sep = ""),  
             quote = F, row.names = F)
}

#'@function IPFub
#'@description Preprocessing function with filtering with IPF
IPFub <- function(data, labels, preproc.function, 
                  ipf.args = list(), 
                  ub.args = list()){
   df <- data.frame(data, PV1MATH = labels)
   clean <- do.call(IPF,
                    append(list(formula = PV1MATH ~ .,
                                data = df),
                           ipf.args))$cleanData
   
   balanced <- do.call(preproc.function,
                       append(list(X = select(clean, -PV1MATH),
                                   Y = clean$PV1MATH),
                              ub.args))
   return(balanced)
}

#'@function IPFub
#'@description Preprocessing function with filtering with IPF
ubIPF <- function(data, labels, preproc.function, 
                  ipf.args = list(), 
                  ub.args = list()){
   balanced <- do.call(preproc.function, list(data = data,
                                              labels = labels,
                                              ub.args))
   df <- data.frame(balanced$X, PV1MATH = balanced$Y)
   clean <- do.call(IPF, list(formula = PV1MATH ~ ., 
                              data = df, 
                              ipf.args))$cleanData
   
   return(list(X = select(clean, -PV1MATH),
               Y = clean$PV1MATH))
}
