
poll.predictions <- data.frame("XGB" = read.csv("Predictions/predictions.xgb.cat.vc.csv",
                                                row.names = "id"), 
                               "RF"  = read.csv("Predictions/predictions.csv",
                               row.names = "id"), 
                               "KNN" = read.csv("Predictions/predictions.knn.csv",
                               row.names = "id"))

predictions <- apply(poll.predictions, 1, function(x){
   ifelse(x[2]==x[3], x[2], x[1])
})

predictions <- data.frame(id = 1:nrow(data.tst.cat),
                          prediction = predictions)
write.csv(predictions, file = "Predictions/predictions.poll.csv",  
          quote = F, row.names = F)
