
poll.predictions <- data.frame("Best" = read.csv("Predictions/SVM.Clean.Over.csv",
                                                row.names = "id"), 
                               "RF.Imp"  = read.csv("Predictions/SVM.Clean.csv",
                               row.names = "id"), 
                               "Clean" = read.csv("Predictions/NCL.SVM.csv",
                               row.names = "id"))

predictions <- apply(poll.predictions, 1, function(x){
   mean(x)
})

predictions <- data.frame(id = 1:nrow(data.tst),
                          prediction = predictions)
write.csv(predictions, file = "Predictions/predictions.poll.csv",  
          quote = F, row.names = F)
