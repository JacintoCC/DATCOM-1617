# MODELO 1

# Random Forest

library(randomForest)

data.tra <- read.csv("Data/pv1math-tra.csv")
data.tst <- read.csv("Data/pv1math-tst.csv")

data.tra$PV1MATH <- as.factor(data.tra$PV1MATH)

model.rf <- randomForest(PV1MATH ~ ., data = data.tra)

predicted.rf <- predict(model.rf, data.tst)
predicted.rf <- data.frame(id = 1:length(predicted.rf), prediction = predicted.rf)
write.csv(predicted.rf, file = "Predictions/predictions.rf.csv",
          quote = F, row.names = F)


