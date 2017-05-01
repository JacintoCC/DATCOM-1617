# MODELO 1

# Sin columnas CARRETERA y ACOND_CALZADA
# Imputación de valores perdidos. 
# Random Forest

library(xgboost)
source("Functions-Models.R")

data.tra <- read.csv("Data/pv1math-tra.csv")
data.tst <- read.csv("Data/pv1math-tst.csv")

data.tra$PV1MATH <- as.factor(data.tra$PV1MATH)

model <- model.xgb(data.tra[ ,-ncol(data.tra)],
                   data.tra[ , ncol(data.tra)],
                   params = c(nrounds = 50))

predicted.xgb <- predict(model, as.matrix(data.tst))
predicted.xgb <- data.frame(id = 1:length(predicted.xgb), prediction = predicted.xgb)
write.csv(predicted.xgb, file = "Predictions/predictions.xgb.csv",
          quote = F, row.names = F)


