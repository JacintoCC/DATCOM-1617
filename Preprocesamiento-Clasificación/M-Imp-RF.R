# MODELO 1

# Sin columnas CARRETERA y ACOND_CALZADA
# Imputación de valores perdidos. 
# Random Forest

library(randomForest)

data.tra.imp <- readRDS("Data/data.tra.imp.Rda")
data.tst.imp <- readRDS("Data/data.tst.imp.Rda")

model.rf.imp <- randomForest(TIPO_ACCIDENTE ~ ., data = data.tra.imp)
saveRDS(model.rf.imp, file="Modelos/model.rf.imp.Rda")

predicted.rf.imp <- predict(model.rf.imp, data.tst.imp)
predicted.rf.imp <- data.frame(id = 1:length(predicted.rf.imp), prediction = predicted.rf.imp)
write.csv(predicted.rf.imp, file = "Predictions/predictions.rf.imp.csv",  quote = F, row.names = F)


