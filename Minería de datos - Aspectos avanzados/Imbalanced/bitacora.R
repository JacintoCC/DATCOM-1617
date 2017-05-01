# BITÁCORA CON RESULTADOS 
library(zoo)

bitacora <- NULL

addEntry <- function(date, AUC, model, position){
   rbind(bitacora, 
         list(Date =  as.Date(date, "%d/%m/%Y"),
              AUC = AUC,
              Model = model,
              Posicion = position)
   )
}

bitacora <- addEntry("13/03/2017", 0.0, ".", 100)
