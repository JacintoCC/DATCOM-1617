# BITÁCORA CON RESULTADOS 
library(zoo)

bitacora <- data.frame(Date =  as.Date("19/02/2017", "%d/%m/%Y"),
                       AUC = 0.82958,
                       Model = "RF. Imputación",
                       Posicion = 2, 
                       stringsAsFactors = F)

bitacora <- rbind(bitacora, 
                  list(Date =  as.Date("09/03/2017", "%d/%m/%Y"),
                       AUC = 0.81911,
                       Model = "XGB. Imputación. nround = 100",
                       Posicion = 6)
                     )

bitacora <- rbind(bitacora, 
                  list(Date =  as.Date("09/03/2017", "%d/%m/%Y"),
                       AUC = 0.8273,
                       Model = "XGB. Imputación. nround = 100",
                       Posicion = 6)
)

bitacora <- rbind(bitacora, 
                  list(Date =  as.Date("11/03/2017", "%d/%m/%Y"),
                       AUC = 0.83106,
                       Model = "XGB. Imputación. nround = 75",
                       Posicion = 2)
)

bitacora <- rbind(bitacora, 
                  list(Date =  as.Date("13/03/2017", "%d/%m/%Y"),
                       AUC = 0.5854,
                       Model = "KNN. K = 7",
                       Posicion = 2)
)

bitacora <- rbind(bitacora, 
                  list(Date =  as.Date("13/03/2017", "%d/%m/%Y"),
                       AUC = 0.83126,
                       Model = "Poll KNN, XGB, RF",
                       Posicion = 2)
)