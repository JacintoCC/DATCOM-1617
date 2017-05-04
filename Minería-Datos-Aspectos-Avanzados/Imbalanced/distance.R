##
#  Distance Functions
## 

require(parallel)

data.tra.imp <- readRDS("Data/data.tra.imp.Rda")
data.tst.imp <- readRDS("Data/data.tst.imp.Rda")

#'@function distance.num
distance.num <- function(x,y){
   return(abs(x-y))
}

#'@function distance.mod
distance.mod <- function(x,y,mod){
   d1 <- (x-y) %% mod
   d2 <- (y-x) %% mod
   
   return(min(d1,d2))
}

distance.cat <- function(x,y){
   return(x!=y)
}

distance.functions <- c(distance.num, 
                        function(x,y) distance.mod(x,y,12),
                        function(x,y) distance.mod(x,y,24), 
                        function(x,y) distance.mod(x,y,7),
                        distance.cat,distance.cat,distance.cat,
                        distance.num,distance.num,distance.num,distance.num,distance.num,
                        distance.cat,distance.cat,distance.cat,distance.cat,distance.cat,
                        distance.cat,distance.cat,distance.cat,distance.cat,distance.cat,
                        distance.cat,distance.cat,distance.cat,distance.cat,distance.cat,
                        distance.cat)

# distance <- function(x,y){
#    d <- sapply(1:length(x), function(i){
#       do.call(distance.functions[[i]], list(x=x[[i]], y=y[[i]]))
#    })
# }

distance <- function(x,y){
   d <- c(distance.mod(x[2],y[2],12)/6,
          distance.mod(x[3],y[3],24)/12,
          x[c(4:7,13:27)] != y[c(4:7,13:27)],
          abs(x[c(1,8:12)] - y[c(1,8:12)]))
   return(unlist(d))
}

distance.euclidean <- function(x,y){
   d <- distance(x,y)
   return(sum(d*d))
}

##
#  CREATE MATRIX OF DISTANCES
##

matrix.distances <- sapply(t(data.tst.imp), function(x){
   sapply(t(data.tra.imp), function(y){
      distance.euclidean(x,y)
   })
})
