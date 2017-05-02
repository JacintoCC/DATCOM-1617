#SMOTE with ordered factors

distance <- function(i, j, data){
   dist <- sqrt(sum(sapply(data, function(f){
      if(is.ordered(f)){
         abs(as.numeric(f[i]) - as.numeric(f[j]))/length(levels(f))
      }
      else if(is.factor(f)){ # nominal feature
         as.numeric(f[i] != f[j])
      } 
      else {
         (f[i] - f[j]) * (f[i] - f[j])
      }
   })))
   
   return(dist)
}

getNeighbors <- function(x, minority.instances, train, k = 5){
   distance.minority <- sapply(minority.instances, 
                               function(j) 
                                  distance(x, j, train))
   
   return(minority.instances[order(distance.minority)][1:k])
}

syntheticInstance <- function(x, neighbors, data){
   sel.neighbor <- sample(neighbors, 1)
   random.value <- runif(1)
   
   lapply(data, function(f){ 
      if(is.ordered(f)){ # ordered feature
         l <- seq(which(levels(f) == f[x]),
                  which(levels(f) == f[sel.neighbor]))
         s <- ifelse(length(l)==1, l, sample(l,1))
         return(levels(f)[s])
      }  
      else if(is.factor(f)){ # nominal feature
         return(levels(f)[sample(c(f[x], f[sel.neighbor]), 1)])
      } 
      else {
         return(f[x] + random.value * (f[sel.neighbor] - f[x]))
      }
   }) -> synthetic 
   return(data.frame(synthetic))
}

SMOTE <- function(data, labels, proportion = 1, k = 5){
   # Search positive instances
   pos.instances <- which(labels == 1)
   # Compute how many positive instances need to be created
   to.create <- sum(labels == "0") / proportion - sum(labels == "1") 

   if(to.create==0){
      stop("to.create == 0")
   }
   generators.indexes <- sample(pos.instances, size = to.create, replace = T)
   
   # Generate Synthetic instances
   synthetic.instances <- plyr::ldply(generators.indexes,
                                      function(x){
                                         neighbors <- getNeighbors(x, pos.instances, 
                                                                   data, k)
                                         return(syntheticInstance(x, neighbors, data))
                                      })
   
   colnames(synthetic.instances) <- colnames(data)
   X = bind_rows(data, synthetic.instances)

   return(list(X = bind_rows(data, synthetic.instances),
               Y = unlist(list(labels, 
                               factor(rep(1, to.create))))))
}
# 
# cross.validation(data = x,
#                  labels = y,
#                  folds = createFolds(y, k = 2),
#                  model.function = model.svm, 
#                  predict.function = pred.svm,
#                  params = expand.grid(kernel = c("radial")),
#                  preproc.function = function(d,l) ubIPF(d,l,SMOTE),
#                  measure = AUC)

# 
# ## SMOTE prueba
# # 
# x <- data.frame(A = rnorm(100),
#                 B = factor(base::sample(c("a","b","c"), 100, replace = T), levels = c("a","b","c"),
#                            ordered = T),
#                 C = factor(base::sample(c("a","b","c"), 100, replace = T), levels = c("a","b","c")))
# y <- factor(base::sample(c(0,1), 100, replace = T, prob = c(0.7,0.3)), levels = c(0,1))
