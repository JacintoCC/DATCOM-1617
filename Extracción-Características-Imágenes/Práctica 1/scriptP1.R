# Práctica 1
# Extracción de características en imágenes
# Jacinto Carrasco Castillo


# Declaración de estructuras para ahorrar cálculos


# Decimal numbers into bits 
dec.into.bits <- t(sapply(0:255, function(x) rev(as.integer(intToBits(x)))))[ ,25:32]
# Number of Transitions
U <- apply(dec.into.bits, 1, function(x) sum(x != c(x[-1],x[1])))
# Corresponding label 
E <- sapply(1:256, function(i) ifelse(U[i] <= 2, sum(U[1:i] <= 2) - 1, 58))

# Estructura para ahorrar cálculos

#'@function Extract Index of blocks and subblocks
#'@param im.dim Dimensions of the image
#'@param cell Size of each block
#'@param overlap Size of overlapping between blocks
blocksStructure <- function(im.dim, cell = 16, overlap = 8){
   
   # Get Dimensions of Original Image
   n.col <- im.dim[1]
   n.row <- im.dim[2]
   
   # Count subblocks by block margin
   subb.in.block <- cell/overlap
   # Count number of subblocks by margin
   n.horiz.subb <- n.col/overlap
   n.verti.subb <- n.row/overlap
   
   # Total number of subblocks
   blocks.numbers <- 1:(n.horiz.subb*n.verti.subb)
   
   # Filter by the available subblocks to start the block
   right.limits <- -seq(0, length.out = subb.in.block - 1) %% n.horiz.subb
   bottom.limits <- (n.verti.subb  - subb.in.block + 1) * n.horiz.subb
   blocks.numbers <- blocks.numbers[!(blocks.numbers %% n.horiz.subb) %in% right.limits & 
                                       blocks.numbers < bottom.limits]
   
   index.blocks <- t(sapply(blocks.numbers, function(i){
      as.vector(sapply(seq(i, length.out = subb.in.block, by = n.horiz.subb), 
                       function(j) seq(j, length.out = subb.in.block)))
   })) 
   
   return(list(n.col = n.col, n.row = n.row,
               overlap = overlap,
               index.blocks = index.blocks))
}

blocks.dims <- blocksStructure(im.dim = c(64,128),
                               cell = 16,
                               overlap = 8)


# Declaración de funciones


#'@function LBPu
#'
#'@description Local Binary Patterns
#'@param im Image
#'@return LBP Image with uniform label
LBPu <- function(im){
   # Creation of auxiliar image with zero padding
   zero.padding <- rbind(0, cbind(0,im,0), 0)
   
   # List of neighbours image to compare with
   neighbours <- list(zero.padding[1:(nrow(zero.padding)-2), 1:(ncol(zero.padding)-2)],
                      zero.padding[1:(nrow(zero.padding)-2), 2:(ncol(zero.padding)-1)],
                      zero.padding[1:(nrow(zero.padding)-2), 3:ncol(zero.padding)],
                      zero.padding[2:(nrow(zero.padding)-1), 3:ncol(zero.padding)],
                      zero.padding[3:nrow(zero.padding),     3:ncol(zero.padding)],
                      zero.padding[3:nrow(zero.padding),     2:(ncol(zero.padding)-1)],
                      zero.padding[3:nrow(zero.padding),     1:(ncol(zero.padding)-2)],
                      zero.padding[2:(nrow(zero.padding)-1), 1:(ncol(zero.padding)-2)])
   
   # Matrix with all the comparisons
   comparison <- sapply(neighbours, 
                        function(x){
                           im >= x
                        })   
   
   # For each pixel, we obtain the associated LPB
   lpb <- matrix(apply(comparison, 1, function(x){
      basic.lpb <- strtoi(paste(as.numeric(x), collapse = ""), base = 2) 
      return(E[basic.lpb + 1])
   }), 
   ncol = nrow(im), nrow = ncol(im), byrow = T)
   
   return(lpb)
}

#'@function LBPu Features
#'
#'@description Local Binary Patterns
#'@param patch LBP Window Image 
#'@return Feature vector
lbp_features <- function(patch, block.dims = blocksStructure()){
   
   subblocks_c <- matrix(1:block.dims$n.col, ncol = block.dims$overlap, byrow = T)
   subblocks_r <- matrix(1:block.dims$n.row, ncol = block.dims$overlap, byrow = T)
   
   subblocks.data <- apply(subblocks_r, 1, function(x){
      apply(subblocks_c, 1, function(y){
         sapply(0:58, function(i) sum(i == patch[x,y]))
      })
   })
   
   subblocks <- matrix(subblocks.data, ncol = 59, byrow = T)
   features <- apply(blocks.dims$index.blocks, 1, function(x){
      col.sum <- colSums(subblocks[x,])
      norm.col.sum <- col.sum/sqrt(sum(col.sum*col.sum))
   })
   return(as.vector(features))
}

# Lectura de ficheros
# Se supone que en el entorno de trabajo tenemos una subcarpeta ./data con la estructura del fichero zip proporcionado
bg.tra.files <- list.files("data/train/background/", pattern = ".png", full.names = T)
bg.tra <- lapply(bg.tra.files, function(x) grayscale(load.image(x))[,,1,1])


pd.tra.files <- list.files("data/train/pedestrians/", pattern = ".png", full.names = T)
pd.tra <- lapply(pd.tra.files, function(x) grayscale(load.image(x))[,,1,1])


bg.tst.files <- list.files("data/test/background/", pattern = ".png", full.names = T)
bg.tst <- lapply(bg.tst.files, function(x) grayscale(load.image(x))[,,1,1])


pd.tst.files <- list.files("data/test/pedestrians/", pattern = ".png", full.names = T)
pd.tst <- lapply(pd.tst.files, function(x) grayscale(load.image(x))[,,1,1])

# Obtención características
bg.tra.descriptor <- t(sapply(bg.tra, function(x) lbp_features(LBPu(x), blocks.dims)))
pd.tra.descriptor <- t(sapply(pd.tra, function(x) lbp_features(LBPu(x), blocks.dims)))
bg.tst.descriptor <- t(sapply(bg.tst, function(x) lbp_features(LBPu(x), blocks.dims)))
pd.tst.descriptor <- t(sapply(pd.tst, function(x) lbp_features(LBPu(x), blocks.dims)))

# Creación conjuntos de entrenamiento y test
data.tra <- rbind(bg.tra.descriptor,
                  pd.tra.descriptor)
label.tra <- c(rep(0, nrow(bg.tra.descriptor)),
               rep(1, nrow(pd.tra.descriptor)))

data.tst <- rbind(bg.tst.descriptor,
                  pd.tst.descriptor)
label.tst <- c(rep(0, nrow(bg.tst.descriptor)),
               rep(1, nrow(pd.tst.descriptor)))

# Creación modelo
xgb.model <- xgboost::xgboost(data = data.tra, label = label.tra,
                              nrounds = 10,
                              params = list(objective = "binary:logistic"),
                              verbose = F)

# Predicción de datos de train
preds <- predict(xgb.model, data.tra) > 0.5
table(preds, label.tra)
mean(preds == label.tra)

# Predicción de datos de test
preds <- predict(xgb.model, data.tst) > 0.5
table(preds, label.tst)
mean(preds == label.tst)