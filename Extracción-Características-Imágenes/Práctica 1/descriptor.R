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