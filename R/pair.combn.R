### Roxygen-izable Documentation ----
#' Determine all paired comparisons
#' 
#' This function returns a matrix containing all paired comparisons defined by
#' a design.
#' 
### Inputs ----
#'
#' @param blocks A matrix defining the blocks of the model. The number of rows
#' must be the number of blocks, each row represents a block and contains the
#' item numbers. The number of columns present the number of items per block.
#' @param unique Logical. Should only unique paired comparisons be returned?
#' 
#' 
### Outputs ---- 
#' @return Returns a matrix with all paired comparisons defined by a design.
#' 
### Examples ----
#' @examples
#' 
#' #' # Define 30 items divided by three triplets as blocks
#' blocks <- matrix(c(1:30), ncol = 3)
#' 
#' # Get all blocks
#' pair.combn(blocks)
#'     
#' @export

### Function definition ----


pair.combn <- function(blocks,unique=TRUE){
  pair_combn <- NULL
  if(is.null(nrow(blocks))){
    blocks <- t(blocks)
  }
  for(i in 1:nrow(blocks)){
    pair_combn <- rbind(pair_combn,t(utils::combn(blocks[i,],2)))
  }
  if(unique){
    pair_combn <- unique(pair_combn[order(pair_combn[,1],pair_combn[,2]),])
  }else{
    pair_combn <- pair_combn[order(pair_combn[,1],pair_combn[,2]),]
  }
  return(pair_combn)
}

