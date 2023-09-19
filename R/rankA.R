### Roxygen-izable Documentation ----
#' Determine the rank of the design matrix defined by the blocks.
#' 
#' This function determines the rank of the fundamental design matrix defined
#' by the blocks.
#' 
### Inputs ----
#'
#' @param blocks A matrix defining the blocks of the model. The number of rows
#' must be the number of blocks, each row represents a block and contains the
#' item numbers. The number of columns present the number of items per block.
#' 
### Outputs ---- 
#' @return Returns the rank of the design matrix as an integer.
#' 
### Examples ----
#' @examples
#' 
#' # Define nine items divided by three triplets as blocks
#' blocks <- matrix(c(1:9), ncol = 3)
#' 
#' # Determine the rank of the design matrix
#' rankA(blocks)
#'     
#' @export


rankA <- function(blocks){
  A <- designA(blocks)
  Arref <- rref(A)
  Arank <- dim(A)[1]-length(which(apply(abs(Arref),1,sum)==0))
  return(Arank)
}

