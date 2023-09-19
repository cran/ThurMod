### Roxygen-izable Documentation ----
#' Sorts the blocks in ascending numbering
#' 
#' This function sorts all items in a block into ascending order.
#' 
### Inputs ----
#'
#' @param blocks A matrix defining the blocks of the model. The number of rows 
#' must be the number of blocks, each row represents a block and contains the 
#' item numbers. The number of columns represent the number of items per block.
#' 
### Outputs ---- 
#' @return Returns a matrix consisting of the blocks where all items per blocks
#' are sorted in ascending order.
#' 
### Examples ----
#' @examples
#' 
#' # Define 30 items divided by ten triplets as blocks
#' blocks <- matrix(c(1:30), ncol = 3)
#' 
#' # sort the blocks
#' blocksort(blocks)
#' 
#' @export


### Function definition ----

blocksort <- function(blocks){
  blocks <- t(apply(blocks,1,sort))
  return(blocks)
}








