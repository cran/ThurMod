### Roxygen-izable Documentation ----
#' Creates names for paired comparisons of a given design.
#' 
#' This function creates names for paired comparisons in the ixiy scheme. If
#' items 1 and 2 are compared, this corresponds to i1i2.
#' 
#' 
### Inputs ----
#'
#' @param blocks A matrix defining the blocks of the model. The number of rows
#' must be the number of blocks, each row represents a block and contains the
#' item numbers. The number of columns present the number of items per block.
#' 
### Outputs ---- 
#' @return Returns a character vector, containing names for all binary indicators
#' of a design.
#' 
### Examples ----
#' @examples
#' 
#' # Define 30 items divided by ten triplets as blocks
#' blocks <- matrix(c(1:30), ncol = 3)
#' 
#' i.name(blocks)
#'     
#' @export

i.name <- function(blocks){
    pair_combn <- pair.combn(blocks)
    pair_names <- paste0('i',pair_combn[,1],'i',pair_combn[,2])
  return(pair_names)
}
