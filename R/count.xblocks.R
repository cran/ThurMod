### Roxygen-izable Documentation ----
#' Determine the number of extra blocks
#' 
#' This function determines the minimal number of extra blocks needed in order
#' to link all blocks.
#' 
### Inputs ----
#'
#' @param blocks A matrix defining the blocks of the model. The number of rows
#' must be the number of blocks, each row represents a block and contains the
#' item numbers. The number of columns present the number of items per block.
#' 
### Details ---- 
#' @details The matrix of blocks must be constructed so that the number of
#' columns corresponds to the number of items per block. The number of rows
#' corresponds to the number of blocks for the specific measure. If $p$ is the
#' number of blocks, and $k$ is the number of items per block (e.g. $k=3$ for
#' triplets), then the number of extra blocks can be determined by (see also
#' Jansen & Schulze, 2023)
#' \deqn{\lceil\frac{p-1}{k-1}\rceil}
#' 
### Outputs ---- 
#' @return An integer corresponding to the number of extra blocks needed.
#' 
### Examples ----
#' @examples
#' 
#' # Define a matrix of blocks
#' blocks <- matrix(1:15,ncol=3, byrow=TRUE)
#' 
#' # Determine the number of extra blocks needed
#' count.xblocks(blocks)
#' 
#' @references 
#' Jansen, M. T., & Schulze, R. (2023). \emph{The Thurstonian linked block design: Improving Thurstonian modeling for paired comparison and ranking data}. Manuscript submitted.
#'
#' @export


### Function definition ----
count.xblocks <- function(blocks){
  return(ceiling((nrow(blocks)-1)/(ncol(blocks)-1)))
}

