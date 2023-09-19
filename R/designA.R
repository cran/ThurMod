### Roxygen-izable Documentation ----
#' Create the Thurstonian design matrix for paired comparison and ranking data.
#' 
#' Creates the Thurstonian design matrix for paired comparison and ranking data,
#' given by blocks or the number of items.
#' 
### Inputs ----
#' @param blocks A matrix defining the blocks of the model. The number of rows
#' must be the number of blocks, each row represents a block and contains the
#' item numbers. The number of columns present the number of items per block.
#' @param nitems The number of items that are included in the design. 
#' 
### Outputs ---- 
#' @return Returns a design matrix which includes all paired comparisons
#' derivable from the blocks.
#' 
### Details
#' @details Each Thurstonian design can be defined by blocks of at least two
#' items. The function determines the fundamental design matrix A of the
#' Thurstonian design, including all possible paired comparisons that can be
#' derived by the blocks. For further information of the importance of the
#' design matrix, see Jansen and Schulze (2023a,2023b).
#' 
### Examples ----
#' @examples
#' 
#' # Define a matrix of blocks
#' blocks <- matrix(1:15,ncol=3, byrow=TRUE)
#' 
#' # Get the design matrix
#' loading_Matrix <- designA(blocks)
#' 
#' @references 
#' Jansen, M. T., & Schulze, R. (2023a). \emph{Linear factor analytic Thurstonian forced-choice models: Current status and issues}. Manuscript submitted.
#' 
#' Jansen, M. T., & Schulze, R. (2023b). \emph{The Thurstonian linked block design: Improving Thurstonian modeling for paired comparison and ranking data}. Manuscript submitted.
#' 
#' @export


### Function definition ----

designA <- function(blocks=NULL, nitems=max(unique(blocks))){
  if(is.null(blocks)&nitems<2){
    stop('If blocks is not defined, nitems must be defined.')
  }
  if(nitems>1&is.null(blocks)){
    pairs <- t(utils::combn(1:nitems,2))
  }else{
    pairs <- pair.combn(blocks)
  }
  
  A_tmp <- c()
  for(i in 1:nrow(pairs)){
    tmp <- rep(0,nitems)
    tmp[pairs[i,1]] <- 1
    tmp[pairs[i,2]] <- -1
    A_tmp <- rbind(A_tmp,unname(tmp))
  }
  return(A_tmp)
}
