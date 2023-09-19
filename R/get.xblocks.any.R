### Roxygen-izable Documentation ----
#' Get extra blocks in a Thurstonian design, that links as few blocks as possible.
#' 
#' The function creates extra blocks in a Thurstonian design, that links as few
#' initial blocks as possible, with the number of blocks determined by
#' `count.xblocks`. This is only useful for comparisons between linked and
#' partially linked block designs (Jansen & Schulze, 2023). 
#' 
#' 
### Inputs ----
#' @param blocks A matrix defining the blocks of the model. The number of rows
#' must be the number of blocks, each row represents a block and contains the
#' item numbers. The number of columns present the number of items per block.
#' @param itf A vector defining the items-to-factor relation. For example
#' `c(1,1,1,2,2,2)` defines six items, the first three correspond to factor 1,
#' the second three correspond to factor 2. 
##' @param multidim Logical. Should the items within each linking block be
##' forced to be multidimensional?
#' 
### Details ---- 
#' @details The main strategy of the function is to create extra blocks that
#' link as few blocks as possible, with the number of blocks determined by
#' `count.xblocks`. Therefore, first all combinations of additional blocks with
#' the first two blocks are created. If more extra blocks are needed the
#' function uses block three, four, etc..
#' 
### Outputs ---- 
#' @return The result is a matrix where the rows correspond to the specific
#' extra blocks.
#' 
### Examples ----
#' @examples
#' 
#' # Define a matrix of blocks
#' blocks <- matrix(1:15,ncol=3, byrow=TRUE)
#' 
#' # define the item-to-factor relation
#' itf <- rep(1:3,5)
#' 
#' # Get the extra blocks for a partially linked design
#' get.xblocks.any(blocks, itf, FALSE)
#'
#' @references
#' Jansen, M. T., & Schulze, R. (2023). \emph{The Thurstonian linked block design: Improving Thurstonian modeling for paired comparison and ranking data}. Manuscript submitted.
#' 
#' @export

### Function definition ----

get.xblocks.any <- function(blocks,itf,multidim){
  
  # Find number of items per block
  nrank <- dim(blocks)[2]
  
  any.block <- matrix(nrow=0,ncol=nrank)
  nxblocks <- count.xblocks(blocks)
  i <- 2
  while(nrow(any.block)!=nxblocks){
    tmp <- t(utils::combn(c(blocks[1:i,]),nrank))
    for(j in 1:i){
      tmp <- tmp[-which(apply(apply(tmp,1,function(x) x%in%blocks[j,]),2,all)),]
    }
    if(multidim){
      itf_mat <- matrix(itf[tmp],ncol=nrank)
      tmp <- tmp[-which(apply(itf_mat,1,function(x) length(unique(x)))!=nrank),]
    }
    if(nxblocks<=nrow(tmp)){
      any.block <- tmp[sample(1:nrow(tmp),nxblocks),]
    }
    i <- i+1
  }
  return(any.block)
}
