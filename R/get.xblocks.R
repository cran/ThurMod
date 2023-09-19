### Roxygen-izable Documentation ----
#' Get extra blocks in a Thurstonian design, that links all blocks.
#' 
#' The function creates extra blocks for a Thurstonian design, that links all
#' initial blocks with as few extra blocks as possible. The number of extra
#' blocks is determined by `count.xblocks` (see Jansen & Schulze, 2023). 
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
#' @param item_not The items that are differently keyed compared to the majority
#' of items.
#' @param min Logical. Should a minimal number of blocks contain mixed keyed items?
#' @param show.warnings Logical. Should warnings be shown?
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
#' # Get the extra blocks for a completely linked design
#' get.xblocks(blocks, itf, FALSE)
#' 
#' @references
#' Jansen, M. T., & Schulze, R. (2023). \emph{The Thurstonian linked block design: Improving Thurstonian modeling for paired comparison and ranking data}. Manuscript submitted.
#'      
#' @export


### Function definition ----

get.xblocks <- function(blocks, itf, multidim, item_not=NULL,min=FALSE,show.warnings=FALSE){
  
  # Find number of items per block
  nrank <- dim(blocks)[2]
  
  if(nrow(blocks)<nrank){
    if(nrank%in%c(2,3,4)){
      number <- c('two','three','four')
      name <- c('dyads','triplets','quads')
      stop(paste0('For ', name[nrank-1],' at least ' , number[nrank-1],' ', name[nrank-1],' are needed!'))
    }else{
      stop('Not enough blocks. Please use more.')
    }
  }
  if(multidim){
    if(length(unique(itf))<nrank){
      stop('If multidimensional blocks should be created, you need at least as many factors as items per block.')
    }
  }
  nrank <- ncol(blocks)
  if(multidim){
    tmp_blocks <- get.xblocks.multidim(nrank,blocks,itf,item_not,min,show.warnings)
  }else{
    tmp_blocks <- get.xblocks.random(nrank,blocks,item_not)
  }
  return(tmp_blocks)
}
