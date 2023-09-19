### Roxygen-izable Documentation ----
#' Find all general blocks
#' 
#' This function creates meta interlinked blocks within a Thurstonian
#' design.
#' 
#' 
### Inputs ----
#'
#' @param blocks A matrix defining the blocks of the model. The number of rows
#' must be the number of blocks, each row represents a block and contains the
#' item numbers. The number of columns present the number of items per block.
#' 
#' 
### Outputs ---- 
#' @return Returns a list of items that form meta interlinked blocks.
#'
### Details
#' @details This function creates meta interlinked blocks of a block design.
#' These are blocks, that have at least one link from each of its items to
#' any other of its items. If there is not such a link between every item,
#' there are at least two meta blocks.
#' 
### Examples ----
#' @examples
#' 
#' # Define 30 items divided by ten triplets as blocks
#' blocks <- matrix(c(1:30), ncol = 3)
#' 
#' # Add one block to link the first three blocks.
#' blocks <- rbind(blocks,c(1,2,3))
#' 
#' # Find mata blocks
#' metablock(blocks)
#'   
#' @export


metablock <- function(blocks){
  A <- designA(blocks)
  Arref <- rref(A)
  Arank <- dim(A)[1]-length(which(apply(abs(Arref),1,sum)==0))
  
  # Find global blocks
  tmp <- which(apply(Arref,2,sum)<0)
  gblock <- list()
  for(i in 1:length(tmp)){
    tmp0 <- which(Arref[,tmp[i]]==-1)
    if(is.null(dim(Arref[tmp0,]))){
      tmp0 <- which(Arref[tmp0,]==1)
    }else{
      tmp0 <- apply(Arref[tmp0,],1,function(x) which(x==1))
    }
    gblock[[i]] <- unname(c(tmp0,tmp[i]))
  }
  return(gblock)
}


