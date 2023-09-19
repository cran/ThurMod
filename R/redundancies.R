### Roxygen-izable Documentation ----
#' Determine the number of redundancies
#' 
#' This function determines the number of redundancies among the tetrachoric
#' correlations and thresholds.
#' 
### Inputs ----
#'
#' @param blocks A matrix defining the blocks of the model. The number of rows
#' must be the number of blocks, each row represents a block and contains the
#' item numbers. The number of columns present the number of items per block.
#' @param warn Logical. Should warning messages be returned? Defaults to `TRUE`.
#' 
#' 
### Outputs ---- 
#' @return Returns an integer of the number of redundancies.
#' 
#' @details If a ranking design is used (variances of binary indicators is zero)
#' there are redundancies among the thresholds and tetrachoric correlations to 
#' be estimated. This is the case, whenever the number of items per block is 
#' larger than two. In these cases the degrees of freedom must be corrected by
#' subtracting the redundancies. For more details see Jansen and Schulze (2023)
#' and Maydeu-Olivares (1999).
#' 
### Examples ----
#' @examples
#' 
#' # Define 30 items divided by ten triplets as blocks
#' blocks <- matrix(c(1:30), ncol = 3)
#' 
#' # define the item-to-factor relation
#' itf <- rep(1:3,10)
#' 
#' # Determine the redundancies
#' redundancies(blocks)
#' 
#' @references 
#' Jansen, M. T., & Schulze, R. (in review). \emph{The Thurstonian linked block design: Improving Thurstonian modeling for paired comparison and ranking data}.
#' 
#' Maydeu-Olivares, A. (1999). Thurstonian modeling of ranking data via mean and covariance structure analysis. \emph{Psychometrika}, \emph{64}(3), 325-340. \doi{10.1007/BF02294299}
#' 
#' @export


redundancies <- function(blocks,warn=TRUE){
  #message('Calculation gblocks...\n')
  gblocks <- metablock(blocks)
  nrank <- dim(blocks)[2]
  
  # original blocks for each gblock
  tmp <- lapply(gblocks, function(x) blocks[which(apply(matrix(blocks%in%x, ncol=ncol(blocks)),1,all)),])
  
  # number of paired comparisons if pairs are unique and not unique
  #message('Testing for block design...\n')
  tmp0 <- unlist(lapply(lapply(tmp,function(x) pair.combn(x)),function(x) ifelse(is.null(nrow(x)),2,nrow(x))))
  tmp1 <- unlist(lapply(tmp,function(x) ifelse(is.null(nrow(x)),2,nrow(pair.combn(x,unique = FALSE)))))
  
  # number of pairs that are possible with all items per gblock
  tmp2 <- unlist(lapply(gblocks,function(x) ifelse(is.null(nrow(pair.combn(x))),1,nrow(pair.combn(x)))))
  
  # Is each comparison considered only once? 
  tmp3 <- tmp0==tmp1
  
  # If comparisons are considered multiple times, are all comparisons considered?
  tmp4 <- tmp0==tmp2
  
  # If any of the two above is true, then calculation of redundancies is easy, else:
  tmp5 <- tmp0[!(tmp3|tmp4)]
  
  if(length(tmp5)!=0){
    if(max(tmp5)>70&warn==T){
      stop('The number of items is larger than 70, but it is not fully linked. It is possible that the calculation of redundancies takes a long time, or is not possible due to computer ressources. I you want to try again use warn=FALSE.')
    }
  }
  
  gpairs <- lapply(tmp,function(x) pair.combn(x))
  red <- 0
  #message('Caculating redundancies per block...\n')
  for(i in 1:length(gblocks)){
    if(tmp4[i]){
      nitems <- length(unique(c(gblocks[[i]])))
      red <- red+nitems*(nitems-1)*(nitems-2)/6
    }else if(tmp3[i]){
      red <- red+nrow(tmp[[i]])*nrank*(nrank-1)*(nrank-2)/6
    }else{

      # Calculation via transitivities
      pairs <- gpairs[[i]]
      all_redun <- t(utils::combn(unique(c(pairs)),3))
      tmp0 <- apply(pairs,1,function(x) paste0(x, collapse = ','))
      tmp1 <- apply(all_redun,1,function(x) paste0(x[c(1,2)], collapse = ','))
      tmp2 <- apply(all_redun,1,function(x) paste0(x[c(1,3)], collapse = ','))
      tmp3 <- apply(all_redun,1,function(x) paste0(x[c(2,3)], collapse = ','))
      tmp <- cbind(tmp1%in%tmp0,tmp2%in%tmp0,tmp3%in%tmp0)
      red <- length(which(apply(tmp,1,all)))
    }
  }
  red <- ifelse(length(red)==0,0,red)
  return(red)
}

