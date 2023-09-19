### Roxygen-izable Documentation ----
#' Count paired comparisons
#' 
#' This function calculates the number of paired comparisons needed to compare a
#'  set of N items.
#' 
#' 
### Inputs ----
#'
#' @param nitem Number of items.
#' 
#' 
### Outputs ---- 
#' @return An integer corresponding to the number of paired comparisons.
#' 
### Details
#' @details 
#' This function is only useful, if the number of paired comparisons of a full
#' design, that is all possible paired comparisons, is of interest. Then the 
#' number is
#' \deqn{\frac{N\times(N-1)}{2}}
#' 
### Examples ----
#' @examples 
#' 
#' # Number if paired comparisons for a set of 15 items = 105.
#' count.combn(15)
#'
#' @export



### Function definition ----
count.combn <- function(nitem){
  return(nitem*(nitem-1)/2)
}
