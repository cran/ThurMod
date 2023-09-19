### Roxygen-izable Documentation ----
#' Recode variables.
#' 
#' 
#' 
#' @author Markus Thomas Jansen
#' 
#' 
#' 
### Inputs ----
#' @param var A variable/column of a data matrix/ data frame containing data to be recoded.
#' @param vals A vector containing the original scores. If cat is TRUE, these are thresholds.
#' @param ct A vector containing the desired scores.
#' @param cat Logical. should the data be categorized?
#' 
#' ### Outputs ---- 
#' @return Returns a vector with the recoded input values.
#' 
### Deatils ----

#' @details This function is a simple recode function. It is possible to recode any value to any other value, as long as the class of values within a vector are equal. If an interval of numerical values should be categorized, for N categories, N-1 thresholds must be given. The first N-1 categories are constructed by using all values that are smaller or equal (<=) than the thresholds, the last category is constructed to be all values larger than the last threshold.

### Examples ----
#' @examples
#' #numerical
#' v <- rep(c(1:5),10)
#' v_r <- recode(v, c(1:5), c(5:1))
#'
#' # character
#' v <- rep(c('a','b','d','e'),10)
#' v_r <- recode(v, c('a','b','d','e'), c('apple','1','dummy',TRUE))
#' 
#' # interval
#' set.seed(1)
#' v <- sample(runif(20,0,6))
#' 
#' # recode x <= 1.3 into 1
#' # recode x <= 2.6 into 2
#' # recode x <= 3.9 into 3
#' # recode x <= 5.2 into 4
#' # recode x > 5.1 into 5
#' v_r <- recode(v,c(1.3,2.6,3.9,5.2),c(1:5),cat=TRUE)
#' 
#' @export


### Function definition ----
recode <- function(var=var, vals=c(1,2,3,4,5), ct=c(5,4,3,2,1),cat=FALSE){
  varnew <- var
  if(cat){
    done <- c()
    for (i in 1:length(vals)){
      score <- setdiff(which(var<=vals[i]),done)
      varnew  <- replace(varnew, score, values=ct[i])
      done <- c(done,score)
    }
    score <- setdiff(which(var>vals[length(vals)]),done)
    varnew  <- replace(varnew, score, values=ct[length(ct)])
  }else{
    for (i in 1:length(vals)) {
      score <- which(var==vals[i])
      varnew  <- replace(varnew, score, values=ct[i])
    }
  }
  return(varnew)
}
