### Roxygen-izable Documentation ----
#' Calculate reliability estimate for factor scores.
#' 
#' This function returns a reliability estimate for factor scores estimated via
#' the Thurstonian IRT models.
#' 
#' 
### Inputs ----
#' @param scores A matrix with factor scores. Rows are respondents, columns are
#' factors/traits.
#' @param scores_se A matrix with the standard errors of the factor scores. 
#' Rows are respondents, columns are factors/traits. Factors/traits must be in
#' the same order as for `scores`.
#' @param method Can be one of 'div' or 'sub'. See details. Defaults to 'sub'.
#'   
### Outputs ---- 
#' @return Returns the reliability value. If method = 'irt', the a plot with the
#' reliability depending on the factorscore is returned. 
#' 
#' @details The function returns the empirical reliability of factor scores. If
#' \eqn{\sigma^2} is the sample variance of the estimated scores and \eqn{\bar{sigma}^2_{error}}
#' is the average if the squared scores, that is
#' 
#' \deqn{\bar{sigma}^2_{error}=\frac{1}{N}\sum^N_i=1 se_{scores}^2}
#' 
#' then the subtraction method, a classical reliability estimate similar to
#' classical test theory is returned using 'sub' yields
#' \deqn{\frac{\sigma^2-\bar{sigma}^2_{error}}{\sigma^2}}
#' for the reliability of the scores. If 'div' is chosen, and alternative
#' division based approach is used.
#' \deqn{\frac{\sigma^2}{\sigma^2+\bar{sigma}^2_{error}}}.
#' 
#' If 'irt' is chosen, a plot returning the standard error of the scores with
#' the scores is returned per factor.
#' 
### Examples ----
#' @examples
#' 
#' # read and save data set FC
#' data(FC_scores)
#' 
#' # get reliability
#' reliabiltyFS(FC_scores[,c(106,108,110)],FC_scores[,c(107,109,111)])
#' 
#' @export

reliabiltyFS <- function(scores, scores_se, method = 'sub'){
  
  if(!is.matrix(scores)&!is.data.frame(scores)){
    scores <- matrix(scores)
    scores_se <- matrix(scores_se)
  }
  x <- apply(scores,2,stats::var)
  y <- apply(scores_se^2,2,mean)
  rels <- (x-y)/x
  relsa <- x/(x+y) # alternative (Brown & Maydeu-Olivares, 2018)
  
  names(rels) <- names(relsa) <- paste0('F',1:length(rels))
  if(method=='sub'){
    return(rels)
  }
  else if(method == 'div'){
    return(relsa)
  }
  else if(method == 'irt'){
    pl <- list()
    for(i in 1:dim(scores)[2]){
      pl[[i]] <- plot(scores[,i],scores_se[,i], xlab = expression(theta), ylab=paste0('standard error F',i))
    }
    return(pl)
  }
}


