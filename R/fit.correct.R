### Roxygen-izable Documentation ----
#' Correct degree of freedom and fit indices in Thurstonian block models
#' 
#' Correct fit indices (RMSEA and CFI) by correcting the degrees of freedom
#' after estimation a Thurstonian model.
#' 
#' 
### Inputs ----
#'
#' @param n The number of respondents.
#' @param blocks A matrix defining the blocks of the model. The number of rows
#' must be the number of blocks, each row represents a block and contains the
#' item numbers. The number of columns present the number of items per block.
#' @param chi2_mod The \eqn{\chi^2}{chi^2} value of the estimated model.
#' @param df_mod The degrees of freedom of the estimated model.
#' @param chi2_base The \eqn{\chi^2}{chi^2} value of the baseline model.
#' @param df_base The degrees of freedom of the baseline model.
#' 
### Outputs ---- 
#' @return Returns a vector containing corrected degrees of freedom, and the
#' corrected RMSEA and CFI values.
#' 
### Details
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
#' # Assume the model yield the following fit, with 426 respondents
#' # chi2_mod = 224.456, df_mod = 59, chi2_base = 1056.566, df_base = 90
#' 
#' fit.correct(426,blocks,224.456,59,1056.566,90)
#' 
#' # The corrected values are rmsea = 0.0917892; cfi =  0.8184749
#'
#' @references 
#' Jansen, M. T., & Schulze, R. (in review). \emph{The Thurstonian linked block design: Improving Thurstonian modeling for paired comparison and ranking data}.
#' 
#' Maydeu-Olivares, A. (1999). Thurstonian modeling of ranking data via mean and covariance structure analysis. \emph{Psychometrika}, \emph{64}(3), 325-340. \doi{10.1007/BF02294299}
#' 
#' @export

fit.correct <- function(n,blocks,chi2_mod,df_mod,chi2_base,df_base){
  df_corr <- df_mod-redundancies(blocks)
  names(df_corr) <- 'df_corr'
  rmsea_corr <- sqrt(max(((chi2_mod-df_corr)/(n-1))/df_corr,0))
  cfi_corr <- min(1-(chi2_mod-df_corr)/(chi2_base-df_base),1)
  return(c(df=df_corr,rmsea=rmsea_corr,cfi=cfi_corr))
}
