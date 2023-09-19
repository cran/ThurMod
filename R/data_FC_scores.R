### Roxygen-inzable documentation
#' Scores of the data set 'FC' from Mplus.
#'
#' This data set contains synthetic data of 1000 participants on all binary
#' indicators of 15 items and their factor scores. For each paired comparison, 
#' participants had to rank the two alternative items according to their
#' preference. It is assumed that transitivity holds (that is, the data comes
#' from a ranking task). More details can be found in Brown and Maydeu-Olivares
#' (2011), Jansen and Schulze (2023) and Maydeu-Olivares and Böckenholt (2005).
#'
#' @name FC_scores
#' 
#' @docType data
#' 
#' @keywords datasets
#'
#' @usage data(FC_scores)
#'
#' @format A data frame with 1000 observations on 111 variables. For a variable
#' ixiy, the result is the response preferences between item x and item y. It is
#' coded a 1, if item x is preferred over item y, and 0 otherwise. The last six
#' variables are the resulting factor scores and the standard error of factor 
#' scores for three factors.
#' 
#'
#' @references 
#' Brown, A, & Maydeu-Olivares, A. (2011). Item response modeling of forced-choice questionnaires. \emph{Educational and Psychological Measurement}, \emph{71}(3), 460-502. \doi{10.1177/0013164410375112}
#' 
#' Jansen, M. T., & Schulze, R. (in review). \emph{The Thurstonian linked block design: Improving Thurstonian modeling for paired comparison and ranking data}.
#' 
#' Maydeu-Olivares, A., & Böckenholt, U. (2005). Structural equation modeling of paired-comparison and ranking data. \emph{Psychological Methods}, \emph{10}(3), 285-304. \doi{10.1037/1082-989X.10.3.285}.
#'
#fit.mplus(1:15,rep(1:3,5),'irt')
#FC_scores <- read.table('myFactorScores.dat')
#save(FC_scores, file="data/FC_scores.rda")
"FC_scores"



