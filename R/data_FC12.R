### Roxygen-inzable documentation
#' Paired comparisons of $N=12$ items from one factor/trait (Thurstonian modeling)
#'
#' This data set contains synthetic data of 1000 participants on all binary
#' indicators of 12 items. For each paired comparison, participants had to rank
#' the two alternative items according to their preference. It is assumed that 
#' transitivity holds (that is, the data comes from a ranking task). More
#' details can be found in Brown and Maydeu-Olivares (2011), Jansen and Schulze
#' (2023) and Maydeu-Olivares and Böckenholt (2005).
#'
#' @name FC12
#' 
#' @docType data
#' 
#' @keywords datasets
#'
#' @usage data(FC12)
#'
#' @format A data frame with 1000 observations on 66 variables. For a variable
#' ixiy, the result is the response preferences between item x and item y. It is
#' coded a 1, if item x is preferred over item y, and 0 otherwise.
#' 
#'
#' @references 
#' Brown, A, & Maydeu-Olivares, A. (2011). Item response modeling of forced-choice questionnaires. \emph{Educational and Psychological Measurement}, \emph{71}(3), 460-502. \doi{10.1177/0013164410375112}
#' 
#' Jansen, M. T., & Schulze, R. (in review). \emph{The Thurstonian linked block design: Improving Thurstonian modeling for paired comparison and ranking data}.
#' 
#' Maydeu-Olivares, A., & Böckenholt, U. (2005). Structural equation modeling of paired-comparison and ranking data. \emph{Psychological Methods}, \emph{10}(3), 285-304. \doi{10.1037/1082-989X.10.3.285}.
#'
#data("FC")
#FC12 <- FC[,i.name(1:12)]
#save(FC12, file="data/FC12.rda")
"FC12"



