### Roxygen-inzable documentation
#' Raw ranking data of $N=15$ items from three factors/traits (Thurstonian modeling)
#'
#' This data set contains synthetic raw data of 1000 participants on a ranking
#' task on 15 items. 
#'
#' @name FC_raw
#' 
#' @docType data
#' 
#' @keywords datasets
#'
#' @usage data(FC_raw)
#'
#' @format A data frame with 1000 observations on 15 variables.
#data(FC)
#FC_raw <- t(sapply(1:1000, function(y) pord(FC[y,],1:15)))
#colnames(FC_raw) <- paste0('item',1:15)
#save(FC_raw, file="data/FC_raw.rda")
"FC_raw"



