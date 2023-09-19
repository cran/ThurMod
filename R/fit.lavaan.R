### Roxygen-izable Documentation ----
#' Performs lavaan estimation of the given model.
#' 
#' This function writes a lavaan syntax given the specifications of the
#' Thurstonian forced choice model. Additionally it runs the code (given lavaan
#' is installed) and returns the results.
#' 
#' 
### Inputs ----
#' @param blocks A matrix defining the blocks of the model. The number of rows
#' must be the number of blocks, each row represents a block and contains the
#' item numbers. The number of columns present the number of items per block.
#' @param itf A vector defining the items-to-factor relation. For example
#' `c(1,1,1,2,2,2)` defines six items, the first three correspond to factor 1,
#' the second three correspond to factor 2. 
#' @param model A descriptor for the model. Can be one of `'lmean'`,
#' `'uc'`, `'irt'` or  `'simple2'`, `'simple3'` or `'simple5'`. The Number behind 
#' the `'simple'` statement defines the Thurstone case.
#' @param data A matrix or data frame including the binary indicators as columns
#' and respondents as rows.
#' @param estimator Which estimator should be used? All estimators that are
#' available in `lavaan` can be used. Defaults to `'ULSMV'`.
#' @param rename_list A list with two vectors to rename the objects in the syntax.
#' Vector one is the original names, vector two the new names. Defaults to `NULL`.
#' 
### Outputs ---- 
#' @return Returns a lavaan object containing the specified results, after model
#' analysis.
#' 
#' @details The syntax currently is able to perform model analysis for the
#' latent utility model (`'simple'` and `'lmean'`; Maydeu-Olivares & Böckenholt, 2005) the 
#' unconstrained factor model (`'uc'`; Maydeu-Olivares & Böckenholt, 2005) and 
#' the IRT model(`'irt'`; Maydeu-Olivares & Brown, 2010). Additionally, all
#' model types can be performed with all types of forced choice designs (full,
#' block, partially linked block, linked block). For an overview and review see
#' Jansen and Schulze (2023a,2023b).
#' 
#' The standard naming procedure ixiy, for the comparison of items x and y, 
#' can be changed by specifying the `rename_list` argument. The first vector of
#' the schould be the vector of original names, for example 
#' `c('i1i2','i1i3','i2i3','Trait1','Trait2','Trait3')`
#' the second vector should contain the new names, for example 
#' `c('A01E12','A01C13','E01C23','Agree','Extra','Consc')`.
#' 
### Examples ----
#' @examples
#' 
#' # read and save data set FC
#' data(FC12)
#' 
#' # set seed and define blocks
#' blocks <- matrix(c(5,2,1,4,7,6,3,8,10,12,9,11), ncol = 3)
#' 
#' # define the item-to-factor relation
#' itf <- rep(1:3,4)
#' 
#' # Create and run syntax
#' \donttest{fit <- fit.lavaan(blocks, itf, 'irt', FC, estimator = 'ULSMV')}
#' 
#' @references 
#' Maydeu-Olivares, A., & Böckenholt, U. (2005). Structural equation modeling of paired-comparison and ranking data. \emph{Psychological Methods}, \emph{10}(3), 285-304. \doi{10.1037/1082-989X.10.3.285}
#'
#' Maydeu-Olivares, A., & Brown, A. (2010). Item response modeling of paired comparison and ranking data. \emph{Multivariate Behavioural Research}, \emph{45}(6), 935-974. \doi{10.1080/00273171.2010.531231}
#'  
#' Jansen, M. T., & Schulze, R. (2023a). \emph{Linear factor analytic Thurstonian forced-choice models: Current status and issues}. Educational and Psychological Measurement.
#' 
#' Jansen, M. T., & Schulze, R. (2023b, in review). \emph{The Thurstonian linked block design: Improving Thurstonian modeling for paired comparison and ranking data}. 
#' 
#'     
#' @export

fit.lavaan <- function(blocks,itf,model,data=NULL,estimator='ULSMV',rename_list=NULL){
  
  if(!is.matrix(blocks)&!is.vector(blocks)){
    stop('Blocks must be given as a matrix. Only for full designs a vector is possible.')
  }
  if(is.vector(blocks)){
    blocks <- matrix(blocks,nrow=1)
  }
  design <- ifelse(dim(blocks)[1]==1,'full','block')
  
  modelsyn <- syntax.lavaan(blocks,itf,model,rename_list)
  
  if(model=='irt'){
    results <- lavaan::lavaan(modelsyn, data = data, ordered = T, 
                               auto.fix.first = FALSE, auto.th = TRUE, parameterization = "theta", estimator=estimator)
  }else if(model=='lmean'|model=='simple2'|model=='simple3'|model=='simple5'){
    results <- lavaan::lavaan(modelsyn, data = data, ordered = T, 
                              auto.fix.first = FALSE, auto.var = TRUE, int.lv.free = TRUE, parameterization = "theta", estimator=estimator)
  }else{
    results <- lavaan::lavaan(modelsyn, data = data, ordered = T, 
                              auto.fix.first = FALSE, auto.var = TRUE, auto.th = TRUE, parameterization = "theta", estimator=estimator)
  }
  
  return(results)

}
