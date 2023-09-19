### Roxygen-izable Documentation ----
#' Performs Mplus estimation of the given model.
#' 
#' This function writes the Mplus syntax given the specifications of a
#' Thurstonian forced choice design. Additionally it runs the code
#' (given Mplus is installed) and returns the results.
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
#' @param input_path Path to save the Mplus input file. Defaults to `'myFC_model.inp'`.
#' @param output_path Path to the Mplus output file. Defaults to `'myFC_model.out'`.
#' @param data_path Path of the data file for Mplus. Defaults to `'myDataFile.dat'`.
#' @param fscore_path Path to save the file of factor scores. Defaults to
#' `'myFactorScores.dat'`.
#' @param title Title of the Mplus model. Defaults to `'myFC_model'`.
#' @param ID Logical. Should a ID variable be included? The ID must be the first 
#' variable in the data set. Defaults to `FALSE`.
#' @param byblock Logical. Should the order in Mplus variable statement be the same
#' as in the blocks. Defaults to `TRUE`.
#' @param estimator Which estimator should be used? All Estimators that are
#' available in Mplus can be used. Defaults to `'ULSMV'`.
#' @param data_full Logical. Are the data considered to be from a full design?
#' Defaults to `FALSE`.
#' @param standardized Logical. Should standardized values be computed? Defaults
#' to `TRUE`.
#' @param rename_list A list with two vectors to rename the objects in the syntax.
#' Vector one is the original names, vector two the new names. Defaults to `NULL`.
#' @param ... Further arguments passed to function `read.mplus`.
#' 
### Outputs ---- 
#' @return Returns a list containing the specified results, after model analysis.
#' 
#' @details The syntax currently is able to perform model analysis for the
#' latent utility models (`'simple'` and `'lmean'`; Maydeu-Olivares & Böckenholt, 2005) the 
#' unconstrained factor model (`'uc'`; Maydeu-Olivares & Böckenholt, 2005) and 
#' the IRT model(`'irt'`; Maydeu-Olivares & Brown, 2010). Additionally, all
#' model types can be performed with all types of forced choice designs (full,
#' block, partially linked block, linked block). For an overview and review see
#' Jansen and Schulze (2023a,2023b).
#' 
#' The function writes and saves the Mplus input files, keeps the output files 
#' and reads the results specified for the function `read.mplus`.
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
#' data(FC)
#' write.table(FC,paste0(tempdir(),'/','my_data.dat'),quote=FALSE, sep=" ",
#' col.names = FALSE, row.names = FALSE)
#' 
#' # set seed and define blocks
#' set.seed(1)
#' blocks <- matrix(sample(1:15,15), ncol = 3)
#' 
#' # define the item-to-factor relation
#' itf <- rep(1:3,5)
#' 
#' # perform analysis 
#' \dontrun{
#' fit.mplus(blocksort(blocks),itf,'irt',data_path = 'mydata.dat', data_full = TRUE,
#' input_path = paste0(tempdir(),'/','myFC_model'))
#' }
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


fit.mplus <- function(blocks,itf,model,input_path,output_path=NULL,data_path='myDataFile.dat',fscore_path='myFactorScores.dat',title='myFC_model',
                      ID=FALSE, byblock=TRUE,estimator='ULSMV',data_full=FALSE,standardized=TRUE,rename_list=NULL,...){
  if(is.null(input_path)){
    stop('You need to specify a Mplus input file in object input_path.')
  }
  if(!is.matrix(blocks)&!is.vector(blocks)){
    stop('Blocks must be given as a matrix. Only for full designs a vector is possible.')
  }
  if(is.vector(blocks)){
    blocks <- matrix(blocks,nrow=1)
  }
  if(is.null(output_path)){
    output_path <- sub('.inp','.out',input_path)
  }
  
  design <- ifelse(dim(blocks)[1]==1,'full','block')
  
  syntax.mplus(blocks,itf,model,input_path,data_path,fscore_path,title,
               ID,byblock,estimator,data_full,standardized,rename_list)
  
  path <- paste0('mplus ',input_path)
  
  system(path, wait=TRUE,show.output.on.console=FALSE)
  
  if(model=='irt'|model=='uc'){
    results <- read.mplus(blocks,itf,model,output_path,intercept = F,...)
  }else if(model=='lmean'|model=='simple2'|model=='simple3'|model=='simple5'){
    results <- read.mplus(blocks,itf,model,output_path,threshold = F,loading=F,cor=F,...)
  }
  
  return(results)

}
