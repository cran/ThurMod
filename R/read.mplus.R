### Roxygen-izable Documentation ----
#' Reads results from Mplus output file.
#' 
#' This function reads and returns results from an Mplus output file.
#' 
#' 
### Inputs ----
#'
#' @param blocks A matrix defining the blocks of the model. The number of rows
#' must be the number of blocks, each row represents a block and contains the
#' item numbers. The number of columns present the number of items per block.
#' @param itf A vector defining the items-to-factor relation. For example
#' `c(1,1,1,2,2,2)` defines six items, the first three correspond to factor 1,
#' the second three correspond to factor 2. 
#' @param model A descriptor for the model. Can be one of `'lmean'`,
#' `'uc'`, `'irt'` or  `'simple2'`, `'simple3'` or `'simple5'`. The Number behind 
#' the `'simple'` statement defines the Thurstone case.
#' @param output_path Path to the Mplus output file. Defaults to `'myFC_model.out'`.
#' @param convergence Logical. Should a message for convergence be returned?
#' Defaults to `TRUE`.
#' @param fit.stat Logical. Should fit statistics be returned? Defaults to `TRUE`.
#' @param loading Logical. Should loading estimates be returned? Defaults to `TRUE`.
#' @param cor Logical. Should latent correlation estimates be returned?
#' Defaults to `TRUE`.
#' @param intercept Logical. Should intercepts be returned? Does only work for
#' `model = 'lmean'`. Defaults to `TRUE`.
#' @param threshold Logical. Should thresholds be returned? Does only work for
#' `model = 'uc'` or `'irt'`. Defaults to `TRUE`.
#' @param resvar Logical. Should residual variances be returned? Defaults to `TRUE`.
#' @param standardized Logical. Should standardized values be returned? Defaults
#' to `FALSE`.
#' 
### Outputs ---- 
#' @return Returns a list containing the specified results, after model analysis, by reading
#' the results from the 'output_path'.
#' 
### Examples ----
#' @examples
#' 
#' # read and save data set FC
#' data(FC)
#' 
#' write.table(FC,paste0(tempdir(),'/','my_data.dat'),quote=FALSE, sep=" ",
#' col.names = FALSE, row.names = FALSE)
#' 
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
#' 
#' 
#' # After estimation
#' read.mplus(blocks,itf,'irt',output_path = paste0(tempdir(),'/','myFC_model.out'))
#' }
#' @export


### Function definition ----
read.mplus <- function(blocks,itf,model,output_path,convergence=TRUE,fit.stat=TRUE,loading=TRUE, cor=TRUE, intercept=TRUE, threshold=TRUE, resvar=TRUE,
                       standardized=FALSE){
  
  if(is.null(output_path)){
    stop('You need to specify a Mplus output file in object output_path.')
  }
  
  if(!is.matrix(blocks)&!is.vector(blocks)){
    stop('Blocks must be given as a matrix. Only for full designs a vector is possible.')
  }
  if(is.vector(blocks)){
    blocks <- matrix(blocks,nrow=1)
  }
  design <- ifelse(dim(blocks)[1]==1,'full','block')
  
  # Find number of items per block
  nrank <- dim(blocks)[2]
  
  # Find number of factors, blocks and items
  nblock <- dim(blocks)[1]
  nitem <- length(unique(c(blocks)))
  nfactor <- max(itf)
  
  
  results <- list()
  MPoutput <- readLines(output_path)
  #MPoutput <- readLines('myFC_model3.out')
  mod <- mod.matrices(blocks,itf,model)
  if(convergence){
    if (!any(grepl('MODEL FIT',MPoutput))) {
      if (any(grepl('NO CONVERGENCE.',MPoutput))|any(grepl('THE MODEL MAY NOT BE IDENTIFIED.',MPoutput))) {
        convergence <- 0
      }else{
        convergence <- 0
      }}else {
        convergence <- 1
      }
    results$converge <- convergence
  }
  
  if(fit.stat){
    locator <- c('^SRMR',rep('^CFI/TLI',2),rep('^Chi-Square Test of Model Fit$',3),
                 rep('^Chi-Square Test of Model Fit for the Baseline Model',3),rep('^RMSEA',2))
    offset <- c(2,2,3,2,3,4,2,3,4,2,4)
    name <- c('srmr','cfi','tli','chisq','df','pvalue','blchisq','bldf','blpvalue','rmsea','rmsea_p')
    results$fit <- valuesMPout.fit(locator,offset,name,TRUE,MPoutput,convergence)
  }
  
  if(loading){
    lmloc <- c()
    lmoff <- c()
    lmname <- c()
    if(model=='irt'){
      mod$lambda <- ifelse(mod$lambda=='',NA,mod$lambda)
      lmlength <- dim(mod$lambda)[1]-matrixStats::colCounts(mod$lambda,value=NA)
      for(i in 1:nfactor){
        lmoff <- c(lmoff,1:lmlength[i])
        lmloc <- c(lmloc,rep(paste0('^ TRAIT',i,'\\s+BY'),lmlength[i]))
        #tmp <- mod$lambda[-which(is.na(mod$lambda[,i])),i]
        tmp <- mod$lambda[,i]
        
        #mod$lambda[ifelse(length(which(is.na(mod$lambda[,i])))==0,NA,-which(is.na(mod$lambda[,i]))),i]
        
        lmname <- c(lmname,tmp)
      }
      if(!standardized){
        lmloc <- c(lmloc,rep('^New/Additional Parameters',mod$naddparam))
        lmoff <- c(lmoff,1:mod$naddparam)
        lmname <- c(lmname,unlist(mod$addparam))
        if(length(which(is.na(lmname)))!=0){
          lmname <- lmname[-which(is.na(lmname))]
        }
      }
    }else if(model=='lmean'|model=='uc'){
      for(i in 1:nfactor){
        lmoff <- c(lmoff,1:(length(mod$lambda[[i]])-1))
        lmloc <- c(lmloc,rep(paste0('^ TRAIT',i,'\\s+BY'),length(mod$lambda[[i]])-1))
        tmp <- sub('\\*.','',mod$lambda[[i]][-1])
        lmname <- c(lmname,tmp)
      }
    }
    results$loadings <- valuesMPout(lmloc,lmoff,lmname,MPoutput,convergence, standardized)
  }
  
  if(cor&nfactor>1){
    corloc <- c()
    coroff <- c()
    for(i in 1:(nfactor-1)){
      tmp <- rep(paste0('^ TRAIT',i,'\\s+WITH'),nfactor-i)
      corloc <- c(corloc,tmp)
      coroff <- c(coroff,1:(nfactor-i))
    }
    corname <- paste0('TRAIT',utils::combn(1:nfactor,2)[1,],'TRAIT',utils::combn(1:nfactor,2)[2,])
    results$cor <- valuesMPout(corloc,coroff,corname,MPoutput,convergence, standardized)
  }
  
  if(model=='simple2'|model=='simple3'|model=='simple5'){
    corloc <- c()
    coroff <- c()
    for(i in 1:(nitem-2)){
      tmp <- rep(paste0('^ T',i,'\\s+WITH'),nitem-i)
      corloc <- c(corloc,tmp)
      coroff <- c(coroff,1:(nitem-i))
    }
    if(model=='simple2'){
      tmp <- rep(paste0('^ T',nitem,'\\s+WITH'),1)
    }else{
      tmp <- rep(paste0('^ T',nitem-1,'\\s+WITH'),1)
    }
    
    corloc <- c(corloc,tmp)
    coroff <- c(coroff,1)

    corname <- paste0('T',utils::combn(1:nitem,2)[1,],'T',utils::combn(1:nitem,2)[2,])
    results$cor <- valuesMPout(corloc,coroff,corname,MPoutput,convergence, standardized)
  }

  if(intercept){
    if(model!='lmean'&model!='simple2'&model!='simple3'&model!='simple5'){
      warning('Intercepts can only be retrieved from a latent mean model!')
    }
    if(model=='lmean'){
      intloc <- c(rep('^ Intercepts',nitem))
    }else{
      intloc <- c(rep('^ Means',nitem))
    }
    intoff <- c(1:nitem)
    intname <- paste0('T',c(1:nitem))
    results$int <- valuesMPout(intloc,intoff,intname,MPoutput,convergence, standardized)
  }
  
  if(threshold){
    threshloc <- c(rep('^ Thresholds',mod$npair))
    threshoff <- c(1:mod$npair)
    threshname <- paste0(mod$pair_names_b,'$1')#paste0("I$", c(1:mod$npair))
    results$thres <- valuesMPout(threshloc,threshoff,threshname,MPoutput,convergence, standardized)
  }
  
  if(resvar){
    if(model=='lmean'|model=='uc'){
      varloc <- c(rep('^ Residual Variances',(mod$npair+nitem)))
      varoff <- c(1:(mod$npair+nitem))
      varname <- c(mod$pair_names_b,paste0('T$',c(1:nitem)))
    } else if(model=='irt'){
      varloc <- c(rep('^ Residual Variances',mod$npair))
      varoff <- c(1:mod$npair)
      varname <- mod$pair_names_b#paste0("I$", c(1:mod$npair))
    }else if(model=='simple2'|model=='simple3'|model=='simple5'){
      varloc <- c(rep('^ Variances',(nitem)))
      varloc <- c(varloc,rep('^ Residual Variances',(mod$npair)))
      varoff <- c(1:(nitem))
      varoff <- c(varoff,1:(mod$npair))
      varname <- c(paste0('T$',c(1:nitem)),mod$pair_names_b)
    }
    results$resvar <- valuesMPout(varloc,varoff,varname,MPoutput,convergence, standardized)
  }
  return(results)
}






