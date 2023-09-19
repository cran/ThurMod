### Roxygen-izable Documentation ----
#' Estimate factor scores based on Genuine Likelihood
#' 
#' This function estimates factor scores based on genuine likelihood (Yousfi, 2019).
#' 
#' 
### Inputs ----
#' @param dat A data.frame or matrix defining holding the binary coded and named
#' response data.
#' @param blocks A matrix defining the blocks of the model. The number of rows
#' must be the number of blocks, each row represents a block and contains the
#' item numbers. The number of columns present the number of items per block.
#' @param itf A vector defining the items-to-factor relation. For example
#' `c(1,1,1,2,2,2)` defines six items, the first three correspond to factor 1,
#' the second three correspond to factor 2. 
#' @param fit The fit object. In case of Mplus, this is the path to the Mplus output file. In case of lavaan this is the lavaan Object.
#' @param alg The algorithm to use for numerical integration. See ?mvtnorm::pmvnorm.
#' @param log logical. Should the log transformed results be returned. Defaults to TRUE.
#' @param mp The means of prior distribution. Defaults to multivariate normal with means 0 and vars 1
#' @param sp The vars of prior distribution. Defaults to multivariate normal with means 0 and vars 1
#' @param serr Logical. Should the standard errors be returned. Defaults to TRUE.
#' @param sv Starting values. Defaults to NULL.
#' @param blocks_ul Similar to `blocks`, but only with the unlinked design. Useful
#' for comparison with other functions based on R.
#' @param mplus Logical. Are results read from mplus? Defaults to FALSE.
#' @param ... other arguments passed by get.scores for optim.
#' 
### Outputs ---- 
#' @return The result is a list with the factor scores and the standard errors per person and factor.
#' 
#' @details The function estimates factor scores based on the Thurstonian IRT model
#' and based on genuine likelihood proposed by Yousfi (2019). The function allows
#' for the estimation of factor scores under all block designs, limited only by 
#' numerical integration procedures. For items per block between 5 and 20 use 
#' `alg=mvtnorm::Miwa()`. Additionally, all model types can be performed with all
#' types of forced choice designs (full, block, partially linked block, linked
#' block). For an overview and review see Jansen and Schulze (2023a,2023b).
#' 
#' The standard naming procedure ixiy, for the comparison of items x and y.
#' 
### Examples ----
#' @examples
#' 
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
#' \donttest{fit <- fit.lavaan(blocks, itf, 'irt', FC, estimator = 'ULSMV')
#' 
#' # get scores for the first two respondents
#' ests <- get.scores(dat=as.matrix(FC)[1:2,],itf=itf, blocks=blocks,
#' fit = fit,mp=rep(0,max(itf)), 
#' sp=diag(1,max(itf)),estimator="MAP",sv=NULL, alg=mvtnorm::Miwa(),log=TRUE, mplus=FALSE)}
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
#' Yousfi, S. (2019). Person Parameter Estimation for IRT Models of Forced-Choice Data: Merits and Perils of Pseudo-Likelihood Approaches. In: Wiberg, M., Molenaar, D., González, J., Böckenholt, U., Kim, JS. (eds) \emph{Quantitative Psychology}. IMPS 2019. Springer Proceedings in Mathematics & Statistics, vol 322. Springer, Cham. \doi{10.1007/978-3-030-43469-4_3}.
#'
#' @export


get.scores <- function(dat, blocks, itf, fit, alg = mvtnorm::TVPACK(), log = TRUE, mp, sp, serr=TRUE, sv=NULL,blocks_ul=NULL,mplus=FALSE,...){
  if (is.vector(blocks)) {
    blocks <- matrix(blocks, nrow = 1)
  }
  nrank <- dim(blocks)[2]
  
  nfactor <-  max(itf)
  nitem <- max(blocks)
  
  if(mplus==TRUE){
    resul <- read.mplus(blocks,itf,model='irt',fit,intercept=F)
    
    thres <- unlist(lapply(resul$thres,function(x) x[1]))
    names(thres) <- sub('\\$','',sub('1$','',names(thres)))
    names(thres) <- sub('I','i',names(thres))
    
    loadmat <- unlist(lapply(resul$loadings,function(x) x[1]))
    loadmat <- loadmat[-grep('_',names(loadmat))]
    loadmat <- loadmat[order(as.numeric(sub('L','',names(loadmat))))]
    lambda <- matrix(0,nrow=nitem,ncol=nfactor)
    for(i in 1:nfactor){
      lambda[,i] <- ifelse(itf==i,loadmat,0)
    }
    lo <- lambda
    
    unis <- unlist(lapply(resul$resvar,function(x) x[1]))
    names(unis) <- sub('I','i',names(unis))
    
    if(!is.null(blocks_ul)){
      thres <- thres[which(names(thres)%in%i.name(blocks_ul))]
      unis <- unis[which(names(unis)%in%i.name(blocks_ul))]
      blocks <- blocks_ul
    }
    res <- diag(unis)
  }else{
    x <- lavaan::lavInspect(fit, what = "list")
    thres <- unlist(x[x$op=='|','est'])
    names(thres) <- unlist(x[x$op=='|','lhs'])
    thres <- thres[i.name(blocks)]
    
    loadmat <- unlist(x[x$label%in%paste0('L',1:nitem),'est'])
    names(loadmat) <- unlist(x[x$label%in%paste0('L',1:nitem),'label'])
    loadmat <- loadmat[paste0('L',1:nitem)]
    
    lambda <- matrix(0,nrow=nitem,ncol=nfactor)
    for(i in 1:nfactor){
      lambda[,i] <- ifelse(itf==i,loadmat,0)
    }
    lo <- lambda
    
    unis <- unlist(x[x$label%in%gsub('i','e',i.name(blocks)),'est'])
    names(unis) <- i.name(blocks)
    
    if(!is.null(blocks_ul)){
      thres <- thres[which(names(thres)%in%i.name(blocks_ul))]
      unis <- unis[which(names(unis)%in%i.name(blocks_ul))]
      blocks <- blocks_ul
    }
    res <- diag(unis)
  }
  


  
  # Tnb eq 5
  matT <- diag(nrank)
  matT[(1:(nrank-1))*(nrank+1)] <- -1 
  matT[nrank,] <- 1
  
  # create orders of responses from the 01 format
  ord <- lapply(1:nrow(dat), function(y) apply(blocks,1,function(x) pord(dat[y,i.name(x)],x)))
  combs <- cbind(utils::combn(1:nrank,2),utils::combn(1:nrank,2)[2:1,])
  ord_o <- lapply(1:nrow(dat),function(w) apply(ord[[w]],2,function(x) sapply(1:(nrank-1), function(ind, comb) which(apply(comb, 2, function(y) all(y==x[ind:(ind+1)]))), comb=combs)))
  
  # Order intercepts/thresholds once
  thres_bs <- apply(blocks,1,function(x) thres[i.name(x)])
  
  # initialize
  fs <- se <- matrix(NA, nrow(dat), dim(lo)[2])
  if(is.null(sv)) sv <- matrix(0, nrow(dat), dim(lo)[2])
  
  # factor scores for every person
  for(i in 1:nrow(dat)) {
    result <- stats::optim(par=sv[i,],fn=glh, hessian=serr,control = list(fnscale=-1,maxit=2), 
                    lower=rep(-8,dim(lo)[2]),upper=rep(8,dim(lo)[2]),method="L-BFGS-B",
                    dat=dat[i,],nrank=nrank,blocks=blocks,thres_bs=thres_bs,lo=lo,res=res,
                    matT=matT,ord=ord[[i]],ord_o=ord_o[[i]],mp=mp,sp=sp,alg=alg,log=log,
                    ...)
    fs[i,] <- result$par
    if(isTRUE(serr)) se[i,] <- sqrt(diag(MASS::ginv(-result$hessian)))
  }
  return(list("fs"=fs, "se"=se))
}




