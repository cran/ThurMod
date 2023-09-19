### Roxygen-izable Documentation ----
#' Create data based on Thurstonian model equations
#' 
#' Simulates a data set of paired comparisons or ranking data based a Thurstonian
#' latent utility model.
#' 
### Inputs ----
#'
#' @param nfactor The number of factors. Defaults to 1.
#' @param nitem The number of items.
#' @param nperson The number of data points (= respondents) to simulate.
#' @param itf A vector defining the items-to-factor relation. For example
#' `c(1,1,1,2,2,2)` defines six items, the first three correspond to factor 1,
#' the second three correspond to factor 2. 
#' @param model The model class from which to simulate. Options are 'simple', 'factor', 'uc' and 'irt'. Defaults to 'factor'.
#' @param variables A vector containing the names of paired comparison variables
#' to return. If `NULL` (default), all variables are returned.
#' @param ints A vector defining the latent intercepts of item pairs. 
#' @param lmu A vector defining the latent means of items. 
#' @param ivarcov A matrix defining the variance-covariance matrix of the items.
#' @param loadings A vector defining the loadings of items.
#' @param varcov A matrix defining the variance-covariance matrix of the traits.
#' @param graded Logical. Should a graded preference model be simulated? Defaults to `FALSE`.
#' @param ncat Numerical. The number of categories to specify for graded preference models. If graded = `FALSE`, ncat is 2.
#' @param thres A vector of thresholds to categorize the latent difference response. If no thresholds are given, they are simulated from the distribution of the latent items. If graded = `FALSE`, the thresholds are all 0.
#' @param transitive Logical. Should the data be transitive? If `TRUE`, ranking
#' data is simulated, else paired comparison data is simulated. Defaults to `TRUE`.
#' @param var A vector containing the variances for each paired comparison. Defaults to 0.
#' @param fvalues Logical. Should simulated factor values be returned? Defaults to `FALSE`.
#' @param sim Logical. Should the simulated data be returned? Defaults to `TRUE`.
#' 
#' @details The syntax currently is able to simulate data from the
#' latent utility model (`'simple'` and `'factor'`; Maydeu-Olivares & Böckenholt, 2005) the 
#' unconstrained factor model (`'uc'`; Maydeu-Olivares & Böckenholt, 2005) and 
#' the IRT model(`'irt'`; Maydeu-Olivares & Brown, 2010).
#' 
#' 
### Outputs ---- 
#' @return Returns a list containing the true factor scores and the data, or a
#' matrix containing the data.
#' 
#' 
### Examples ----
#' @examples
#' 
#' nfactor <- 3
#' nitem <- 15
#' nperson <- 1000
#' itf <- rep(1:3,5)
#' varcov <- diag(1,3)
#' 
#' # latent utility means
#' set.seed(69)
#' lmu <- runif(nitem, -1, 1)
#' loadings <- runif(nitem, 0.30, 0.95)
#' 
#' FC <- sim.data(nfactor=nfactor, nitem=nitem, nperson=nperson, itf=itf, 
#' varcov = varcov, lmu = lmu, loadings=loadings)
#'     
#' @export

### Function definition ----
sim.data <- function(nfactor=1,nitem,nperson,itf,model='factor',variables=NULL, ints=NULL, lmu=NULL, ivarcov=NULL, loadings=NULL, varcov=NULL, graded = FALSE, ncat=NULL, thres = NULL, transitive = TRUE, var = 0, fvalues=FALSE, sim=TRUE){  
  if(model%in%c('simple')){
    if(is.null(lmu)){
      stop('For simple Thurstonian models for Cases II, III and V you must specify latent item means (object lmu).')
    }
    if(is.null(ivarcov)){
      stop('For simple Thurstonian models for Cases II, III and V you must specify a covariance matrix of latent items (object ivarcov). For Case II variances and covarainces of latent items are free. For Case III variances are free and covarainces are fixed to zero. For Case V variances are all equal and covarainces are fixed to 0.')
    }
  }
  if(model=='factor'){
    if(is.null(nfactor)){
      stop('For Thurstonian factor models you must specify the number of factors (object nfactor).')
    }
    if(is.null(lmu)){
      stop('For Thurstonian factor models you must specify latent item means (object lmu).')
    }
    if(is.null(varcov)){
      stop('For Thurstonian factor models you must specify a covariance matrix of latent traits (object varcov).')
    }
    if(is.null(loadings)){
      stop('For Thurstonian factor models you must specify the laodings of items on latent traits (object loadings).')
    }
  }
  if(model%in%c('uc','irt')){
    if(is.null(nfactor)){
      stop('For Thurstonian irt models or unconstrained threshold models you must specify the number of factors (object nfactor).')
    }
    if(is.null(ints)){
      stop('For Thurstonian irt models or unconstrained threshold models you must specify latent intercepts of item pairs (object ints).')
    }
    if(is.null(varcov)){
      stop('For Thurstonian irt models or unconstrained threshold you must specify a covariance matrix of latent traits (object varcov).')
    }
    if(is.null(loadings)){
      stop('For Thurstonian irt models or unconstrained threshold you must specify the laodings of items on latent traits (object loadings).')
    }
  }
  if(graded){
    if(is.null(thres)&is.null(lmu)){
      stop('For Thurstonian graded preference models you must specify either a covariance matrix of latent items (object ivarcov) and the latent item means (object lmu), or the thresholds (object thres).')
    }
    if(is.null(ivarcov)&is.null(thres)){
      stop('For Thurstonian graded preference models you must specify either a covariance matrix of latent items (object ivarcov) and the latent item means (object lmu), or the thresholds (object thres).')
    }
    if(is.null(ncat)&is.null(thres)){
      stop('For Thurstonian graded preference models you must specify either the thresholds (object thres) or the number of categories to simulate (object ncat).')
    }
  }
  
  # Init parameters
  npair <- nitem*(nitem-1)/2
  
  if(!is.null(loadings)){
    # Set loadings, means, factor scores and latent variances (of items)
    # loadings
    tmp_l <- loadings
    lambda <- matrix(0, ncol=nfactor, nrow = nitem)
    
    for(i in 1:nitem){
      lambda[i,itf[i]] <- tmp_l[i]
    }
  }
  
  # design
  A <- designA(nitems=nitem)
  
  # simulate latent difference response
  if(transitive==FALSE){
    eps <- t(MASS::mvrnorm(nperson, rep(0,npair), diag(var,npair))) # variance pair comparison
  }else{
    eps <- 0 # variance pair comparison; 0 for rankings
  }
  
  if(sim==T){
    if(model%in%c('simple')){
      # Case II Covs_t free; Var_t free
      # Case III Covs_t = 0; Var_t free
      # Case V Covs_t = 0; Var_t (all equal)
      t <- t(MASS::mvrnorm(nperson, lmu, ivarcov)) #eq 7 (MO&B, 2010) 
      lresp <- A%*%t
    }
    if(model=='factor'){
      # factor scores
      # make eta
      eta <- t(MASS::mvrnorm(nperson, rep(0,nfactor), varcov))
      # latent variances (items)
      psi <- t(MASS::mvrnorm(nperson, rep(0,nitem), diag(1-c(tmp_l^2),nitem)))
      
      pgam <- A%*%lmu
      At <- A%*%(lambda%*%eta+psi)
      
      lresp <- apply(At,2,function(x) x+pgam) # eq 11 (MO&B, 2010)
    }
    if(model%in%c('uc','irt')){
      # factor scores
      # make eta
      eta <- t(MASS::mvrnorm(nperson, rep(0,nfactor), varcov))
      # latent variances (items)
      psi <- t(MASS::mvrnorm(nperson, rep(0,nitem), diag(1-c(tmp_l^2),nitem)))
      
      pgam <- -ints
      At <- A%*%(lambda%*%eta+psi)
      
      lresp <- apply(At,2,function(x) x+pgam) # eq 21 (MO&B, 2010)
    }
    
    # Add variance of pairs if transitive = FALSE
    lresp <- lresp+eps
    
    if(graded){
      if(is.null(thres)){
        pairs <- t(utils::combn(1:nitem,2))
        gvars <- apply(pairs,1,function(x) ivarcov[x[1],x[1]]+ivarcov[x[2],x[2]]-2*ivarcov[x[1],x[2]])
        pmeans <- apply(pairs,1,function(x) lmu[x[2]]-lmu[x[1]]) # pair means
        thresholds <- sapply(1:length(pmeans),function(x) sort(stats::rnorm(ncat-1, pmeans[x], gvars[x])))
        
        tmp <- ncat/2==floor(ncat/2)
        
        if(tmp){
          thresholds <- apply(thresholds,2, function(x) x-x[ncat/2])
        }else{
          thresholds <- apply(thresholds,2, function(x) x-stats::runif(1,x[floor(ncat/2)],x[ceiling(ncat/2)]))
        }
      }
      if(is.vector(thres)){
        thresholds <- matrix(rep(thres,npair), ncol=length(thres),byrow = T)
      }
      resp <- sapply(1:dim(thresholds)[2],function(x) recode(t(lresp)[,x],thresholds[,x],1:ncat,cat=TRUE))
    }else{
      resp <- t(ifelse(lresp<0,0,1))
    }

    resp <- as.data.frame(resp)
    names(resp) <- i.name(1:nitem)
    
    if(!is.null(variables)){
      tmp <- which(i.name(1:nitem)%in%variables)
      resp <- resp[,tmp]
    }
    
  }else{
    resp <- 'You chose to not simulate the data.'
  }
  
  if(fvalues==T){
    return(list(resp,eta))
  }else{
    return(resp)
  }
  
}

