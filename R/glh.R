# Genuine Likelihood
# Estimates Genuine Likelihood based on (Yousfi, 2019)
# Likelihood per person

glh <- function(fs, dat, nrank, blocks, thres_bs, lo, res, matT, ord, ord_o, estimator="MAP", mp, sp, alg, log) {
  like <- 0
  
  #loop over all blocks (also additional)
  for (k in 1:nrow(blocks)) {
    b <- blocks[k,]
    like <- like + bglh(fs=fs,nrank=nrank,thres_b=c(thres_bs[,k],-thres_bs[,k]),lo_b=lo[b,],res_b=res[b,b],
                      matT=matT,ord_b=ord[,k],ord_o=ord_o[,k],alg=alg,log=log)
  }
  if(estimator=="MAP") {
    return(mvtnorm::dmvnorm(x=fs, mean=mp, sigma=sp, log=TRUE) + like)
  } else {
    return(like)
  }
}
