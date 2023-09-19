# Genuine Likelihood
# Estimates Genuine Likelihood based on (Yousfi, 2019)

bglh <- function(fs,nrank,thres_b,lo_b,res_b,matT,ord_b,ord_o,alg=mvtnorm::TVPACK(),log=T){
  means <- thres_b[ord_o] + (matT%*%(lo_b[ord_b,]%*% fs))[-nrank]
  vars <- (tcrossprod(matT%*%res_b[ord_b,ord_b], matT))[-nrank,-nrank]
  
  out <- mvtnorm::pmvnorm(lower=rep(0,nrank-1), upper=rep(Inf,nrank-1), mean=means, sigma=vars, algorithm=alg)
  
  ## out: likelihood for one block and one person
  if (log==TRUE) {
    return(ifelse(out<=0, -10^100, log(out))) #small number
  } else {
    return(ifelse(out<=0, 1e-100, out)) #number close to 0
  }
}


