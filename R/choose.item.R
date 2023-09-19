choose.item <- function(tmp,tmp2,i, multidim, itf=NULL, tmp_itf=NULL,warn=0){
  
  if(length(tmp2)==(length(tmp[[i]])-1)){
    tmp3 <- tmp[[i]][-tmp2]
  }else if(length(tmp2)>0&multidim){
    if(length(tmp[[i]])==length(tmp2)){
      tmp2 <- which(itf[tmp[[i]]]%in%tmp_itf)
      if(length(tmp[[i]])==length(tmp2)){
        stop("There are not enough items to choose from for multidimensional blocks. This is most likly due to nfactors < nrank.")
      }else if(length(tmp2)>0){
        tmp3 <- sample(tmp[[i]][-tmp2],size=1)
        warn <- warn+1
      }else{
        tmp3 <- sample(tmp[[i]],size=1)
      }
    }else{
      tmp3 <- sample(c(tmp[[i]][-tmp2]),size=1)
    }
  }else{
    tmp3 <- sample(tmp[[i]],size=1)
  }
  return(list(tmp3,warn))
}

