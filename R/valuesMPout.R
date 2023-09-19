valuesMPout <- function(locator,offset,name,MPoutput, convergence, standardized){
  output <- list()
  for (i in 1:(length(locator))) {
    tmp <- ifelse(standardized==T,max(grep(locator[i],MPoutput)),min(grep(locator[i],MPoutput)))
    tmp <- MPoutput[tmp+offset[i]]
    tmp <- gsub('\\s+',' ',tmp)
    tmp <- strsplit(tmp,' ')[[1]]
    tmp <- tmp[3:length(tmp)] # Estimate,SE,Est/SE, p
    tmp <- ifelse(tmp=="*********",NA,tmp)
    if(convergence==1){
      output[[name[i]]] <- as.numeric(tmp)
    }else{
      output[[name[i]]] <- NA
    }
  }
  return(output)
}

