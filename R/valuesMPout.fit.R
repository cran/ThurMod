valuesMPout.fit <- function(locator,offset,name,rmsea_conf,MPoutput,convergence){
  output <- list()
  for (i in 1:(length(locator))) {
    tmp <- gsub('[*]','',gsub('\\s+','',gsub(' <= .05','',gsub('[A-Za-z-]+','',MPoutput[grep(locator[i],MPoutput)+offset[i]]))))
    if(convergence==1){
      output[[name[i]]] <- as.numeric(tmp)
    }else{
      output[[name[i]]] <- NA
    }
  }
  if(rmsea_conf&convergence==1){
    tmp <- strsplit(gsub('\\s+',' ',MPoutput[grep('^RMSEA',MPoutput)+3]),' ')[[1]]
    output['rmsea.ci.lower'] <- as.numeric(tmp[5])
    output['rmsea.ci.upper'] <- as.numeric(tmp[6])
  }else{
    output['rmsea.ci.lower'] <- NA
    output['rmsea.ci.upper'] <- NA
  }

  return(output)
}
