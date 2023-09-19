get.xblocks.random <- function(nrank, blocks, item_not){
  warn <- 0
  
  # If some items should not be used first, separate blocks
  if(!is.null(item_not)){
    tmp <- apply(matrix(blocks%in%item_not,ncol=nrank),1,any)
    not_blocks <- matrix(blocks[tmp,],ncol=nrank)
    not_left <- split.matrix(not_blocks,1,nrank)
    blocks <- blocks[!tmp,]
  }
  
  # Init
  tmp_block <- matrix(nrow=0,ncol=nrank)
  if(length(blocks)>0){
    left <- split.matrix(blocks,1,nrank)
  }else{
    left <- list()
  }
  

  
  # First iteration of connecting
  while(length(left)>=nrank){
    tmp <- left[1:nrank]
    tmp_left <- unlist(tmp)
    left[1:nrank] <- NULL
    tmp_choose <- matrix(unlist(lapply(tmp, sample,size=1)),ncol=nrank,byrow = T)
    tmp_block <- rbind(tmp_block,tmp_choose)
    left[[(length(left)+1)]] <- tmp_left
  }
  
  # If some items should not be used first, 1. connect them
  if(!is.null(item_not)){
    while(length(not_left)>=nrank){
      tmp <- not_left[1:nrank]
      tmp_left <- unlist(tmp)
      not_left[1:nrank] <- NULL
      tmp_choose <- c()
      for(i in 1:length(tmp)){
        tmp2 <- which(!tmp[[i]]%in%item_not)
        tmptmp <- choose.item(tmp,tmp2,i,F,NULL, NULL,0)
        tmp3 <- tmptmp[[1]]
        tmp_choose <- c(tmp_choose,tmp3)
      }
      tmp_choose <- matrix(tmp_choose,ncol=nrank)
      tmp_block <- rbind(tmp_block,tmp_choose)
      not_left[[(length(not_left)+1)]] <- tmp_left
    }
    # 2. add them to the rest
    left <- c(not_left,left)
    
    # 3. connect blocks with as least different keyed blocks as possible
    while(length(left)>=nrank){
      tmp <- left[1:nrank]
      tmp_left <- unlist(tmp)
      left[1:nrank] <- NULL
      any_not <- unlist(lapply(lapply(tmp, function(x) x%in%item_not),any))
      whole_not <- unlist(lapply(lapply(tmp, function(x) x%in%item_not),all))
      tmp_choose <- c()
      if(any(whole_not)&all(any_not)){
          for(i in 1:length(tmp)){
            tmp2 <- which(!tmp[[i]]%in%item_not)
            tmptmp <- choose.item(tmp,tmp2,i,F,NULL, NULL,0)
            tmp3 <- tmptmp[[1]]
            tmp_choose <- c(tmp_choose,tmp3)
        }
      }else{
        for(i in 1:length(tmp)){
          tmp2 <- which(tmp[[i]]%in%item_not)
          tmptmp <- choose.item(tmp,tmp2,i,F,NULL, NULL,0)
          tmp3 <- tmptmp[[1]]
          tmp_choose <- c(tmp_choose,tmp3)
        }
      }
      tmp_block <- rbind(tmp_block,matrix(tmp_choose,ncol=nrank))
      left[[(length(left)+1)]] <- tmp_left
    }
  }
  
  # Last block
  if(length(left)!=1){
    tmp <- left
    tmp_choose <- c()
    missing <- nrank-length(left)
    # If some items should not be used first
    if(!is.null(item_not)){
      any_not <- unlist(lapply(lapply(tmp, function(x) x%in%item_not),any))
      whole_not <- unlist(lapply(lapply(tmp, function(x) x%in%item_not),all))
      if(any(whole_not)==T){
        for(i in 1:length(left)){
          tmp3 <- sample(tmp[[i]],size=1)
          tmp_choose <- c(tmp_choose,tmp3)
        }
      }else{
        for(i in 1:length(left)){
          tmp2 <- which(tmp[[i]]%in%item_not)
          tmptmp <- choose.item(tmp,tmp2,i,F,NULL, NULL,0)
          tmp3 <- tmptmp[[1]]
          tmp_choose <- c(tmp_choose,tmp3)
        }
      }
      num_items <- rev(order(unlist(lapply(left,length))))
      for(i in 1:missing){
        j <- ifelse(i%%length(left)==0,length(left),i%%length(left))
        tmp2 <- which(left[[j]]%in%item_not)
        tmp_choose[length(left)+i] <- sample(unlist(left[num_items[j]])[-which(unlist(left[num_items[j]])%in%c(item_not,tmp_choose))], size=1)
      }
      tmp_choose <- matrix(tmp_choose,ncol=nrank)
      tmp_block <- rbind(tmp_block,tmp_choose)
      left <- unlist(left)
    }else{# else
      tmp_choose <- matrix(unlist(lapply(left, sample,size=1)),ncol=nrank-missing,byrow = T)
      tmp <- left
      num_items <- rev(order(unlist(lapply(left,length))))
      for(i in 1:missing){
        j <- ifelse(i%%length(left)==0,length(left),i%%length(left))
        tmp_choose[length(left)+i] <- sample(unlist(left[num_items[j]])[-which(unlist(left[num_items[j]])%in%c(tmp_choose))], size=1)
      }
      tmp_choose <- matrix(tmp_choose,ncol=nrank)
      tmp_block <- rbind(tmp_block,tmp_choose)
      left <- unlist(left)
    }
  }
  return(tmp_block)
}

