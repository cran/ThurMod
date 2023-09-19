get.xblocks.multidim <- function(nrank, blocks, itf, item_not, min, show.warnings){
  warn <- 0
  if(min){
    # If some items should not be used first, separate blocks
    if(!is.null(item_not)){
      tmp <- apply(matrix(blocks%in%item_not,ncol=nrank),1,any)
      not_blocks <- matrix(blocks[tmp,],ncol=nrank)
      not_left <- split.matrix(not_blocks,1,nrank)
      blocks <- blocks[!tmp,]
    }
    
    # Init
    tmp_block <- matrix(nrow=0,ncol=nrank)
    left <- split.matrix(blocks,1,nrank)
    
    # First iteration of connecting
    while(length(left)>=nrank){
      tmp <- left[1:nrank]
      tmp_left <- unlist(tmp)
      left[1:nrank] <- NULL
      tmp_choose <- c()
      tmp_itf <- c()
      for(i in 1:nrank){
        tmp2 <- which(itf[tmp[[i]]]%in%tmp_itf)
        tmptmp <- choose.item(tmp,tmp2,i,T,itf,tmp_itf,warn)
        tmp3 <- tmptmp[[1]]
        tmp_itf <- c(tmp_itf,itf[tmp3])
        tmp_choose <- c(tmp_choose,tmp3)
      }
      tmp_block <- rbind(tmp_block,matrix(tmp_choose,ncol=nrank))
      left[[(length(left)+1)]] <- tmp_left
    }
    
    # If some items should not be used first, 1. connect them
    if(!is.null(item_not)){
      while(length(not_left)>=nrank){
        tmp <- not_left[1:nrank]
        tmp_left <- unlist(tmp)
        not_left[1:nrank] <- NULL
        whole_not <- unlist(lapply(lapply(tmp, function(x) x%in%item_not),all))
        tmp_choose <- c()
        tmp_itf <- c()
        warn <- 0
        
        if(any(whole_not)){
          for(i in 1:length(tmp)){
            tmp2 <- unique(c(which(!tmp[[i]]%in%item_not),which(itf[tmp[[i]]]%in%tmp_itf)))
            tmptmp <- choose.item(tmp,tmp2,i,T,itf,tmp_itf,warn)
            tmp3 <- tmptmp[[1]]
            warn <- tmptmp[[2]]
            tmp_itf <- c(tmp_itf,itf[tmp3])
            tmp_choose <- c(tmp_choose,tmp3)
          }
        }else{
          for(i in 1:length(tmp)){
            tmp2 <- unique(c(which(tmp[[i]]%in%item_not),which(itf[tmp[[i]]]%in%tmp_itf)))
            tmptmp <- choose.item(tmp,tmp2,i,T,itf,tmp_itf,warn)
            tmp3 <- tmptmp[[1]]
            warn <- tmptmp[[2]]
            tmp_itf <- c(tmp_itf,itf[tmp3])
            tmp_choose <- c(tmp_choose,tmp3)
          }
        }

        tmp_choose <- matrix(tmp_choose,ncol=nrank)
        tmp_block <- rbind(tmp_block,tmp_choose)
        if(warn>0&show.warnings){
          warning(paste0("The addition of one mixed key block was unavoidable. This concerns block ",nrow(tmp_block),'.'))
        }
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
        tmp_itf <- c()
        warn <- 0
        
        if(any(whole_not)&all(any_not)){
          
            for(i in 1:length(tmp)){
              tmp2 <- unique(c(which(!tmp[[i]]%in%item_not),which(itf[tmp[[i]]]%in%tmp_itf)))
              tmptmp <- choose.item(tmp,tmp2,i,T,itf,tmp_itf,warn)
              tmp3 <- tmptmp[[1]]
              warn <- tmptmp[[2]]
              tmp_itf <- c(tmp_itf,itf[tmp3])
              tmp_choose <- c(tmp_choose,tmp3)
          }
        }else{
          for(i in 1:length(tmp)){
            tmp2 <- unique(c(which(tmp[[i]]%in%item_not),which(itf[tmp[[i]]]%in%tmp_itf)))
            tmptmp <- choose.item(tmp,tmp2,i,T,itf,tmp_itf,warn)
            tmp3 <- tmptmp[[1]]
            warn <- tmptmp[[2]]
            tmp_itf <- c(tmp_itf,itf[tmp3])
            tmp_choose <- c(tmp_choose,tmp3)
          }
          
          tmp_block <- rbind(tmp_block,matrix(tmp_choose,ncol=nrank))
          if(warn>0&show.warnings){
            warning(paste0("The addition of one mixed key block was unavoidable. This concerns block ",nrow(tmp_block),'.'))
          }
          left[[(length(left)+1)]] <- tmp_left
        }
      }
    }
    
    # Last block
    tmp_choose <- c()
    tmp_itf <- c()
    warn <- 0
    if(length(left)!=1){
      missing <- nrank-length(left)
      tmp <- left
      # If some items should not be used first
      if(!is.null(item_not)){
        any_not <- unlist(lapply(lapply(tmp, function(x) x%in%item_not),any))
        whole_not <- unlist(lapply(lapply(tmp, function(x) x%in%item_not),all))
        if(any(whole_not)==T){
          for(i in 1:length(tmp)){
            tmp2 <- which(itf[tmp[[i]]]%in%tmp_itf)
            tmptmp <- choose.item(tmp,tmp2,i,T,itf,tmp_itf,warn)
            tmp3 <- tmptmp[[1]]
            warn <- tmptmp[[2]]
            tmp_itf <- c(tmp_itf,itf[tmp3])
            tmp_choose <- c(tmp_choose,tmp3)
          }
        }else{
          for(i in 1:length(tmp)){
            tmp2 <- unique(c(which(tmp[[i]]%in%item_not),which(itf[tmp[[i]]]%in%tmp_itf)))
            tmptmp <- choose.item(tmp,tmp2,i,T,itf,tmp_itf,warn)
            tmp3 <- tmptmp[[1]]
            warn <- tmptmp[[2]]
            tmp_itf <- c(tmp_itf,itf[tmp3])
            tmp_choose <- c(tmp_choose,tmp3)
          }
        }
        num_items <- rev(order(unlist(lapply(left,length))))
        for(i in 1:missing){
          j <- ifelse(i%%length(left)==0,length(left),i%%length(left))
          tmp2 <- unique(c(which(left[[j]]%in%item_not),which(itf[left[[j]]]%in%tmp_itf)))
          tmp_choose[length(left)+i] <- sample(unlist(left[num_items[j]])[-c(which(unlist(left[num_items[j]])%in%c(tmp_choose)),tmp2)], size=1)
        }
        tmp_choose <- matrix(tmp_choose,ncol=nrank)
        tmp_block <- rbind(tmp_block,tmp_choose)
        left <- unlist(left)
      }else{#else
        for(i in 1:length(tmp)){
          tmp2 <- which(itf[tmp[[i]]]%in%tmp_itf)
          tmptmp <- choose.item(tmp,tmp2,i,T,itf,tmp_itf,warn)
          tmp3 <- tmptmp[[1]]
          tmp_itf <- c(tmp_itf,itf[tmp3])
          tmp_choose <- c(tmp_choose,tmp3)
        }
        num_items <- rev(order(unlist(lapply(left,length))))
        for(i in 1:missing){
          j <- ifelse(i%%length(left)==0,length(left),i%%length(left))
          
          tmp2 <- which(itf[tmp[[j]]]%in%tmp_itf)
          tmptmp <- choose.item(tmp,tmp2,i,T,itf,tmp_itf,warn)
          tmp3 <- tmptmp[[1]]
          tmp_itf <- c(tmp_itf,itf[tmp3])
          tmp_choose <- c(tmp_choose,tmp3)
        }
        
        tmp_choose <- matrix(tmp_choose,ncol=nrank)
        tmp_block <- rbind(tmp_block,tmp_choose)
        if(warn>0&show.warnings){
          warning(paste0("The addition of one mixed key block was unavoidable. This concerns block ",nrow(tmp_block),'.'))
        }
        left <- unlist(left)
      }
    }
  }else{
    # Init
    tmp_block <- matrix(nrow=0,ncol=nrank)
    left <- split.matrix(blocks,1,nrank)
    # First iteration of connecting
    while(length(left)>=nrank){
      tmp <- left[1:nrank]
      tmp_left <- unlist(tmp)
      left[1:nrank] <- NULL
      tmp_choose <- c()
      tmp_itf <- c()
      for(i in 1:nrank){
        tmp2 <- which(itf[tmp[[i]]]%in%tmp_itf)
        tmptmp <- choose.item(tmp,tmp2,i,T,itf,tmp_itf,warn)
        tmp3 <- tmptmp[[1]]
        tmp_itf <- c(tmp_itf,itf[tmp3])
        tmp_choose <- c(tmp_choose,tmp3)
      }
      tmp_block <- rbind(tmp_block,matrix(tmp_choose,ncol=nrank))
      left[[(length(left)+1)]] <- tmp_left
    }
    
    # Last block

    if(length(left)!=1){
      tests <- TRUE
      while(any(tests)){
        tmp_choose <- c()
        tmp_itf <- c()
        missing <- nrank-length(left)
        tmp <- left
        for(i in 1:length(tmp)){
          tmp2 <- which(itf[tmp[[i]]]%in%tmp_itf)
          tmptmp <- choose.item(tmp,tmp2,i,T,itf,tmp_itf,warn)
          tmp3 <- tmptmp[[1]]
          tmp_itf <- c(tmp_itf,itf[tmp3])
          tmp_choose <- c(tmp_choose,tmp3)
        }
        num_items <- rev(order(unlist(lapply(left,length))))
        for(i in 1:missing){
          j <- ifelse(i%%length(left)==0,length(left),i%%length(left))
          tmp2 <- which(itf[tmp[[j]]]%in%tmp_itf)
          tmptmp <- choose.item(tmp,tmp2,i,T,itf,tmp_itf,warn)
          tmp3 <- tmptmp[[1]]
          tmp_itf <- c(tmp_itf,itf[tmp3])
          tmp_choose <- c(tmp_choose,tmp3)
        }
        tests <- c(any(apply(apply(pair.combn(blocksort(rbind(blocks,tmp_block))),1,function(x) x%in%pair.combn(blocksort(matrix(tmp_choose,ncol=nrank)))[1,]),2,all)),
                   any(apply(apply(pair.combn(blocksort(rbind(blocks,tmp_block))),1,function(x) x%in%pair.combn(blocksort(matrix(tmp_choose,ncol=nrank)))[2,]),2,all)),
                   any(apply(apply(pair.combn(blocksort(rbind(blocks,tmp_block))),1,function(x) x%in%pair.combn(blocksort(matrix(tmp_choose,ncol=nrank)))[3,]),2,all)))
      }

      tmp_choose <- matrix(tmp_choose,ncol=nrank)
      tmp_block <- rbind(tmp_block,tmp_choose)
      left <- unlist(left)
    }
  }
  return(tmp_block)
}

