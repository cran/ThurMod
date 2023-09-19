### Roxygen-izable Documentation ----
#' Create model matrices for Thurstonian modeling
#' 
#' This function creates and returns model matrices of Thurstonian model equations.
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
#' 
### Outputs ---- 
#' @return Returns a list of elements containing model matrix information.
#' 
### Examples ----
#' @examples
#' 
#' # set seed and define blocks
#' set.seed(1)
#' blocks <- matrix(sample(1:15,15), ncol = 3)
#' 
#' # define the item-to-factor relation
#' itf <- rep(1:3,5)
#' 
#' mod.matrices(blocks,itf,'irt')
#'     
#' @export


### Function definition ----

mod.matrices <- function(blocks,itf,model){
  
  if(model=='simple2'){
    model <- 'simple'
    case <- 2
  }
  if(model=='simple3'){
    model <- 'simple'
    case <- 3
  }
  if(model=='simple5'){
    model <- 'simple'
    case <- 5
  }
  
  if(!is.matrix(blocks)&!is.vector(blocks)){
    stop('Blocks must be given as a matrix. Only for full designs a vector is possible.')
  }
  if(is.vector(blocks)){
    blocks <- blocks <- matrix(blocks,nrow=1)
  }
  design <- ifelse(dim(blocks)[1]==1,'full','block')
  
  # Find number of items per block
  nrank <- dim(blocks)[2]
  
  # Find number of factors, blocks and items
  nblock <- dim(blocks)[1]
  nitem <- length(unique(c(blocks)))
  nfactor <- max(itf)
  
  # Pair combinations
  pair_combn <- t(utils::combn(1:nitem,2))
  
  # Define items to factors for lambda (loadings)
  lambda_raw <- matrix(0,nrow=nitem,ncol=nfactor)
  for(i in 1:nfactor){
    lambda_raw[,i] <- ifelse(itf==i,i,0)
  }
  
  # Create names of pairs
  pair_names <- paste0('i',pair_combn[,1],'i',pair_combn[,2])
  
  # For each block: is it uni-dimensional?
  blocks_lambda <- matrix(itf[blocks],ncol=nrank)
  unidim <- apply(blocks_lambda,1,function(x) min(x)==max(x))
  
  # Create names of pairs for blocks
  pair_names_bs <- i.name(blocksort(blocks))
  pair_names_b <- i.name(blocks)
  
  tmp <- which(pair.combn(blocks)[,1]>pair.combn(blocks)[,2])
  
  if(length(tmp)!=0){
    tmp1 <- pair_names_b[tmp]
    tmp2 <- sub('^i.+i','i',tmp1)
    tmp3 <- tmp1
    for(j in 1:length(tmp)){
      tmp3 <- paste0(tmp2[j],sub(paste0(tmp2[j],'$'),'',tmp1[j]))
      pair_names_bs[which(pair_names_bs%in%tmp3)] <- pair_names_b[tmp[j]]
    }
  }
  pair_combn_b <- pair.combn(blocks)
  
  
  # MODEL
  if(model!='simple'){
    traits <- paste0('Trait',1:nfactor)
  }
  
  # Create Matrix A
  A <- designA(blocks)
  Arref <- rref(A)
  Arank <- dim(A)[1]-length(which(apply(abs(Arref),1,sum)==0))
  
  # Find global blocks
  gblocks <- metablock(blocks)
  unidim_gblocks <- sapply(gblocks,function(x) min(itf[x])==max(itf[x]))
  
  # Latent utility model and the unconstrained threshold model
  if(model%in%c('lmean','uc','simple')){
    # Part 1 (primary factors)
    t <- paste0('t',1:nitem)
    A_sign <- ifelse(A==0,'',ifelse(A==1,'+','-'))
    
    At <- matrix(NA, nrow=nrow(A_sign), ncol=nitem)
    At <- t(apply(A_sign,1,function(x)sub('\\+','',sub('^_0.*','',sub('^\\+0.*','',sub('^0.*','',paste0(x,t)))))))
    At <- sub('^-','',matrix(paste0(At,' BY ',pair_names_b,'@',ifelse(A_sign=='+','',A_sign),'1;\n'), ncol=nitem))
    At <- ifelse(A_sign=='','',At)
    At_text <- paste0(apply(At,2,function(x)paste0(x,collapse = '')), collapse = '')
    
    # Part 2 (secondary factors)
    if(model!='simple'){
      if(nfactor==1&Arank==(nitem-1)){
        input <- paste0(input, 'Trait1 BY\nt1@1\n', paste0(paste0('t',2:nitem,'*1'),collapse='\n'), ';\n\n')
      }else{
        A_fac <- rep('',nfactor)
        
        itf_gblocks <- lapply(gblocks,function(x) itf[x])
        items_fac <- list()
        items_fac_uni <- list()
        for(i in 1:nfactor){
          item_fac <- c()
          item_fac_uni <- c()
          for(j in 1:length(gblocks)){
            if(unidim_gblocks[j]){
              item_fac_uni <- c(item_fac_uni,gblocks[[j]][which(itf_gblocks[[j]]==i)[1]])
              item_fac <- c(item_fac,gblocks[[j]][which(itf_gblocks[[j]]==i)[-1]])
            }else{
              item_fac <- c(item_fac,gblocks[[j]][which(itf_gblocks[[j]]==i)])
            }
          }
          items_fac[[i]] <- sort(item_fac)
          if(all(is.na(item_fac_uni))){
            items_fac_uni[[i]] <- NA
          }else{
            items_fac_uni[[i]] <- sort(item_fac_uni)
          }
          
          A_fac[[i]] <- paste0(traits[i], ' BY\n')
          A_fac[[i]] <- paste0(paste0(traits[i], ' BY\n'),paste0('t',unname(items_fac[[i]]),'*1', '\n', collapse = ''))
          
          #for(j in 1:length(gblocks)){
          #if(unidim_gblocks[j]){
          #if(length(items_fac_uni[[i]])>0){
          if(all(!is.na(items_fac_uni[[i]]))){
            A_fac[[i]] <- paste0(A_fac[[i]],paste0('t',unname(items_fac_uni[[i]]),'@1', '\n', collapse = ''))
          }
          #}
          #}
          #}
        }
      }
    }
  }
  
  # IRT model
  if(model%in%c('irt')){
    # Create matrix lambda
    lambda <- ifelse(lambda_raw==0,0,paste0('L',row(lambda_raw)))
    A_sign <- ifelse(A==0,0,ifelse(A==1,'+','_'))
    A_lambda <- A%*%ifelse(lambda_raw==0,0,1)
    Lambda <- matrix(NA, nrow=nrow(A_sign), ncol=ncol(lambda))
    
    for(i in 1:nfactor){
      Lambda[,i] <- apply(A_sign,1,function(x) paste0(sub('^_0.*','',sub('^\\+0.*','',sub('^0.*','',paste0(x,lambda[,i])))), collapse =''))
    }
    tmp <- grep('^_L.+\\+',Lambda)
    tmp1 <- Lambda[tmp]
    Lambda[tmp] <- paste0(sub('^_L.+\\+','',tmp1),sub('\\+L.+','',tmp1))
    Lambda <- sub('\\+','',Lambda)
    Lambda <- sub('^_L','L_',Lambda)
    
    A_mplus <- NULL
    for(i in 1:nfactor){
      A_mplus <- paste0(A_mplus,'\n\n',traits[i],' BY \n',
                        paste0(ifelse(Lambda[,i]=='','',
                                      paste0(pair_names_b,'*',A_lambda[,i],' (',Lambda[,i],')\n')),collapse = ''),';')
    }
    
    if(nrank==2){
      
      Psi <- ifelse(row(diag(nitem))==col(diag(nitem)),paste0('1/2'),0)
      Psi <- ifelse(Psi==0,'',Psi) # neu
      A_sign <- ifelse(A==0,0,ifelse(A==1,'+','-')) # alt
      
      
      APsi <- matrix(NA, nrow=length(pair_names_b), ncol=nitem)
      for(i in 1:nitem){
        for(j in 1:length(pair_names_b)){
          tmp <- paste0(A_sign[j,],Psi[,i])
          tmp <- sub('^-$','',sub('^\\+$','',tmp))
          tmp <- sub('^-0.*','',sub('^\\+0.*','',sub('^0.*','',tmp)))
          APsi[j,i] <- paste0(tmp, collapse ='')
        }
      }
      APsi <- sub('^\\+','',APsi)
      
      APsiA <- matrix(NA, nrow=length(pair_names_b), ncol=length(pair_names_b))
      for(i in 1:length(pair_names_b)){
        for(j in 1:length(pair_names_b)){
          tmp <- paste0(A_sign[i,],APsi[j,])
          #tmp <- sub('\\+','',sub('^0.*','',tmp))
          tmp <- sub('^0.*','',tmp)
          APsiA[j,i] <- paste0(tmp, collapse ='')
        }
      }
      APsiA <- ifelse(APsiA=='+-','',ifelse(APsiA=='-+','',APsiA))
      APsiA <- gsub('-\\+','',APsiA)
      APsiA <- gsub('\\+-','-',APsiA)
      APsiA <- gsub('--','+',APsiA)
      
      APsiA <- sub('-$','',APsiA)#
      APsiA <- sub('\\+$','',APsiA)#
      APsiA <- sub('^\\+','',APsiA)
      
      APsiA <- sub('1/2+1/2','1',APsiA,fixed = TRUE)
      
      if(length(gblocks)!=nblock){
        # Define new parameters
        unidim_item <- Lambda[grep('^L.*_L',Lambda)]
        tmp <- unique(c(sub('_.*','',unidim_item),sub('^.*_','',unidim_item)))
        if(length(which(tmp%in%Lambda))!=0){
          tmp <- tmp[-which(tmp%in%Lambda)]
        }
        
        AddL <- c(paste0('L',1:nitem)[which(!paste0('L',1:nitem)%in%Lambda)])
        
        tmp <- c(AddL,tmp)
        Add_param <- unique(c(sort(tmp[which(nchar(tmp)==2)]),sort(tmp[which(nchar(tmp)==3)]),sort(tmp[which(nchar(tmp)==4)])))
        
        Add_unidim <- NULL
        for(i in 1:nrow(blocks)){
          tmp <- blocks[i,]%in%sub('L','',Add_param)
          if(all(tmp)){
            Add_unidim <- c(Add_unidim ,paste0('L',blocks[i,]))
          }
        }
        
        # Constraints
        conL <- rep('',nitem)
        for(i in 1:nitem){
          if(paste0('L',i)%in%Lambda|paste0('L',i)%in%Add_param){
            if(paste0('L_',i)%in%Lambda){
              conL[i] <- paste0('L_',i,' = ','- L',i)
            }
          }else if(i%in%sub('L','',Add_param[-which(Add_param%in%Add_unidim)])){
            conL[i] <- paste0('L_',i,' = ','- L',i)
          }
        }
        
        Add_param <- paste0(paste0(Add_param, collapse = '\n'),';')
        unidim_item <- ifelse(length(unidim_item)==0,'',paste0(unidim_item,' = ', sub('_','-',unidim_item),';\n',collapse = ''))
        conL <- paste0(ifelse(conL=='','',paste0(conL,';\n')), collapse = '')
      }else{
        Add_param <- NULL
      }
    }
    if(nrank>2){
      
      # Create Matrix Psi for irt models
      Psi <- ifelse(row(diag(nitem))==col(diag(nitem)),paste0('e',1:nitem),0)
      Psi <- ifelse(Psi==0,'',Psi) # neu
      A_sign <- ifelse(A==0,0,ifelse(A==1,'+','-')) # alt
      
      APsi <- matrix(NA, nrow=length(pair_names_b), ncol=nitem)
      for(i in 1:nitem){
        for(j in 1:length(pair_names_b)){
          tmp <- paste0(A_sign[j,],Psi[,i])
          tmp <- sub('^-$','',sub('^\\+$','',tmp))
          tmp <- sub('^-0.*','',sub('^\\+0.*','',sub('^0.*','',tmp)))
          APsi[j,i] <- paste0(tmp, collapse ='')
        }
      }
      APsi <- sub('^\\+','',APsi)
      
      APsiA <- matrix(NA, nrow=length(pair_names_b), ncol=length(pair_names_b))
      for(i in 1:length(pair_names_b)){
        for(j in 1:length(pair_names_b)){
          tmp <- paste0(A_sign[i,],APsi[j,])
          tmp <- sub('^0.*','',tmp)
          APsiA[j,i] <- paste0(tmp, collapse ='')
        }
      }
      APsiA <- ifelse(APsiA=='+-','',ifelse(APsiA=='-+','',APsiA))
      APsiA <- gsub('-\\+','',APsiA)
      APsiA <- gsub('\\+-','-',APsiA)
      APsiA <- gsub('--','+',APsiA)
      
      APsiA <- sub('-$','',APsiA)#
      APsiA <- sub('\\+$','',APsiA)#
      APsiA <- sub('^\\+','',APsiA)
      
      APsiA_under  <- sub('-e','e_',APsiA)
      APsiA_sign <- ifelse(APsiA=='','',ifelse(substr(APsiA,1,1)=='-','-',''))
      
      
      # Define new parameters
      unidim_item <- Lambda[grep('^L.*_L',Lambda)]
      tmp <- unique(c(sub('_.*','',unidim_item),sub('^.*_','',unidim_item)))
      if(length(which(tmp%in%Lambda))!=0){
        tmp <- tmp[-which(tmp%in%Lambda)]
      }
      
      AddL <- c(paste0('L',1:nitem)[which(!paste0('L',1:nitem)%in%Lambda)])
      
      tmp <- c(AddL,tmp)
      Add_param <- unique(c(sort(tmp[which(nchar(tmp)==2)]),sort(tmp[which(nchar(tmp)==3)]),sort(tmp[which(nchar(tmp)==4)])))
      
      Add_unidim <- NULL
      for(i in 1:nrow(blocks)){
        tmp <- blocks[i,]%in%sub('L','',Add_param)
        if(all(tmp)){
          Add_unidim <- c(Add_unidim ,paste0('L',blocks[i,]))
        }
      }
      
      # Constraints
      conL <- rep('',nitem)
      for(i in 1:nitem){
        if(paste0('L',i)%in%Lambda|paste0('L',i)%in%Add_param){
          if(paste0('L_',i)%in%Lambda){
            conL[i] <- paste0('L_',i,' = ','- L',i)
          }
        }else if(i%in%sub('L','',Add_param[-which(Add_param%in%Add_unidim)])){
          conL[i] <- paste0('L_',i,' = ','- L',i)
        }
      }
      
      Add_param <- paste0(paste0(Add_param, collapse = '\n'),';')
      unidim_item <- ifelse(length(unidim_item)==0,'',paste0(unidim_item,' = ', sub('_','-',unidim_item),';\n',collapse = ''))
      conL <- paste0(ifelse(conL=='','',paste0(conL,';\n')), collapse = '')
      
      
      # Pair's uniqueness is equal to sum of two utility uniquenesses
      if(design=='full'){
      }else if(design=='block'){
        uniquenesses <- c()
        for(i in 1:nrow(blocks)){
          items <- blocks[i,]
          whitems <- which(pair_combn_b[,1]%in%items&pair_combn_b[,2]%in%items)
          tmp <- pair_combn_b[whitems,1][which(pair_combn_b[whitems,1]%in%pair_combn_b[whitems,2])]
          
          tmp <- paste0('e',pair_combn_b[whitems,1],'e',pair_combn_b[whitems,2],' = ' ,ifelse(pair_combn_b[whitems,1]%in%tmp,'-e_','e'),pair_combn_b[whitems,1],
                        ifelse(pair_combn_b[whitems,2]%in%tmp,' - e_',' + e'),pair_combn_b[whitems,2],';')
          tmp0 <- paste0('e',pair_combn_b[whitems,1],'e',pair_combn_b[whitems,2])
          for(i in length(tmp0):1){
            if(any(grepl(tmp0[i],uniquenesses))){
              tmp <- tmp[-i]
            }
          }
          uniquenesses <- c(uniquenesses,tmp)
        }
        uniquenesses <- unique(uniquenesses[order(nchar(uniquenesses),uniquenesses)])
      }
      
    }
  }
  
  if(model=='irt'){
    return(list(lambda=Lambda, 
                resvar=paste0(pair_names_b,'*2 ','(e',pair_combn_b[,1],'e',pair_combn_b[,2],');', collapse = '\n'),
                Psi=Psi,
                APsi=APsi,
                APsiA=APsiA,
                npair=length(pair_names_b),
                naddparam=length(unlist(strsplit(gsub(';','',Add_param),'\n'))),
                addparam=strsplit(gsub(';','',Add_param),'\n'),
                pair_names_b=sub('i','I',pair_names_b)))
  }else if(model=='lmean'|model=='uc'){
    lambda_read <- strsplit(A_fac,'\n')
    #if(connected|xtra){
    #  for(i in 1:nfactor){
    #    lambda_read[[i]] <- lambda_read[[i]][1:(length(which(itf==i))+1)]
    #  }
    #}
    return(list(lambda=lambda_read,
                npair=length(pair_names_b),
                pair_names_b=sub('i','I',pair_names_b)))
  }else{
    #lambda_read <- strsplit(A_fac,'\n')
    #if(connected|xtra){
    #  for(i in 1:nfactor){
    #    lambda_read[[i]] <- lambda_read[[i]][1:(length(which(itf==i))+1)]
    #  }
    #}
    return(list(npair=length(pair_names_b),
                pair_names_b=sub('i','I',pair_names_b)))
  }
}







