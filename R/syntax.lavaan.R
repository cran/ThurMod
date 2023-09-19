### Roxygen-izable Documentation ----
#' Create lavaan syntax for Thurstonian forced choice analysis
#' 
#' This function writes a lavaan syntax given the specifications of the
#' Thurstonian forced choice model. 
#' 
### Inputs ----
#' @param blocks A matrix defining the blocks of the model. The number of rows
#' must be the number of blocks, each row represents a block and contains the
#' item numbers. The number of columns present the number of items per block.
#' @param itf A vector defining the items-to-factor relation. For example
#' `c(1,1,1,2,2,2)` defines six items, the first three correspond to factor 1,
#' the second three correspond to factor 2. 
#' @param model A descriptor for the model. Can be one of `'lmean'`,
#' `'uc'`, `'irt'` or  `'simple2'`, `'simple3'` or `'simple5'`. The Number behind 
#' the `'simple'` statement defines the Thurstone case.
#' @param rename_list A list with two vectors to rename the objects in the syntax.
#' Vector one is the original names, vector two the new names. Defaults to `NULL`.
#' 
### Outputs ---- 
#' @return Returns a description of the user-specified model. Typically, the
#' model is described using the lavaan model syntax. See `lavaan::model.syntax`
#' for more information. 
#' 
#' @details The syntax currently is able to perform model analysis for the
#' latent utility models (`'simple'` and `'lmean'`; Maydeu-Olivares & Böckenholt, 2005) the 
#' unconstrained factor model (`'uc'`; Maydeu-Olivares & Böckenholt, 2005) and 
#' the IRT model(`'irt'`; Maydeu-Olivares & Brown, 2010). Additionally, all
#' model types can be performed with all types of forced choice designs (full,
#' block, partially linked block, linked block). For an overview and review see
#' Jansen and Schulze (2023a,2023b).
#' 
#' The standard naming procedure ixiy, for the comparison of items x and y, 
#' can be changed by specifying the `rename_list` argument. The first vector of
#' the schould be the vector of original names, for example 
#' `c('i1i2','i1i3','i2i3','Trait1','Trait2','Trait3')`
#' the second vector should contain the new names, for example 
#' `c('A01E12','A01C13','E01C23','Agree','Extra','Consc')`.
#' 
### Examples ----
#' @examples
#' 
#' # read data set FC
#' data(FC)
#' 
#' # set seed and define blocks
#' set.seed(1)
#' blocks <- matrix(sample(1:15,15), ncol = 3)
#' 
#' # define the item-to-factor relation
#' itf <- rep(1:3,5)
#' 
#' # Create lavaan model syntax
#' syntax.lavaan(blocks,itf,'irt')
#' 
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
#' @export




syntax.lavaan <- function(blocks,itf,model,rename_list=NULL){
  
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
  pair_names_b <- i.name(blocks)
  pair_combn_b <- pair.combn(blocks)
  
  # Begin syntax

  
  # write lavaan model
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
    input <- paste0('# Model of primary factors:\n')
    t <- paste0('t',1:nitem)
    A_sign <- ifelse(A==0,'',ifelse(A==1,'+','-'))
    
    At <- matrix(NA, nrow=nrow(A_sign), ncol=nitem)
    At <- t(apply(A_sign,1,function(x)sub('\\+','',sub('^_0.*','',sub('^\\+0.*','',sub('^0.*','',paste0(x,t)))))))
    At <- sub('^-','',matrix(paste0(At,' =~ ',ifelse(A_sign=='+','',A_sign),'1*',pair_names_b,'\n'), ncol=nitem))
    At <- ifelse(A_sign=='','',At)
    At_text <- paste0(apply(At,2,function(x)paste0(x,collapse = '')), collapse = '')
    
    input <- paste0(input, paste(At_text, collapse='\n'), '\n\n')
    
    # For rankings: Fix variances of indicators to 0
    input <- paste0(input, '# Fix the variances of all indicators to 0:\n')
    input <- paste0(input, paste0(pair_names_b,' ~~ 0*',pair_names_b, collapse = '\n'))
    #### Whats with paired comparisons?
  
    # Part 2 (secondary factors)
    if(model!='simple'){
      input <- paste0(input, '\n\n # Model of secondary factors:\n')
      if(nfactor==1&Arank==(nitem-1)){
        input <- paste0(input, 'Trait1 =~ 1 * t1 +', paste0(paste0('start(1) * t',2:nitem),collapse=' + \n'), '\n\n')
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
          
          A_fac[[i]] <- paste0(traits[i], ' =~ ')
          A_fac[[i]] <- paste0(paste0(traits[i], ' =~ '),paste0('start(1) * t',unname(items_fac[[i]]), collapse = ' + '))
          
          #for(j in 1:length(gblocks)){
          #if(unidim_gblocks[j]){
          #if(length(items_fac_uni[[i]])>0){
          #A_fac[[i]] <- paste0(A_fac[[i]],paste0('1 * t',unname(items_fac_uni[[i]]), '\n', collapse = '+'))
          if(all(!is.na(items_fac_uni[[i]]))){
            A_fac[[i]] <- paste0(A_fac[[i]],' + ',paste0('1 * t',unname(items_fac_uni[[i]]), '\n', collapse = ' + \n'))
          }
          #}
          #}
          #}
        }
        input <- paste0(input, paste0(A_fac,collapse = '\n'),'\n\n')
      }
      
      # Fix variances of factors to 1
      input <- paste0(input, '# Fix variances of all factors to 1:\n')
      input <- paste0(input, paste0(traits,' ~~ 1 * ',traits, collapse = '\n'), '\n')
      
      input <- paste0(input, '# Define the correlations between factors:\n')
      if(nfactor>1){
        inp_tmp <- paste0('Trait',t(utils::combn(1:nfactor,2))[,1], ' ~~ ', 'Trait', t(utils::combn(1:nfactor,2))[,2], collapse = '\n')
        input <- paste0(input, inp_tmp, '\n\n')
      }
      # Fix uniqueness of one item "per block"
      # This is not necessary but maybe more helpful for application. Fix the lowest item per block
      #####
      if(nrank==2){
        input <- paste0(input, '# Fix uniquenesses of all items to 1:\n')
        input <- paste0(input, paste0('t',1:nitem, '~~ 1*t',1:nitem,collapse = '\n'), '\n')
      }else{
        input <- paste0(input, '#Fix uniquenesses of one items per grand block:\n')
        for(i in 1:length(gblocks)){
          tmp <- min(gblocks[[i]])
          input <- paste0(input,'t',tmp,'~~ 1*t',tmp,'\n')
        }
      }
    }

    # Fixing for the latent utility model
    if(model=='lmean'|model=='simple'){
      input <- paste0(input, '#Fix all thresholds to 0 to estimate means:\n')
      input <- paste0(input, paste0(pair_names_b,'|0*t1', collapse='\n'),'\n\n')
      if(model!='simple'){
        input <- paste0(input, '#Fix all means of traits to 0:\n')
        input <- paste0(input, paste0(traits,'~0', collapse='\n'),'\n\n')
      }

      if(design=='full'){
        input <- paste0(input, '# Fix one mean to 0 and free all other means via int.lv.free = TRUE:\n')
        input <- paste0(input, 't1~0','\n\n')
      }
      if(design=='block'){
        input <- paste0(input, '# Fix one mean per grand block to 0 and free all other means via int.lv.free = TRUE:\n')

        tmptmp <- c()
        for(i in 1:length(gblocks)){
          tmp <- min(gblocks[[i]])
          tmptmp <- c(tmptmp,tmp)
        }
        
        input <- paste0(input, paste0('t',tmptmp,' ~ 0', collapse='\n'))

      }
    }
    
    #input <- ''
    # Additional Fixing for the Thurstone cases
    if(model=='simple'){
      if(case==5){
        input <- paste0(input,'\n','# fix factor variances to 1\n',paste0('t',1:nitem,' ~~ 1*','t',1:nitem, collapse = '\n'),'\n\n')
        input <- paste0(input, '# fix all covariances between factors to zero:\n')
        
        inp_tmp <- paste0('t',t(utils::combn(1:nitem,2))[,1], ' ~~ 0*', 't', t(utils::combn(1:nitem,2))[,2], collapse = '\n')
        input <- paste0(input, inp_tmp, '\n\n')
      }
      if(case==3){
        input <- paste0(input,'\n','# free factor variances except the last one\n',paste0('t',nitem),' ~~ 1*\n',paste0('t',nitem),'\n\n')
        
        input <- paste0(input, '# fix all covariances between factors to zero:\n')
        
        inp_tmp <- paste0('t',t(utils::combn(1:nitem,2))[,1], ' ~~ 0*', 't', t(utils::combn(1:nitem,2))[,2], collapse = '\n')
        input <- paste0(input, inp_tmp, '\n\n')
      }
      if(case==2){
        input <- paste0(input,'\n','# free factor variances except the first and last one\n', paste0('t',c(1,nitem),' ~~ 1*','t',c(1,nitem),collapse = '\n'),'\n\n')
        
        input <- paste0(input, '# all Factor covariances are free except these involving the last factor:\n')
        tmp <- t(utils::combn(1:nitem,2))
        tmp1 <- tmp[which(tmp[,2]!=nitem),]
        inp_tmp1 <- paste0('t',tmp1[,1], ' ~~ ', 't', tmp1[,2], collapse = '\n')
        
        tmp2 <- tmp[which(tmp[,2]==nitem),]
        inp_tmp2 <- paste0('t',tmp2[,1], ' ~~ 0*', 't', tmp2[,2], collapse = '\n')
        input <- paste0(input, inp_tmp1, '\n\n', inp_tmp2, '\n\n')
      }
    }
    
    
    #if(standardized){
    #  input <- paste0(input, '\n\nOUTPUT: STDYX;\n\n')
    #}
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
      tmp <- ifelse(Lambda[,i]=='','',
                    paste0('start(',A_lambda[,i],') * ',Lambda[,i],' * ',pair_names_b))
      tmp <- tmp[which(tmp!='')]
      A_mplus <- paste0(A_mplus,'\n',traits[i],' =~ ',
                        paste0(tmp,collapse = ' + '),'')
    }
    
    # Fix variances of factors to 1
    input <- paste0(A_mplus,'\n\n', '# Fix variances of all factors to 1:\n')
    input <- paste0(input, paste0(traits,' ~~ 1 * ',traits, collapse = '\n'), '\n')
    
    
    # Correlations between factors
    input <- paste0(input, '# Define the correlations between factors:\n')
    if(nfactor>1){
      inp_tmp <- paste0('Trait',t(utils::combn(1:nfactor,2))[,1], ' ~~ ', 'Trait', t(utils::combn(1:nfactor,2))[,2], collapse = '\n')
      input <- paste0(input, inp_tmp, '\n\n')
    }
    
    if(nrank==2){
      input <- paste0(input, '# Fix uniquenesses of all indicators to 1:\n')
      input <- paste0(input, paste0(pair_names_b,'~~1*',pair_names_b,collapse = '\n'), '\n')
      if(length(gblocks)!=nblock){
        # Model constraints
        input <- paste0(input, '# Set model constraints for loadings:\n')
        #input <- paste0(input, '\n','MODEL CONSTRAINT:\nNEW\n')
        
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
              conL[i] <- paste0('L_',i,' == ','- L',i)
            }
          }else if(i%in%sub('L','',Add_param[-which(Add_param%in%Add_unidim)])){
            conL[i] <- paste0('L_',i,' == ','- L',i)
          }
        }
        #pair_names_b_mat <- ifelse(Lambda=="","",pair_names_b)
        
        
        #Add_param_tmp_pos <- paste0(Add_param,' := -',gsub('L','L_',Add_param), collapse = '\n')
        #Add_param_tmp_neg <- paste0(gsub('L','L_',Add_param),' := -',Add_param, collapse = '\n')
        unidim_item_lavx <- ifelse(length(unidim_item)==0,'',paste0(unidim_item,' == ', sub('_','-',unidim_item),'\n',collapse = ''))
        conL <- paste0(ifelse(conL=='','',paste0(conL,'\n')), collapse = '')
        
        
        tmp <- sub('_.*$','',unidim_item)
        tmp1 <- sub('^.*_','',unidim_item)
        unidim_item_tmp <- unidim_item
        
        y <- c(paste0('L_',1:nitem)[which(paste0('L_',1:nitem)%in%Lambda)])
        y <- sub('_','',y)
        y <- y[which(y%in%AddL)]
        
        
        mins <- c()
        for(i in 1:length(gblocks)){
          if(unidim_gblocks[i]){
            mins <- c(mins, min(gblocks[[i]]))
          }
        }
        
        known <- c(paste0('L',1:nitem))
        known <- known[which(!known%in%AddL)]
        known <- unique(c(known,y))
        if(length(mins)!=0){
          known <- unique(c(known,paste0('L',mins)))
        }
        
        conL_neg <- ''
        if(length(y)!=0){
          conL_neg <- paste0(y,' := -',gsub('L','L_',y), collapse = '\n')
        }
        
        
        
        unidim_item_lav <- ''
        lknown <- length(known)
        if(lknown!=nitem){
          
          x <- which(tmp%in%AddL|tmp1%in%AddL)
          
          
          tmp <- tmp[x]
          tmp1 <- tmp1[x]
          unidim_item_tmp <- unidim_item_tmp[x]
          
          
          tmp2 <- which(!Add_param%in%known) #which(!Add_param%in%c(tmp1,paste0('L',mins)))
          
          # change order for fixing parameters where needed
          tmp1s <- c()
          tmps <- c()
          if(length(tmp2)>0){
            for(i in 1:length(tmp2)){
              w <- which(tmp==Add_param[tmp2[i]]) # which(tmp==paste0('L',tmp2[i]))
              if(length(w)!=0){
                x <- tmp1[w]
                b <- table(tmp1)[names(table(tmp1))%in%x]
                z <- names(which(b==max(b)))[1]
                
                a <- w[which(x==z)]
                
                u <- tmp[a]
                v <- tmp1[a]
                tmp1[a] <- u
                tmp[a] <- v
                tmp1s <- c(tmp1s,z)
                tmps <- c(tmps,u)
              }
            }
          }
          
          while(!all(tmp%in%known)){
            x <- which(tmp%in%known)
            known <- unique(c(known,tmp1[x]))
            tmp <- c(tmp[x],tmp[-x])
            tmp1 <- c(tmp1[x],tmp1[-x])
            unidim_item_tmp <- c(unidim_item_tmp[x],unidim_item_tmp[-x])
          }
          
          i <- 2
          while(!is.na(tmp1[i])){
            x <- tmp1[i]
            if(x%in%tmp1[1:(i-1)]){
              tmp <- tmp[-i]
              tmp1 <- tmp1[-i]
              unidim_item_tmp <- unidim_item_tmp[-i]
            }else{
              i <- i+1
            }
          }
          b <- which(tmp1%in%tmps&tmp%in%tmp1s)
          if(length(b)==0){
            b <- 0
          }
        }
        
        if(length(tmp1)!=0&lknown!=nitem){
          
          x <- AddL[which(!AddL%in%y)]
          x <- which(tmp1%in%x)
          
          tmp <- tmp[x]
          tmp1 <- tmp1[x]
          unidim_item_tmp <- unidim_item_tmp[x]
          
          unidim_sign <- rep('-',length(tmp1))
          unidim_sign[b] <- '+'
          
          unidim_item_lav <- paste0(tmp1,' := ', tmp, unidim_sign,unidim_item_tmp,'\n',collapse = '')
        }
        
        mins <- ifelse(length(mins)==0,'',paste0('\n L',mins,':=1', collapse = '\n'))
        
        #Add_param_pre <- paste0(paste0(Add_param,' =~ 0', collapse = '\n'),'\n')
        #Add_param_post <- paste0(Add_param," ~ NA*1 + label('",Add_param,"')*1 +0?1", collapse = '\n')
        #unidim_item <- ifelse(length(unidim_item)==0,'',paste0(unidim_item,' == ', sub('_','-',unidim_item),'\n',collapse = ''))
        #conL <- paste0(ifelse(conL=='','',paste0(conL,'\n')), collapse = '')
        
        #input <- paste0(input,Add_param,'\n\n',unidim_item,'\n',conL,'\n\n')
        
        #input <- paste0(input,Add_param_pre,'\n\n',Add_param_post,'\n\n',unidim_item,'\n',conL,'\n\n')
        
        #if(unidim_item_lav==''){
        #  input <- paste0(input,'\n\n',mins,'\n\n',conL_neg,'\n\n',unidim_item_lavx,'\n\n',
        #                  conL,'\n\n')
        #}else{
        input <- paste0(input,'\n\n',mins,'\n\n',conL_neg,'\n\n',unidim_item_lav,'\n',unidim_item_lavx,'\n\n',
                        '\n',conL,'\n\n')
        #}
        
        
        
        #Add_param <- paste0(Add_param,' == -',gsub('L','L_',Add_param), collapse = '\n')
        #unidim_item <- ifelse(length(unidim_item)==0,'',paste0(unidim_item,' == ', sub('_','-',unidim_item),'\n',collapse = ''))
        #conL <- paste0(ifelse(conL=='','',paste0(conL,'\n')), collapse = '')
        
        
        
        #input <- paste0(input,unidim_item,'\n',Add_param,'\n',conL,'\n\n')
        
        
        # Fix one loading in each unidim block if this block is not connected with blocks of another factor or of already fixed blocks
        # This is not necessary but maybe more helpful for application. Fix the lowest item per block
        input <- paste0(input, '# Fix one loading per uni-dimensional grand block:\n')
        for(i in 1:length(gblocks)){
          if(unidim_gblocks[i]){
            input <- paste0(input, paste0('L',min(gblocks[[i]]),'==1\n', '\n'))
          }
        }
        
        input <- paste0(input,'\n')
      }
    }
    if(nrank>2){
      # Declare uniqunesses
      input <- paste0(input, '# Declare uniquenesses:\n')
      input <- paste0(input,paste0(pair_names_b,' ~~ ','e',pair_combn_b[,1],'e',pair_combn_b[,2],' * ',pair_names_b, collapse = '\n'))
      
      # Create Matrix Psi for irt models
      input <- paste0(input, '\n\n# Define structured uniquenesses:\n')
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
      
      APsiA_under  <- sub('-e','e_',APsiA)
      APsiA_sign <- ifelse(APsiA=='','',ifelse(substr(APsiA,1,1)=='-','-',''))
      
      
      # Correlations in Psi
      APsiA_text <- matrix('', nrow=length(pair_names_b), ncol=length(pair_names_b))
      for(i in 1:(nrow(APsiA)-1)){
        for(j in (i+1):nrow(APsiA)){
          APsiA_text[j,i] <- paste0(pair_names_b[i],' ~~ start(',APsiA_sign[j,i],'1) * ',APsiA_under[j,i],' * ', pair_names_b[j])
        }
      }
      
      APsiA_text <- ifelse(APsiA=='','',APsiA_text)
      APsiA_text <- paste0(ifelse(APsiA_text=='','',paste0(APsiA_text,'\n')), collapse = '')
      
      input <- paste0(input,'\n\n',APsiA_text,'\n\n')
      
      # Model constraints
      input <- paste0(input, '# Set model constraints for loadings and uniquenesses:\n')
      
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
            conL[i] <- paste0('L_',i,' == ','- L',i)
          }
        }else if(i%in%sub('L','',Add_param[-which(Add_param%in%Add_unidim)])){
          conL[i] <- paste0('L_',i,' == ','- L',i)
        }
      }
      
      #pair_names_b_mat <- ifelse(Lambda=="","",pair_names_b)
      
      
      #Add_param_tmp_pos <- paste0(Add_param,' := -',gsub('L','L_',Add_param), collapse = '\n')
      #Add_param_tmp_neg <- paste0(gsub('L','L_',Add_param),' := -',Add_param, collapse = '\n')
      unidim_item_lavx <- ifelse(length(unidim_item)==0,'',paste0(unidim_item,' == ', sub('_','-',unidim_item),'\n',collapse = ''))
      conL <- paste0(ifelse(conL=='','',paste0(conL,'\n')), collapse = '')

      
      tmp <- sub('_.*$','',unidim_item)
      tmp1 <- sub('^.*_','',unidim_item)
      unidim_item_tmp <- unidim_item
      
      y <- c(paste0('L_',1:nitem)[which(paste0('L_',1:nitem)%in%Lambda)])
      y <- sub('_','',y)
      y <- y[which(y%in%AddL)]
      
      
      mins <- c()
      for(i in 1:length(gblocks)){
        if(unidim_gblocks[i]){
          mins <- c(mins, min(gblocks[[i]]))
        }
      }
      
      known <- c(paste0('L',1:nitem))
      known <- known[which(!known%in%AddL)]
      known <- unique(c(known,y))
      if(length(mins)!=0){
        known <- unique(c(known,paste0('L',mins)))
      }
      
      conL_neg <- ''
      if(length(y)!=0){
        conL_neg <- paste0(y,' := -',gsub('L','L_',y), collapse = '\n')
      }
      
      
      
      unidim_item_lav <- ''
      lknown <- length(known)
      if(lknown!=nitem){
        
        x <- which(tmp%in%AddL|tmp1%in%AddL)
        
        
        tmp <- tmp[x]
        tmp1 <- tmp1[x]
        unidim_item_tmp <- unidim_item_tmp[x]
        
        
        tmp2 <- which(!Add_param%in%known) #which(!Add_param%in%c(tmp1,paste0('L',mins)))
        
        # change order for fixing parameters where needed
        tmp1s <- c()
        tmps <- c()
        if(length(tmp2)>0){
          for(i in 1:length(tmp2)){
            w <- which(tmp==Add_param[tmp2[i]]) # which(tmp==paste0('L',tmp2[i]))
            if(length(w)!=0){
              x <- tmp1[w]
              b <- table(tmp1)[names(table(tmp1))%in%x]
              z <- names(which(b==max(b)))[1]
              
              a <- w[which(x==z)]
              
              u <- tmp[a]
              v <- tmp1[a]
              tmp1[a] <- u
              tmp[a] <- v
              tmp1s <- c(tmp1s,z)
              tmps <- c(tmps,u)
            }
          }
        }

        while(!all(tmp%in%known)){
          x <- which(tmp%in%known)
          known <- unique(c(known,tmp1[x]))
          tmp <- c(tmp[x],tmp[-x])
          tmp1 <- c(tmp1[x],tmp1[-x])
          unidim_item_tmp <- c(unidim_item_tmp[x],unidim_item_tmp[-x])
        }
        
        i <- 2
        while(!is.na(tmp1[i])){
          x <- tmp1[i]
          if(x%in%tmp1[1:(i-1)]){
            tmp <- tmp[-i]
            tmp1 <- tmp1[-i]
            unidim_item_tmp <- unidim_item_tmp[-i]
          }else{
            i <- i+1
          }
        }
        b <- which(tmp1%in%tmps&tmp%in%tmp1s)
        if(length(b)==0){
          b <- 0
        }
      }
      
      if(length(tmp1)!=0&lknown!=nitem){
        
        x <- AddL[which(!AddL%in%y)]
        x <- which(tmp1%in%x)
        
        tmp <- tmp[x]
        tmp1 <- tmp1[x]
        unidim_item_tmp <- unidim_item_tmp[x]
        
        unidim_sign <- rep('-',length(tmp1))
        unidim_sign[b] <- '+'
        
        unidim_item_lav <- paste0(tmp1,' := ', tmp, unidim_sign,unidim_item_tmp,'\n',collapse = '')
      }
      
      mins <- ifelse(length(mins)==0,'',paste0('\n L',mins,':=1', collapse = '\n'))
      
      #Add_param_pre <- paste0(paste0(Add_param,' =~ 0', collapse = '\n'),'\n')
      #Add_param_post <- paste0(Add_param," ~ NA*1 + label('",Add_param,"')*1 +0?1", collapse = '\n')
      #unidim_item <- ifelse(length(unidim_item)==0,'',paste0(unidim_item,' == ', sub('_','-',unidim_item),'\n',collapse = ''))
      #conL <- paste0(ifelse(conL=='','',paste0(conL,'\n')), collapse = '')
      
      #input <- paste0(input,Add_param,'\n\n',unidim_item,'\n',conL,'\n\n')
      
      #input <- paste0(input,Add_param_pre,'\n\n',Add_param_post,'\n\n',unidim_item,'\n',conL,'\n\n')
      
      #if(unidim_item_lav==''){
      #  input <- paste0(input,'\n\n',mins,'\n\n',conL_neg,'\n\n',unidim_item_lavx,'\n\n',
      #                  conL,'\n\n')
      #}else{
        input <- paste0(input,'\n\n',mins,'\n\n',conL_neg,'\n\n',unidim_item_lav,'\n',unidim_item_lavx,'\n\n',
                        '\n',conL,'\n\n')
      #}
      

      
      #Add_param <- paste0(Add_param,' == -',gsub('L','L_',Add_param), collapse = '\n')
      #unidim_item <- ifelse(length(unidim_item)==0,'',paste0(unidim_item,' == ', sub('_','-',unidim_item),'\n',collapse = ''))
      #conL <- paste0(ifelse(conL=='','',paste0(conL,'\n')), collapse = '')
      

      
      #input <- paste0(input,unidim_item,'\n',Add_param,'\n',conL,'\n\n')
      
      
      
      
      
      
      
      
      
      
      # Fix one loading in each unidim block if this block is not connected with blocks of another factor or of already fixed blocks
      # This is not necessary but maybe more helpful for application. Fix the lowest item per block
      input <- paste0(input, '# Fix one loading per uni-dimensional grand block:\n')
      for(i in 1:length(gblocks)){
        if(unidim_gblocks[i]){
          input <- paste0(input, paste0('L',min(gblocks[[i]]),'==1\n', '\n'))
        }
      }
      
      input <- paste0(input,'\n')
      
      # Uniquenesses relating to the same item are equal
      input <- paste0(input, '# Set uniquenesses relation to the same item as equal:\n')
      tmp <- rep(NA,nitem)
      for(i in 1:nitem){
        tmp[i] <- paste0('e',i)%in%APsiA[lower.tri(APsiA)]&paste0('-e',i)%in%APsiA[lower.tri(APsiA)]
        if(tmp[i]){
          input <- paste0(input,paste0('e_',i),' == ',paste0('- e',i),'\n')
        }
      }
      
      input <- paste0(input,'\n')
      
      # Pair's uniqueness is equal to sum of two utility uniquenesses
      input <- paste0(input, '# A pairs uniqueness is equal to the sum of the item uniquenesses:\n')
      if(design=='full'){
        input <- paste0(input,paste0('e',pair_combn[,1],'e',pair_combn[,2],' == e',pair_combn[,1],' + e',pair_combn[,2], collapse = '\n'),'\n\n')
      }else if(design=='block'){
        uniquenesses <- c()
        for(i in 1:nrow(blocks)){
          items <- blocks[i,]
          whitems <- which(pair_combn_b[,1]%in%items&pair_combn_b[,2]%in%items)
          tmp <- pair_combn_b[whitems,1][which(pair_combn_b[whitems,1]%in%pair_combn_b[whitems,2])]
          
          tmp <- paste0('e',pair_combn_b[whitems,1],'e',pair_combn_b[whitems,2],' == ' ,ifelse(pair_combn_b[whitems,1]%in%tmp,'-e_','e'),pair_combn_b[whitems,1],
                        ifelse(pair_combn_b[whitems,2]%in%tmp,' - e_',' + e'),pair_combn_b[whitems,2])
          tmp0 <- paste0('e',pair_combn_b[whitems,1],'e',pair_combn_b[whitems,2])
          for(i in length(tmp0):1){
            if(any(grepl(tmp0[i],uniquenesses))){
              tmp <- tmp[-i]
            }
          }
          uniquenesses <- c(uniquenesses,tmp)
          ##sapply(paste0('e',pair_combn_b[whitems,1],'e',pair_combn_b[whitems,2]),function(x) grepl(x,uniquenesses))
        }
        uniquenesses <- unique(uniquenesses[order(nchar(uniquenesses),uniquenesses)])
        input <- paste0(input,paste0(uniquenesses, collapse = '\n'),'\n', collapse = '\n')
      }

      # This is not necessary but maybe more helpful for application. Fix the lowest item per block
      for(i in 1:length(gblocks)){
        tmp2 <- min(gblocks[[i]])
        tmp3 <- max(gblocks[[i]])
        if(paste0('e',tmp2)%in%APsiA_under[lower.tri(APsiA)]){
          input <- paste0(input,'\n','e',tmp2,' == 1')
        }else{
          input <- paste0(input,'\n','e_',tmp2,' == -1')
        }
        
      }
    }
    
  }
  if(!is.null(rename_list)){
    tmp <- ceiling(log10(length(rename_list[[1]])))
    elem <- c()
    for(i in 1:tmp){
      tmp0 <- paste0('el3m3nt',paste0(rep('0',tmp-i),collapse = ''),(10^(i-1)):((10^i)-1))
      elem <- c(elem,tmp0)
    }
    elem <- elem[1:length(rename_list[[1]])]
    
    for(i in 1:length(rename_list[[1]])){
      input <- gsub(rename_list[[1]][i],elem[i],input)
    }
    for(i in 1:length(rename_list[[1]])){
      input <- gsub(elem[i],rename_list[[2]][i],input)
    }
  }
  
  return(input)
}


