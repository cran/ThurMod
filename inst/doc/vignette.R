params <-
list(EVAL = FALSE)

## ---- SETTINGS-knitr, include=FALSE-----------------------------------------------------
stopifnot(require(knitr))
options(width = 90)
opts_chunk$set(
  comment = NA,
  message = FALSE,
  warning = FALSE
)

## ---- echo=FALSE------------------------------------------------------------------------
library(ThurMod)
designA(4)

## ---- echo=FALSE------------------------------------------------------------------------
load("4v.RData")

## ---------------------------------------------------------------------------------------
library(ThurMod)

## ---------------------------------------------------------------------------------------
nfactor <- 4
nitem <- 12
nperson <- 1000
itf <- rep(1:4, 3)
varcov <- diag(1, 4)

# latent utility means
set.seed(69)
lmu <- runif(nitem, -1, 1)
loadings <- runif(nitem, 0.30, 0.95)

## ---------------------------------------------------------------------------------------
data <- sim.data(nfactor = nfactor, nitem = nitem, nperson = nperson, itf = itf,
                 varcov = varcov, lmu = lmu, loadings = loadings)

#save the file
write.table(data, paste0(tempdir(),'/','myDataFile_f.dat'), quote = FALSE, 
            sep = " ", col.names = FALSE, row.names = FALSE)

## ---------------------------------------------------------------------------------------
blocks <- matrix(1:nitem, nrow = 1)

## ---- eval =FALSE-----------------------------------------------------------------------
#  # Mplus
#  syntax.mplus(blocks, itf, model = 'lmean', input_path = 'myFC_model_f.inp', data_path =
#                 "myDataFile_f.dat")

## ---------------------------------------------------------------------------------------
#lavaan
modelsyn <- syntax.lavaan(blocks, itf, model = 'lmean')

## ---- eval = FALSE----------------------------------------------------------------------
#  # Mplus
#  system('mplus myFC_model_f.inp', wait = TRUE, show.output.on.console= FALSE)

## ---- eval = FALSE----------------------------------------------------------------------
#  #lavaan
#  results_lav1 <- lavaan::lavaan(modelsyn, data = data, ordered = TRUE, auto.fix.first = FALSE,
#                        auto.var = TRUE, int.lv.free = TRUE, parameterization = "theta",
#                        estimator = 'ULSMV')

## ----eval = FALSE-----------------------------------------------------------------------
#  # Mplus
#  results_mplus1 <- read.mplus(blocks, itf, model = 'lmean', output_path = "myFC_model_f.out")

## ---------------------------------------------------------------------------------------
unlist(results_mplus1$fit)

## ---- eval = FALSE----------------------------------------------------------------------
#  results_lav1 <- lavaan::fitmeasures(results_lav1)[c('chisq.scaled','df.scaled','pvalue.scaled',
#                                       'rmsea.scaled','rmsea.ci.lower.scaled','rmsea.ci.upper.scaled',
#                                       'rmsea.pvalue.scaled','cfi.scaled')]

## ---------------------------------------------------------------------------------------
results_lav1

## ---- eval = FALSE----------------------------------------------------------------------
#  # Mplus
#  results_mplus2 <- fit.mplus(blocks, itf, model = 'lmean', input_path = 'myFC_model_f.inp',
#                              output_path = "myFC_model_f.out", data_path = "myDataFile_f.dat")

## ---- eval =  FALSE---------------------------------------------------------------------
#  #lavaan
#  results_lav2 <- fit.lavaan(blocks, itf, model = 'lmean', data = data)
#  lavaan::fitmeasures(results_lav2)[c('rmsea.scaled','rmsea.ci.lower.scaled','rmsea.ci.upper.scaled',
#                                      'rmsea.pvalue.scaled','cfi.scaled')]

## ---- eval = FALSE----------------------------------------------------------------------
#  # Mplus
#  results_mplus2irt <- fit.mplus(blocks, itf, model = 'irt', input_path = 'myFC_model_irt.inp',
#                                 output_path = "myFC_model_irt.out", data_path = "myDataFile_f.dat")

## ---------------------------------------------------------------------------------------
unlist(results_mplus2irt$fit)

## ---- eval = FALSE----------------------------------------------------------------------
#  #lavaan
#  results_lav2irt <- fit.lavaan(blocks, itf, model = 'irt', data = data)

## ---- eval = FALSE----------------------------------------------------------------------
#  results_lav2irt <- lavaan::fitmeasures(results_lav2irt)[c('chisq.scaled','df.scaled','pvalue.scaled',
#                                       'rmsea.scaled','rmsea.ci.lower.scaled','rmsea.ci.upper.scaled',
#                                       'rmsea.pvalue.scaled','cfi.scaled')]

## ---------------------------------------------------------------------------------------
results_lav2irt

## ---- eval = FALSE----------------------------------------------------------------------
#  scores_results_lav2irt <- lavaan::lavPredict(results_lav2irt)

## ---------------------------------------------------------------------------------------
nfactor <- 5
nitem <- 30
nperson <- 1000
itf <- rep(1:5, 6)
varcov <- diag(1, 5)

# latent utility means
set.seed(69)
lmu <- runif(nitem, -1, 1)
loadings <- runif(nitem, 0.30, 0.95)

## ---------------------------------------------------------------------------------------
set.seed(1234)
data <- sim.data(nfactor = nfactor, nitem = nitem, nperson = nperson, itf = itf, varcov = varcov,
                 lmu = lmu, loadings = loadings)

## ---- eval = FALSE----------------------------------------------------------------------
#  #save the file
#  write.table(data,'myDataFile.dat', quote = FALSE, sep = " ", col.names = FALSE, row.names = FALSE)

## ---------------------------------------------------------------------------------------
blocks <- matrix(sample(1:nitem, nitem), ncol = 3)

## ---------------------------------------------------------------------------------------
pair.combn(blocks)

## ---------------------------------------------------------------------------------------
blocks_sorted <- blocksort(blocks)
pair.combn(blocks_sorted)

## ---------------------------------------------------------------------------------------
# Get names of binary indicators that have non-ascending names
tmp <- which(pair.combn(blocks)[,1] > pair.combn(blocks)[,2])

# get names
pair_names_b <- i.name(blocks)
pair_names_ori <- i.name(1:nitem)

# Rename
pair_names <- i.name(1:nitem)
if(length(tmp) != 0){
  tmp1 <- pair_names_b[tmp]
  tmp2 <- sub('^i.+i','i', tmp1)
  tmp3 <- tmp1
  for(j in 1:length(tmp)){
    tmp3 <- paste0(tmp2[j], sub(paste0(tmp2[j],'$'), '', tmp1[j]))
    pair_names[which(pair_names %in% tmp3)] <- pair_names_b[tmp[j]]
  }
}

tmp <- which(!names(data) %in% pair_names)
# Clone data
data_recoded <- data
# Recode and rename
data_recoded[,tmp] <- abs(data[,tmp]-1)
names(data_recoded) <- pair_names


## ---- eval = FALSE----------------------------------------------------------------------
#  # Save data
#  write.table(data_recoded, 'myDataFile_rec.dat', quote = FALSE, sep = " ", col.names = FALSE,
#              row.names = FALSE)

## ---- eval = FALSE----------------------------------------------------------------------
#  # Blocks_sorted
#  # Mplus
#  results_mplus_b <- fit.mplus(blocks_sorted, itf, model = 'irt', input_path = 'myFC_model_bu.inp',
#                               output_path = "myFC_model_bu.out", data_path = "myDataFile.dat",
#                               data_full = TRUE)

## ---------------------------------------------------------------------------------------
unlist(results_mplus_b$fit)

## ---- eval = FALSE----------------------------------------------------------------------
#  #lavaan
#  results_lav_b <- fit.lavaan(blocks_sorted, itf, model = 'irt', data = data)
#  results_lav_b_fm <- lavaan::fitmeasures(results_lav_b)

## ---- eval = FALSE----------------------------------------------------------------------
#  results_lav_b <- lavaan::fitmeasures(results_lav_b)[c('chisq.scaled','df.scaled','pvalue.scaled',
#                                       'rmsea.scaled','rmsea.ci.lower.scaled','rmsea.ci.upper.scaled',
#                                       'rmsea.pvalue.scaled','cfi.scaled')]

## ---------------------------------------------------------------------------------------
results_lav_b

## ---- eval = FALSE----------------------------------------------------------------------
#  scores_results_lav_b <- lavaan::lavPredict(results_lav_b)

## ----eval = FALSE-----------------------------------------------------------------------
#  # Recoded data
#  # Mplus
#  results_mplus_brec <- fit.mplus(blocks, itf, model = 'irt', input_path = 'myFC_model_brecu.inp',
#                                  output_path = "myFC_model_brecu.out", data_path =
#                                    "myDataFile_rec.dat", data_full = TRUE)

## ---------------------------------------------------------------------------------------
unlist(results_mplus_brec$fit)

## ---- eval = FALSE----------------------------------------------------------------------
#  #lavaan
#  results_lav_brec <- fit.lavaan(blocks, itf, model = 'irt', data = data_recoded)

## ---- eval = FALSE----------------------------------------------------------------------
#  results_lav_brec <- lavaan::fitmeasures(results_lav_brec)[c('chisq.scaled','df.scaled','pvalue.scaled',
#                                       'rmsea.scaled','rmsea.ci.lower.scaled','rmsea.ci.upper.scaled',
#                                       'rmsea.pvalue.scaled','cfi.scaled')]

## ---------------------------------------------------------------------------------------
results_lav_brec

## ---- eval = FALSE----------------------------------------------------------------------
#  scores_results_lav_brec <- lavaan::lavPredict(results_lav_brec)

## ---------------------------------------------------------------------------------------
#save fit measures
tmp <- results_lav_b_fm
fit.correct(1000, blocks, tmp['chisq.scaled'], tmp['df.scaled'], tmp['baseline.chisq.scaled'],
            tmp['baseline.df.scaled'])

## ---------------------------------------------------------------------------------------
blocks

## ---------------------------------------------------------------------------------------
count.xblocks(blocks)

## ---------------------------------------------------------------------------------------
blocks_extra <- matrix(c(23,14,8,24,28,4,25,16,29,22,26,13,1,21,30), ncol = 3, byrow = TRUE)
blocks_con <- rbind(blocks, blocks_extra)

## ---------------------------------------------------------------------------------------
rankA(blocks_con)

## ---------------------------------------------------------------------------------------
metablock(blocks_con)

## ---------------------------------------------------------------------------------------
blocks_extra <- matrix(c(23,14,8,24,28,4,25,16,29,1,21,30), ncol = 3, byrow = TRUE)
blocks_con <- rbind(blocks, blocks_extra)

## ---------------------------------------------------------------------------------------
rankA(blocks_con)

## ---------------------------------------------------------------------------------------
metablock(blocks_con)

## ---------------------------------------------------------------------------------------
blocks_extra <- get.xblocks(blocks, itf, multidim = TRUE, item_not = NULL)
blocks_con <- rbind(blocks, blocks_extra)
blocks_con
blocks_con_sorted <- blocksort(blocks_con)

## ---------------------------------------------------------------------------------------
# Get names of binary indicators that have non-ascending names
tmp <- which(pair.combn(blocks_con)[,1] > pair.combn(blocks_con)[,2])

# get names
pair_names_b <- i.name(blocks_con)
pair_names_ori <- i.name(1:nitem)

# Rename
pair_names <- i.name(1:nitem)
if(length(tmp)!=0){
  tmp1 <- pair_names_b[tmp]
  tmp2 <- sub('^i.+i','i', tmp1)
  tmp3 <- tmp1
  for(j in 1:length(tmp)){
    tmp3 <- paste0(tmp2[j], sub(paste0(tmp2[j],'$'), '', tmp1[j]))
    pair_names[which(pair_names %in% tmp3)] <- pair_names_b[tmp[j]]
  }
}

tmp <- which(!names(data) %in% pair_names)
# Clone data
data_recoded <- data
# Recode and rename
data_recoded[,tmp] <- abs(data[,tmp]-1)
names(data_recoded) <- pair_names

## ---- eval = FALSE----------------------------------------------------------------------
#  # Save data
#  write.table(data_recoded, 'myDataFile_rec.dat', quote = FALSE, sep = " ", col.names = FALSE,
#              row.names = FALSE)

## ---- eval = FALSE----------------------------------------------------------------------
#  # Blocks_sorted
#  # Mplus
#  results_mplus_bc <- fit.mplus(blocks_con_sorted,itf,model='irt',input_path='myFC_model_b_con.inp',
#                               output_path="myFC_model_b_con.out",data_path="myDataFile.dat",
#                               data_full = TRUE)

## ---------------------------------------------------------------------------------------
unlist(results_mplus_bc$fit)

## ---- eval = FALSE----------------------------------------------------------------------
#  #lavaan
#  results_lav_bc <- fit.lavaan(blocks_con_sorted, itf, model = 'irt', data = data)
#  results_lav_bc_fm <- lavaan::fitmeasures(results_lav_bc)

## ---- eval = FALSE----------------------------------------------------------------------
#  results_lav_bc <- lavaan::fitmeasures(results_lav_bc)[c('chisq.scaled','df.scaled','pvalue.scaled',
#                                       'rmsea.scaled','rmsea.ci.lower.scaled','rmsea.ci.upper.scaled',
#                                       'rmsea.pvalue.scaled','cfi.scaled')]

## ---------------------------------------------------------------------------------------
results_lav_bc

## ---- eval = FALSE----------------------------------------------------------------------
#  scores_results_lav_bc <- lavaan::lavPredict(results_lav_bc)

## ----eval = FALSE-----------------------------------------------------------------------
#  # Recoded data
#  # Mplus
#  results_mplus_bcrec <- fit.mplus(blocks_con, itf, model = 'irt', input_path = 'myFC_model_brec.inp',
#                                  output_path = "myFC_model_brec.out",data_path =
#                                    "myDataFile_rec.dat",data_full = TRUE, byblock = FALSE)

## ---------------------------------------------------------------------------------------
unlist(results_mplus_bcrec$fit)

## ---- eval = FALSE----------------------------------------------------------------------
#  #lavaan
#  results_lav_bcrec <- fit.lavaan(blocks_con, itf, model = 'irt', data = data_recoded)

## ---- eval = FALSE----------------------------------------------------------------------
#  results_lav_bcrec <- lavaan::fitmeasures(results_lav_bcrec)[c('chisq.scaled','df.scaled','pvalue.scaled',
#                                       'rmsea.scaled','rmsea.ci.lower.scaled','rmsea.ci.upper.scaled',
#                                       'rmsea.pvalue.scaled','cfi.scaled')]

## ---------------------------------------------------------------------------------------
results_lav_bcrec

## ---- eval = FALSE----------------------------------------------------------------------
#  scores_results_lav_bcrec <- lavaan::lavPredict(results_lav_bcrec)

## ---------------------------------------------------------------------------------------
#save fit measures
tmp <- results_lav_bc_fm
fit.correct(1000, blocks_con, tmp['chisq.scaled'], tmp['df.scaled'], tmp['baseline.chisq.scaled'],
            tmp['baseline.df.scaled'])

## ---------------------------------------------------------------------------------------
#Get the relevant names
tmp_names <- i.name(blocks_con_sorted)

#same example as before
set.seed(1234)
data <- sim.data(nfactor = nfactor, nitem = nitem, nperson = nperson, itf = itf, varcov = varcov,
                 lmu = lmu, loadings = loadings, variables = tmp_names)

## ---- eval = FALSE----------------------------------------------------------------------
#  #save the file
#  write.table(data, 'myDataFile.dat', quote = FALSE, sep = " ", col.names = FALSE,
#              row.names = FALSE)

