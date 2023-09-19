# get orders from 01 format

pord <- function(data,block){
  tmp0 <- utils::combn(block,2)
  tmp0 <- table(c(tmp0[1,which(data==1)],tmp0[2,which(data==0)],block))
  tmp1 <- order(tmp0, decreasing = TRUE)
  return(tmp1)
}
