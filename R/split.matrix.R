split.matrix <- function(mat, r=NULL, c=NULL){

  if(is.null(r)){
    r <- nrow(mat)
  }
  if(is.null(c)){
    c <- ncol(mat)
  }
  
  rg <- (row(mat)-1)%/%r+1
  cg <- (col(mat)-1)%/%c+1
  rci <- (rg-1)*max(cg) + cg
  N <- prod(dim(mat))/r/c
  cv <- lapply(1:N, function(x) mat[rci==x])
  return(cv)
}
