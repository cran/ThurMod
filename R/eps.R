eps <- function (x = 1){
  stopifnot(is.numeric(x))
  x <- max(abs(x))
  if (x < .Machine$double.xmin) {
    e <- .Machine$double.xmin
  }
  else {
    e <- 2^floor(log2(x)) * .Machine$double.eps
  }
  return(e)
}

