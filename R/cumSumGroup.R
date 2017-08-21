cumSumGroup <- function(x,group) {
  if (length(x) != length(group))
    stop('Length of x and group differs')
  r <- .C('cumSumGroup', w = as.double(x), as.integer(length(x)), as.integer(group))
  return(r$w)
}
