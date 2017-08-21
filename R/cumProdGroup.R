cumProdGroup <- function(x,group) {
  if (length(x) != length(group))
    stop('Length of x and group differs')
  r <- .C('cumProdGroup', w = as.double(x), as.integer(length(x)), as.integer(group))
  return(r$w)
}
