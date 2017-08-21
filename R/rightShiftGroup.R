rightShiftGroup <- function(x, group, initVal) {
  if (length(x) != length(group))
    stop('Length of x and group differs')
  r <- .C('rightShiftGroup', w = as.double(x), as.integer(length(x)),
          as.integer(group), as.double(initVal), as.integer(length(initVal)))
  return(r$w)
}
