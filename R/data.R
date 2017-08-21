#' Simulated survival data
#'
#' Individuals whos censoring/death hazards depend on a binary baseline variable u
#'
#' @format A data frame with 300 rows and 6 variables:
#' \describe{
#'   \item{id}{Individual}
#'   \item{from}{Start time}
#'   \item{to}{Event time}
#'   \item{from.state}{Start state}
#'   \item{to.state}{Event state}
#'   \item{u}{Binary vaiable}
#' }
"fFrame"