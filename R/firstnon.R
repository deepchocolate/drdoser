#' Take the first occurring observation in a vector not equal to a specified value along a vector.
#' @param value The value searched for.
#' @param values The values to search for value in.
#' @return NA if value is not in values or the value.
#' @examples firstNon()
#' @details Given the vector values=c(1,2,3) and value=1,
#' firstNon(1, c(1,2,3)) returns 2.
firstNon <- function (value, values) {
  out <- values[values != value][1]
  if (length(out) == 0) out <- NA
  else out <- out[1]
  return(out)
}
