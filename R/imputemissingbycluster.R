#' Impute missing using the nearest observation (carry-forward/backward)
#' @param x A numeric.
#' @return x without missing values when there is at least one non-missing value.
#' @export
#' @examples mergePeriodsByCluster()
#' @details This function applies mergePeriods() across clusters (e.g., individuals).
imputeByNearest <- function (x) {
  n <- length(x)
  if (sum(is.na(x)) == n | n == 1) return(x)
  # Run until no missing observations exist
  while (sum(is.na(x)) > 0) {
    for (i in 2:n) {
      # Carry forward
      if (is.na(x[i]) & !is.na(x[i-1])) x[i] <- x[i-1]
      # Carry backward
      else if (!is.na(x[i]) & is.na(x[i-1])) x[i-1] <- x[i]
    }
  }
}
# Fix missing days. Replace missing with closest non-missing observation.
replaceNAs <- function (x) {
  for (i in 1:length(x)) {
    if (i == 1 & is.na(x[1])) {
      if (!is.na(x[2])) x[1] <- x[2]
    } else if (is.na(x[i])) {
      if (!is.na(x[i-1])) {x[i] <- x[i-1];}
      else if (!is.na(x[i+1])) {x[i] <- x[i+1];}
    }
  }
  # Return x if all or none observations are missing
  if (sum(is.na(x))==0 | sum(is.na(x))==length(x)) {return(x);}
  else {return(replaceNAs(x));}
}
