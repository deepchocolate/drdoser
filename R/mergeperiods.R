#' Merge dispensations so that the implied treatment periods are not overlapping.
#' @param times A vector of timepoints (e.g, medication dispensations).
#' @param lengths A vector of treatment lengths.
#' @param cumilative Indicate swhether the days in overlapping treatment periods should accumulate.
#' @return A data frame without overlapping periods.
#' @export
#' @examples mergePeriods()
#' @details Given the vectors times and lengths,
#' mergePeriods(data, "date", "quantity", FALSE) returns an aggregation of data where overlapping periods
#' have been merged to a single period. times and lengths need to be paired, and sorted ascendingly.

mergePeriods <- function (times, lengths, cumulative=F) {
  n <- length(times)
  if (n != length(lengths)) stop('The length of the vectors days and dates differ.')
  # If only one row or no rows, just return that
  if (n == 1) return(cbind(times, lengths))
  while (T) {
    # For each row after the first
    for (i in 2:n) {
      # If the predicted end date from the preceeding treatment length overlaps the current,
      # merge these to one period
      if (times[i-1] + lengths[i-1] >= times[i]) {
        if (cumulative == T) lengths[i-1] <- lengths[i-1] + lengths[i]
        else lengths[i-1] <- (times[i] - times[i-1]) + lengths[i]
        # Mark that this period should be removed
        lengths[i] <- 0
      }
    }
    # Remove periods that have been merged
    subs <- lengths != 0
    lengths <- lengths[subs]
    times <- times[subs]
    # If no dates have been removed, exit the loop.
    if (sum(subs == F) == 0) break;
  }
  return(cbind(times,lengths))
}
