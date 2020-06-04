#' Merge dispensations so that the implied treatment periods are not overlapping.
#' @param data The data frame.
#' @param dates The column name containing the dates of treatment initiations.
#' @param doses The column name containing the treatment length.
#' @param cumilative Indicate swhether the days in overlapping treatment periods should accumulate.
#' @return A data frame without overlapping periods.
#' @export
#' @examples mergePeriods()
#' @details Given data with a column of dates ("date") as a date variable, a column of quantity dispensed ("quantity"),
#' mergePeriods(data, "date", "quantity", FALSE) returns an aggregation of data where overlapping periods
#' have been merged to a single period.

mergePeriods <- function (data, dates, doses, cumulative=F) {
  n <- nrow(data)
  # If only one row or no rows, just return that
  if (n<=1) return(data)
  # For each row after the first
  for (i in 2:n) {
    # If the predicted end date from the preceeding dispensation overlaps the current, merge these
    if (data[i,doses] > 0 & data[i-1,dates]+data[i-1,doses] >= data[i,dates]) {
      if (cumulative==T) data[i-1,doses] <- data[i-1,doses] + data[i,doses]#-(data[i,dates]-data[i-1,dates])
      else data[i-1,doses] <- (data[i-1,dates] + data[i-1,doses] - data[i,dates]) + data[i,doses]
      # Silence this row
      data[i,doses] <- 0
      # If a doses is negative, this medication is returned and is deducted from the prior dispensation
    } else if (data[i,doses] < 0) {
      data[i-1,doses] <- data[i-1,doses] + data[i, doses]
      data[i,doses] <- 0
    }
  }
  # If no silenced rows exist, return data otherwise recurse since more overlapping rows might exist
  if (sum(data[,doses] == 0)==0) return(data)
  else return(mergePeriods(data[data[,doses] != 0,],dates,doses))
}
