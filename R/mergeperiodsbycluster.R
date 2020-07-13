#' Merge dispensations so that the implied treatment periods are not overlapping.
#' @param times A vector of timepoints (e.g, medication dispensations).
#' @param lengths A vector of treatment lengths.
#' @param data A data frame.
#' @param cumilative Indicate swhether the days in overlapping treatment periods should accumulate.
#' @param ... Names of clustering variables in data.
#' @return A data frame without overlapping periods with one column per cluster.
#' @export
#' @examples mergePeriodsByCluster()
#' @details This function applies mergePeriods() across clusters (e.g., individuals).

mergePeriodsByCluster <- function(times, lengths, data, cumulative=F, ...) {
  clusters <- substitute(...)
  dta <- by(data, INDICES=list(eval(substitute(...),data)),
            FUN=function (dta, times, lengths, cumulative, clusters) {
              cbind(unique(dta[,deparse(clusters)]),mergePeriods(times=eval(times, dta), lengths=eval(lengths, dta), cumulative));
              },
            times=substitute(times), lengths=substitute(lengths),cumulative=cumulative,clusters=clusters)
  dta <- data.frame(do.call(rbind, dta))
  colnames(dta) <- c(clusters,'times','lengths')
  return(dta)
}
