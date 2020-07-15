#' Replace values in columns of data with new values.
#' @param data Any data object that can be coerced to a data frame.
#' @param ... column=value pairs, or column=column,
#' @return A data frame.
#' @export
#' @example replaceInData(data.frame(a=1:5, b=6:10), a=b, b=1)
#' @details This function replaces provided values in the columns of data.
#' For column=value, the column is assigned value while if column=otherColumn
#' the values in otherColumn replaces values in column. This function can be useful
#' when doing prediction from a fitted model object and one wishes to compare different
#' scenarios by manipulating the source data.
replaceInData <- function (data, ...) {
  lst <- eval(substitute(list(...)), as.data.frame(data))
  colNames <- colnames(data)
  newColumns <- names(lst)
  for (col in newColumns) {
    if (!col %in% colNames) stop(paste0('Column ', col, 'was not found in the columns of data.'));
    data[,col] <- lst[[col]]
  }
  return(data)
}
