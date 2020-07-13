#' Reduce the data to summary statistics along factors.
#' @param x See aggregate for details of how to specify x.
#' @param data The data.
#' @param ... Functions that return a single value, specified as name=functionName.
#' @return A data frame.
#' @export
#' @details This function is just a wrapper for aggregate, but add
#' a simple way to use several summary functions.
#' aggregate2(cbind(varA=funA, varB=funB)~cluster1 + ..., data=data)
#' data(varA_funA, varB_funB, ...)
aggregateFuns <- function (x, data, ...) {
  lst <- c(...)
  colNames <- names(lst)
  funStr <- 'function (x) c('
  for (y in colNames) {
    fun <- attr(lst[[y]],which = 'generic')
    funStr <- paste0(funStr,y,'=',fun,'(x), ')
  }
  funStr <- paste0(substr(funStr,1,nchar(funStr)-2),')')
  out <- aggregate(x, data=data, FUN=eval(parse(text=funStr)))
  out <- do.call(data.frame, out)
  return(out)
}
#hmm <- aggregateFuns(cbind(forpddd=forpddd, days=days)~MotherId, data=tst,mean=mean,sd=mean)
  #print(terms(x))
  #  print(attr(terms(formula(x)),'variables')[-1])
  #  terms <- as.list(attr(terms(formula(x)),'variables'))[-1]
  #  lhs <- terms[[1]]
  #  colNames <- colnames(eval(lhs))
  #print(lhs)
  #  funs <- all.vars(lhs,functions=T,unique=F)
  #  funs <- funs[2:length(funs)]

  #print(all.names(lhs))
  #print(colnames(lhs))
  #print()
  #print(eval(quote(lhs),data))
