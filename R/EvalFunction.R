EvalFunction <- function(txt, cols) {
  # Evaluates an R expression
  
  d <- list()
  
  ids <- sapply(cols, function(i) i$id)
  
  for (i in seq(along=ids)) {
    id.quoted <- paste("\"", ids[i], "\"", sep="")
    if (regexpr(id.quoted, txt, fixed=TRUE)[1] >= 0) {
      if (is.null(cols[[i]]$index)) {
        d[[i]] <- EvalFunction(cols[[i]]$fun, cols)
      } else {
        d[[i]] <- Data("data.raw")[, cols[[i]]$index]
        if (!is.null(cols[[i]]$digits)) 
          d[[i]] <- round(d[[i]], cols[[i]]$digits)
      }
    }
  }
  
  fun <- txt
  ids.quoted <- paste("\"", ids, "\"", sep="")
  for (i in seq(along=ids.quoted)) 
    fun <- gsub(ids.quoted[i], i, fun, fixed=TRUE)
  
  fun <- eval(parse(text=paste("function(DATA) {", fun, "}", sep="")))
  
  rtn <- try(fun(d), silent=TRUE)
  
  if (inherits(rtn, "try-error")) 
    return(rtn)
  
  n <- length(rtn)
  nrows <- nrow(Data("data.raw"))
  if (n == 0 || (!is.null(nrows) && n != nrows))  
    return("length-error")
  if (is.numeric(rtn)) 
    rtn[!is.finite(rtn)] <- NA
  
  rtn
}
