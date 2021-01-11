
## =================================================================================
## function to extract parts of InputsModel or .OutputsModelDA object
## =================================================================================

'[.OutputsModelDA' <- function(x, i) {
  if (!inherits(x, "OutputsModelDA")) {
    stop("'x' must be of class 'OutputsModelDA'")
  }
  if (is.factor(i)) {
    i <- as.character(i)
  }
  if (is.numeric(i)) {
    res <- airGR:::.ExtractOutputsModel(x, i)
    res$NbMbr   <- x$NbMbr
    res$NbTime  <- x$NbTime
    res$NbState <- x$NbState
  } else {
    res <- NextMethod()
  }
  return(res)
}
