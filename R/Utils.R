

'[.OutputsModelDA' <- function(x, i) {
  NbMbr   <- x$NbMbr
  NbTime  <- x$NbTime
  NbState <- x$NbState
  if (!inherits(x, "OutputsModelDA")) {
    stop("'x' must be of class 'OutputsModelDA'")
  }
  res <- airGR:::.ExtractOutputsModel(x, i)
  res$NbMbr   <- NbMbr
  res$NbTime  <- NbTime
  res$NbState <- NbState
  res
}
