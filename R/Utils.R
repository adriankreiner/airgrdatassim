.ExtractInputsModel <- function(x, i) {
  res <- lapply(x, function(x) {
    if (is.matrix(x)) {
      res0 <- x[i, ]
    }
    if (is.vector(x) | inherits(x, "POSIXt")) {
      res0 <- x[i]
    }
    if (is.list(x) & !inherits(x, "POSIXt")) {
      res0 <- .ExtractInputsModel(x = x, i = i)
    }
    return(res0)
  })
  if (!is.null(x$ZLayers)) {
    res$ZLayers <- x$ZLayers
  }
  class(res) <- class(x)
  res
}


'[.InputsModel' <- function(x, i) {
  if (!inherits(x, "InputsModel")) {
    stop("'x' must be of class 'InputsModel'")
  }
  .ExtractInputsModel(x, i)
}

.ExtractOutputsModel <- function(x, i) {
  IsStateEnd <- !is.null(x$StateEnd)
  if (IsStateEnd) {
    IsStateEnd <- TRUE
    StateEnd <- x$StateEnd
    x$StateEnd <- NULL
  }
  res <- lapply(x, function(x) {
    if (is.matrix(x)  && length(dim(x)) == 2L) {
      res0 <- x[i, ]
    }
    if (is.array(x) && length(dim(x)) == 3L) {
      res0 <- x[i, , ]
    }
    if (is.vector(x) | inherits(x, "POSIXt")) {
      res0 <- x[i]
    }
    if (is.list(x) & !inherits(x, "POSIXt")) {
      res0 <- .ExtractOutputsModel(x = x, i = i)
    }
    return(res0)
  })
  if (IsStateEnd) {
    res$StateEnd <- StateEnd
  }
  class(res) <- class(x)
  res
}

'[.OutputsModel' <- function(x, i) {
  if (!inherits(x, "OutputsModel")) {
    stop("'x' must be of class 'OutputsModel'")
  }
  .ExtractOutputsModel(x, i)
}

'[.OutputsModelDA' <- function(x, i) {
  NbMbr   <- x$NbMbr
  NbTime  <- x$NbTime
  NbState <- x$NbState
  if (!inherits(x, "OutputsModelDA")) {
    stop("'x' must be of class 'OutputsModelDA'")
  }
  res <- .ExtractOutputsModel(x, i)
  res$NbMbr   <- NbMbr
  res$NbTime  <- NbTime
  res$NbState <- NbState
  res
}
