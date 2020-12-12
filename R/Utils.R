.ExtractInputsModel <- function(Inputs, i) {
  res <- lapply(Inputs, function(x) {
    if (is.matrix(x)) {
      res0 <- x[i, ]
    }
    if (is.vector(x) | inherits(x, "POSIXt")) {
      res0 <- x[i]
    }
    if (is.list(x) & !inherits(x, "POSIXt")) {
      res0 <- .ExtractInputsModel(Inputs = x, i = i)
    }
    return(res0)
  })
  if (!is.null(Inputs$ZLayers)) {
    res$ZLayers <- Inputs$ZLayers
  }
  class(res) <- class(Inputs)
  res
}


'[.InputsModel' <- function(Inputs, i) {
  if (!inherits(Inputs, "InputsModel")) {
    stop("'Inputs' must be of class 'InputsModel'")
  }
  .ExtractInputsModel(Inputs, i)
}

.ExtractOutputsModel <- function(Outputs, i) {
  IsStateEnd <- !is.null(Outputs$StateEnd)
  if (IsStateEnd) {
    IsStateEnd <- TRUE
    StateEnd <- Outputs$StateEnd
    Outputs$StateEnd <- NULL
  }
  res <- lapply(Outputs, function(x) {
    if (is.matrix(x)  && length(dim(x)) == 2L) {
      res0 <- x[, i]
    }
    if (is.array(x) && length(dim(x)) == 3L) {
      res0 <- x[, , i]
    }
    if (is.vector(x) | inherits(x, "POSIXt")) {
      res0 <- x[i]
    }
    if (is.list(x) & !inherits(x, "POSIXt")) {
      res0 <- .ExtractOutputsModel(Outputs = x, i = i)
    }
    return(res0)
  })
  if (IsStateEnd) {
    res$StateEnd <- StateEnd
  }
  class(res) <- class(Outputs)
  res
}

'[.OutputsModel' <- function(Outputs, i) {
  if (!inherits(Outputs, "OutputsModel")) {
    stop("'Outputs' must be of class 'OutputsModel'")
  }
  .ExtractOutputsModel(Outputs, i)
}

'[.OutputsModelDA' <- function(Outputs, i) {
  NbMbr   <- Outputs$NbMbr
  Nt      <- Outputs$Nt
  NbState <- Outputs$NbState
  if (!inherits(Outputs, "OutputsModelDA")) {
    stop("'Outputs' must be of class 'OutputsModelDA'")
  }
  res <- .ExtractOutputsModel(Outputs, i)
  res$NbMbr   <- NbMbr
  res$Nt      <- Nt
  res$NbState <- NbState
  res
}
