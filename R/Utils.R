.ExtractInputsModel <- function(Inputs, IndRun) {
  res <- lapply(Inputs, function(x) {
    if (is.matrix(x)) {
      res0 <- x[IndRun, ]
    }
    if (is.vector(x) | inherits(x, "POSIXt")) {
      res0 <- x[IndRun]
    }
    if (is.list(x) & !inherits(x, "POSIXt")) {
      res0 <- .ExtractInputsModel(Inputs = x, IndRun = IndRun)
    }
    return(res0)
  })
  if (!is.null(Inputs$ZLayers)) {
    res$ZLayers <- Inputs$ZLayers
  }
  class(res) <- class(Inputs)
  res
}


'[.InputsModel' <- function(Inputs, IndRun) {
  if (!inherits(Inputs, "InputsModel")) {
    stop("'Inputs' must be of class 'InputsModel'")
  }
  .ExtractInputsModel(Inputs, IndRun)
}

.ExtractOutputsModel <- function(Outputs, IndRun) {
  IsStateEnd <- !is.null(Outputs$StateEnd)
  if (IsStateEnd) {
    IsStateEnd <- TRUE
    StateEnd <- Outputs$StateEnd
    Outputs$StateEnd <- NULL
  }
  res <- lapply(Outputs, function(x) {
    if (is.matrix(x)  && length(dim(x)) == 2L) {
      res0 <- x[, IndRun]
    }
    if (is.array(x) && length(dim(x)) == 3L) {
      res0 <- x[, , IndRun]
    }
    if (is.vector(x) | inherits(x, "POSIXt")) {
      res0 <- x[IndRun]
    }
    if (is.list(x) & !inherits(x, "POSIXt")) {
      res0 <- .ExtractOutputsModel(Outputs = x, IndRun = IndRun)
    }
    return(res0)
  })
  if (IsStateEnd) {
    res$StateEnd <- StateEnd
  }
  class(res) <- class(Outputs)
  res
}

'[.OutputsModel' <- function(Outputs, IndRun) {
  if (!inherits(Outputs, "OutputsModel")) {
    stop("'Outputs' must be of class 'OutputsModel'")
  }
  .ExtractOutputsModel(Outputs, IndRun)
}

'[.OutputsModelDA' <- function(Outputs, IndRun) {
  NbMbr   <- Outputs$NbMbr
  Nt      <- Outputs$Nt
  NbState <- Outputs$NbState
  if (!inherits(Outputs, "OutputsModelDA")) {
    stop("'Outputs' must be of class 'OutputsModelDA'")
  }
  res <- .ExtractOutputsModel(Outputs, IndRun)
  res$NbMbr   <- NbMbr
  res$Nt      <- Nt
  res$NbState <- NbState
  res
}
