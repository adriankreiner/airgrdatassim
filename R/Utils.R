'[.Inputs.default' <- function(Inputs, IndRun) {
  res <- lapply(Inputs, function(x) {
    if (is.matrix(x)) {
      res0 <- x[IndRun, ]
    }
    if (is.vector(x) | inherits(x, "POSIXt")) {
      res0 <- x[IndRun]
    }
    if (is.list(x) & !inherits(x, "POSIXt")) {
      res0 <- '[.Inputs.default'(Inputs = x, IndRun = IndRun)
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
  '[.Inputs.default'(Inputs, IndRun)
}
