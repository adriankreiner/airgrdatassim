plot.OutputsModelDA <- function(x, Qobs = NULL, ...) {
  
  ## ---------- check arguments
  
  ## class
  if (!inherits(x, "OutputsModelDA")) {
    stop("'x' must be of class OutputsModelDA")
  }
  
  ## Qobs
  if (!is.numeric(Qobs) || length(Qobs) != length(x$DatesR)) {
    Qobs <- NULL
    warning("'Qobs' is not a numeric of the same length as Qsim. Time series of observed flow not drawn")
  }
  

  ## ---------- graphical variables
  
  TimeUnit <- c("day", "hour")
  TimeUnit <- match.arg(class(x), TimeUnit, several.ok = TRUE)
  DaMethod <- c("EnKF", "PF", "none")
  DaMethod <- match.arg(class(x), DaMethod, several.ok = TRUE)
  DaMethod <- gsub(pattern = "none", replacement = "OpenLoop", x = DaMethod)
  
  if (!is.null(Qobs)) {
    ColObs <- par("fg")
    LegObs <- "obs"
  } else {
    Qobs <- rep(NA, length(x$DatesR))
    ColObs <- NULL
    LegObs <- NULL
  }
  RangeQsimEns <- apply(x$QsimEns, MARGIN = 2, FUN = range)
  ColSim <- "orangered"
  ColSimInt <- adjustcolor(ColSim, alpha.f = 0.25)
  Pal <- c(ColObs, ColSim)
  Leg <- c(LegObs, sprintf("sim (%s)", DaMethod))
  
  ## ---------- plot
  
  plot(x = x$DatesR, y = colMeans(x$QsimEns),
       ylim = range(RangeQsimEns, Qobs, na.rm = TRUE),
       type = "l", col = ColSim, lwd = 2,
       main = sprintf("%s-based discharge simulations", DaMethod),
       xlab = sprintf("Time [%s]", TimeUnit), ylab = sprintf("Discharge [mm/%s]", TimeUnit),
       panel.first = polygon(x = c(as.numeric(x$DatesR),
                                   rev(as.numeric(x$DatesR))),
                             y = c(RangeQsimEns[1L, ], rev(RangeQsimEns[2L, ])),
                             col = ColSimInt, border = NA),
       panel.last = lines(x = x$DatesR,
                          y = Qobs,
                          type = "l", col = ColObs, lwd = 2))
  legend("topright", legend = Leg,
         lty = 1, col = Pal)
  
}