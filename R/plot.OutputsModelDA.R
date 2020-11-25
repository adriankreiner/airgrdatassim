plot.OutputsModelDA <- function(x, Qobs = NULL, main = NULL,
                                ColSim = "orangered", ColObs = par("fg"), ...) {
  
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
  
  RangeQsimEns <- apply(x$QsimEns, MARGIN = 2, FUN = range)
  
  if (!is.null(Qobs)) {
    LegObs <- "obs"
  } else {
    Qobs <- rep(NA, length(x$DatesR))
    ColObs <- NULL
    LegObs <- NULL
  }
  
  ColSimInt <- adjustcolor(ColSim, alpha.f = 0.25)
  Pal <- c(ColObs, ColSim)
  
  TimeUnit <- c("daily", "hourly")
  TimeUnit <- match.arg(class(x), TimeUnit, several.ok = TRUE)
  TimeUnit <- switch(TimeUnit,
                     daily  = "day",
                     hourly = "hour")
  DaMethod <- c("EnKF", "PF", "none")
  DaMethod <- match.arg(class(x), DaMethod, several.ok = TRUE)
  DaMethod <- gsub(pattern = "none", replacement = "OpenLoop", x = DaMethod)
  
  Main <- ifelse(test = is.null(main),
                 yes = sprintf("%s-based discharge simulations", DaMethod),
                 no = main)
  Leg <- c(LegObs, sprintf("sim (%s)", DaMethod))
  
  
  ## ---------- plot
  
  plot(x = x$DatesR, y = colMeans(x$QsimEns),
       ylim = range(RangeQsimEns, Qobs, na.rm = TRUE),
       type = "l", col = ColSim, lwd = 2,
       main = Main,
       xlab = sprintf("Time [%s]", TimeUnit), ylab = sprintf("Discharge [mm/%s]", TimeUnit),
       panel.first = polygon(x = c(as.numeric(x$DatesR), rev(as.numeric(x$DatesR))),
                             y = c(RangeQsimEns[1L, ], rev(RangeQsimEns[2L, ])),
                             col = ColSimInt, border = NA),
       panel.last = lines(x = x$DatesR,
                          y = Qobs,
                          type = "l", col = ColObs, lwd = 2))
  legend("topright", legend = Leg,
         lty = 1, col = Pal)
  
}