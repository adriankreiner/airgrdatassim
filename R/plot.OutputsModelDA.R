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
  
  timeUnit <- c("day", "hour")
  timeUnit <- match.arg(class(x), timeUnit, several.ok = TRUE)
  if (!is.null(Qobs)) {
    colObs <- par("fg")
    legObs <- "obs"
  } else {
    Qobs <- rep(NA, length(x$DatesR))
    colObs <- NULL
    legObs <- NULL
  }
  rangeQsimEns <- apply(x$QsimEns, MARGIN = 2, FUN = range)
  colSim <- "orangered"
  colSimInt <- adjustcolor(colSim, alpha.f = 0.25)
  pal <- c(colObs, colSim)
  leg <- c(legObs, "DA-based")
  
  ## ---------- plot
  
  plot(x = x$DatesR, y = colMeans(x$QsimEns),
       ylim = range(rangeQsimEns, Qobs, na.rm = TRUE),
       type = "l", col = colSim, lwd = 2,
       main = "EnKF-based discharge simulations",
       xlab = sprintf("Time [%s]", timeUnit), ylab = sprintf("Discharge [mm/%s]", timeUnit),
       panel.first = polygon(x = c(as.numeric(x$DatesR),
                                   rev(as.numeric(x$DatesR))),
                             y = c(rangeQsimEns[1L, ], rev(rangeQsimEns[2L, ])),
                             col = colSimInt, border = NA),
       panel.last = lines(x = x$DatesR,
                          y = Qobs,
                          type = "l", col = colObs, lwd = 2))
  legend("topright", legend = leg,
         lty = 1, col = pal)
  
}