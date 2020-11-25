plot.OutputsModelDA <- function(x...) {
  
  ## ---------- check arguments
  
  if (!inherits(x, "OutputsModelDA")) {
    stop("'x' must be of class OutputsModelDA")
  }
  
  rangeQsimEns <- apply(x$QsimEns, MARGIN = 2, FUN = range)
  colSim <- "orangered"
  colSimInt <- adjustcolor(colSim, alpha.f = 0.25)
  colObs <- par("fg")
  plot(x = x$DatesR, y = colMeans(x$QsimEns),
       ylim = range(rangeQsimEns, BasinObs$Qmm, na.rm = TRUE),
       type = "l", col = colSim, lwd = 2,
       main = "EnKF-based discharge simulations",
       xlab = "Time [day]", ylab = "Discharge [mm/day]",
       panel.first = polygon(x = c(as.numeric(x$DatesR),
                                   rev(as.numeric(x$DatesR))),
                             y = c(rangeQsimEns[1L, ], rev(rangeQsimEns[2L, ])),
                             col = colSimInt, border = NA),
       panel.last = lines(x = x$DatesR,
                          y = BasinObs$Qmm[IndRun],
                          type = "l", col = colObs, lwd = 2))
  legend("topright", legend = c("DA-based", "obs"),
         lty = 1, col = c(colSim, colObs))
  
}