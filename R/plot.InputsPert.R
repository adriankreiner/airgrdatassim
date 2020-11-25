plot.InputsPert <- function(x, which = "all", main = NULL,
                            ColPrecip = "royalblue", ColPotEvap = "green3", ...) {
  
  ## ---------- check arguments
  
  ## class
  if (!inherits(x, "InputsPert")) {
    stop("'x' must be of class InputsPert")
  }
  
  ## which
  NamesInputsPert <- c("Precip", "PotEvap")
  which <- match.arg(arg = which, choices = c("all", NamesInputsPert), several.ok = TRUE)
  if (which == "all") {
    which <- NamesInputsPert
  }
  NamesInputsPert <- match.arg(names(InputsPert), NamesInputsPert, several.ok = TRUE)
  NamesInputsPert <- NamesInputsPert
  
  
  ## ---------- graphical variables
  
  TimeUnit <- c("daily", "hourly")
  TimeUnit <- match.arg(class(x), TimeUnit, several.ok = TRUE)
  TimeUnit <- switch(TimeUnit,
                     daily  = "day",
                     hourly = "hour")
  
  
  for (i in seq_along(NamesInputsPert)) {
    
    iNames <- NamesInputsPert[i]
    IsPrecip <- iNames == "Precip"
    Main <- ifelse(test = IsPrecip, yes = "Precipitation", no = "Potential evapotranspiration")
    YLab <- ifelse(test = IsPrecip, yes = "total precip",  no = "pot. evap.")
    Col  <- ifelse(test = IsPrecip, yes = ColPrecip,       no = ColPotEvap)
    Main <- ifelse(test = is.null(main),
                   yes = sprintf("%s ensemble", Main),
                   no = main[i])
    
    
    ## ---------- plot
    
    iRangeInputsPert <- apply(x[[iNames]], MARGIN = 1, FUN = range) 
    plot(x = x$DatesR, y = rowMeans(x[[iNames]]),
         ylim = range(iRangeInputsPert),
         type = "l", col = Col, lwd = 2,
         main = Main,
         xlab = sprintf("time [%s]", TimeUnit), ylab = sprintf("%s [mm/%s]", YLab, TimeUnit),
         panel.first = polygon(x = c(as.numeric(x$DatesR), rev(as.numeric(x$DatesR))),
                               y = c(iRangeInputsPert[1L, ], rev(iRangeInputsPert[2L, ])),
                               col = adjustcolor(Col, alpha.f = 0.25), border = NA))
  }
  
  
}