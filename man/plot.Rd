\encoding{UTF-8}


\name{plot}
\alias{plot.InputsPert}
\alias{plot.OutputsModelDA}
\alias{plot}


\title{Default preview of perturbed inputs and DA model outputs}


\description{
Function which creates time series plots for perturbed inputs and DA model outputs.
}


\usage{
\method{plot}{InputsPert}(x, which = "all", main = NULL,
     ColPrecip = "royalblue", ColPotEvap = "green3",
     ask = prod(par("mfcol")) < length(which) && dev.interactive(), ...)
\method{plot}{OutputsModelDA}(x, Qobs = NULL, main = NULL,
     ColSim = "orangered", ColObs = par("fg"), ...)
}


\arguments{
\item{x}{[InputsPert] or [OutputsModelDA] containing the vector of dates (\emph{POSIXt}) and the time series of numeric values list perturbed inputs and DA model outputs (see \code{\link{CreateInputsPert}} and \code{\link{RunModel_DA}})}

\item{which}{(optional) [character] choice of plots (e.g. \code{"Precip"} or \code{"PotEvap"}, default = \code{"all"})}

\item{main}{(optional) [character] an overall title for the plot (see \code{\link[graphics]{title}})}

\item{ColPrecip}{(optional) [character] color to be used for perturbed precipitation (in any format that \code{\link[grDevices]{col2rgb}} accepts)}

\item{ColPotEvap}{(optional) [character] color to be used for perturbed potential evapotransipration (in any format that \code{\link[grDevices]{col2rgb}} accepts)}

\item{ask}{(optional) [logical] if \code{TRUE}, the user is asked before each plot, see \code{\link[graphics]{par}(ask = .)}}

\item{Qobs}{(optional) [numeric] time series of observed flow [mm/day]}

\item{ColSim}{(optional) [character] color to be used for simulated flow (in any format that \code{\link[grDevices]{col2rgb}} accepts)}

\item{ColObs}{(optional) [character] color to be used for observed flow} (in any format that \code{\link[grDevices]{col2rgb}} accepts)

\item{...}{other parameters to be passed through to plotting functions}
}


\value{
solid line: medians of the input values \cr
polygon: maxima and minima of the input values
}

\author{
Olivier Delaigue, Gaia Piazzi
}


\seealso{
\code{\link{CreateInputsPert}}, \code{\link{RunModel_DA}}
}


\examples{
### Generation of ensemble model inputs for data assimilation
example(CreateInputsPert)

## results preview
par(mfrow = c(2, 1))
plot(InputsPert)

## results preview on a subset on one perturbed variable
par(mfrow = c(1, 1))
plot(InputsPert[IndRun], which = "PotEvap")


### Run discharge simulations with ensemble-based data assimilation
example(RunModel_DA)

## results preview
plot(OutputsModelDA, Qobs = BasinObs$Qmm[IndRun])
}
