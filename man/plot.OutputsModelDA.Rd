\encoding{UTF-8}


\name{plot}
\alias{plot.OutputsModelDA}
\alias{plot}


\title{Default preview of DA model outputs}


\description{
Function which creates a plot of time series of DA model outputs.
}


\usage{
\method{plot}{OutputsModelDA}(x, Qobs = NULL, main = NULL,
     ColSim = "orangered", ColObs = par("fg"), ...)
}


\arguments{
\item{x}{[object of class \emph{OutputsModelDA}] list of model outputs (see \code{\link{RunModel_DA}})}

\item{Qobs}{(optional) [numeric] time series of observed flow (for the same time steps than simulated) [mm/time step]}

\item{main}{(optional) [character] an overall title for the plot (see \code{\link[graphics]{title}})}

\item{ColSim}{(optional) [character] color to be used for simulated flow (in any format that \code{\link[grDevices]{col2rgb}} accepts)}

\item{ColObs}{(optional) [character] color to be used for observed flow}

\item{...}{other parameters to be passed through to plotting functions}
}


\author{
Olivier Delaigue, Gaia Piazzi
}


\seealso{
\code{\link{RunModel_DA}}
}
