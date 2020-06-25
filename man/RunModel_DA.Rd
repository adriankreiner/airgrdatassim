\encoding{UTF-8}


\name{RunModel_DA}
\alias{RunModel_DA}

\title{Run discharge simulations with ensemble-based data assimilation}

\usage{
RunModel_DA(DaMethod, IndRun, IndState, NbMbr,
            FUN_MOD, InputsModel, InputsPert = NULL, Param,
            IsState, Qobs = NULL, Seed = NULL)
}

\arguments{

\item{DaMethod}{[character] name of the data assimilation technique:
  \tabular{ll}{                                                                      
    if DaMethod = 'EnKF'   \tab discharge assimilation via Ensemble Kalman filter                   \cr
    if DaMethod = 'PF'     \tab discharge assimilation via Particle filter                          \cr
    if DaMethod = 'none'   \tab discharge assimilation is not performed (i.e. open-loop simulation) \cr
  }} 
  
\item{IndRun}{[numeric] index of period to be used for the model run [-]}

\item{IndState}{[character] vector of the names of state variables to be updated and/or perturbed (see \emph{IsState}):
  \tabular{ll}{                                                        
    Prod \tab level of the production store [mm] \cr      
    Rout \tab level of the routing store [mm]    \cr      
    UH1  \tab unit hydrograph 1 levels [mm] (not defined in GR5J model)     \cr    
    UH2  \tab unit hydrograph 2 levels [mm]
  }}  
  
\item{NbMbr}{[numeric] number of ensemble members}

\item{FUN_MOD}{[function] daily hydrological model functions (GR4J, GR5J or GR6J; e.g. \code{\link{RunModel_GR5J}}, \code{\link{RunModel_CemaNeigeGR5J}})}

\item{InputsModel}{[list] object of class \emph{InputsModel} containing the data required to run the model  (see \code{\link{CreateInputsModel}} for details)}

\item{InputsPert}{(optional) [list] object of class \emph{InputsModel} containing the ensembles of perturbed data required to evaluate the model probabilistic outputs, if the uncertainty in meteorological forcings is taken into account. The variables data proposed in \emph{InputsPert} will erase the variables data given in \emph{InputsModel}}

\item{Param}{[numeric] vector of model parameters (number of parameters depends on the used hydrological model, from 4 parameters in \emph{RunModel_GR4J} up to 10 parameters in \emph{RunModel_CemaNeigeGR6J})}
  
\item{IsState}{[logical] flag to enable/disable the perturbation of state variables defined in \emph{IndState}:
  \tabular{ll}{                                                                      
    if IsState = TRUE  \tab perturbation of state variables      \cr
    if IsState = FALSE \tab no perturbation of state variables  \cr
  }}
  
\item{Qobs}{(optional) [numeric] time series of observed discharges [mm/d]}

\item{Seed}{(optional) [numeric] seed of random number generator}

}

\value{
[list] runModel_DA function provides a list containing the outputs organised as follows:

  \tabular{ll}{                                                                                         
    \emph{QsimEns  } \tab [matrix, dim(NbMbr, Nt)] ensemble discharge simulations (with Nt the length of IndRun)
            \cr
            \cr
    \emph{EnsStateBkg  } \tab [array, dim(Nstate, NbMbr, Nt)] (with Nstate the number of model states) ensemble values of background model states (before the filter update)                      
            \cr
            \cr
    \emph{EnsStateA  } \tab [array, dim(Nstate, NbMbr, Nt)] ensemble values of analysis model states (after the filter update)                               
            \cr
         }                                                                            
}

\description{
Function which performs discharge ensemble simulations with the assimilation of observed discharges through the Ensemble Kalman filter (EnKF) or the Particle filter (PF) schemes.
}

\details{
Discharge observations are sequentially assimilated at each time step (if \emph{DaMethod} != 'none' ) via EnKF or PF scheme. Because of the recursive approach, the analysis states resulting from the assimilation procedure at time step \emph{t} are used as initial states at the following prediction time step \emph{t + 1}.  
\cr
When accounting for the uncertainty in model inputs (see \emph{InputsPert}), the ensemble discharge simulations are driven by perfect meteorological forecasts, which are generated according to the methodology proposed by Clark et al. (2008) (see \code{\link{CreateInputsPerturb}} for details).
\cr
While all model states are jointly updated via PF, it is possible to enable/disable the update of specific state variables via EnKF, by defining their names in \emph{IndState}.
\cr
Both the PF and EnKF can account for uncertainty in model states. If \emph{IsState} = 'TRUE', the state variables specified in \emph{IndState} are perturbed after Salamon and Feyen (2009). If the state uncertainty is taken into account, the initial states at the prediction time step \emph{t + 1} are the perturbed analysis states resulting from the assimilation and perturbation procedures at time step \emph{t}.
\cr
In order to ensure reproducible results, \emph{Seed} can be set to fix the randomness in the generation of perturbations. 
\cr
For further details and guidelines on the choice of the DA technique, see the references section.
\cr
Nota: The function can be applied when using GR4J, GR5J and GR6J models (i.e. daily model time step), with or withouth the CemaNeige module (see \emph{airGR} package).
}

\examples{
library(airGRdatassim)

## loading catchment data
data(L0123001, package = "airGR")
Param <- c(X1 = 194.243, X2 = -0.088, X3 = 117.740, X4 = 1.680, X5 = 0.000)
         
## run period selection
IndRun <- seq(which(format(BasinObs$DatesR, format = "\%Y-\%m-\%d")=="2006-01-01"),
              which(format(BasinObs$DatesR, format = "\%Y-\%m-\%d")=="2006-01-31"))
               
## preparation of the InputsModel object
InputsModel <- CreateInputsModel(FUN_MOD = RunModel_GR5J, DatesR = BasinObs$DatesR, 
                                 Precip = BasinObs$P, PotEvap = BasinObs$E)
                         

## preparation of perturbed meteorological ensemble            
InputsPert <- CreateInputsPerturb(FUN_MOD = RunModel_GR5J, DatesR = BasinObs$DatesR,
                                  Precip = BasinObs$P, PotEvap = BasinObs$E, 
                                  NbMbr = 100)               

## simulation with DA via EnKF
result <- RunModel_DA(DaMethod = "EnKF", IndRun = IndRun, IndState = c("Prod", "Rout"), NbMbr = 100,
                      FUN_MOD = RunModel_GR5J,
                      InputsModel = InputsModel, InputsPert = InputsPert, Param = Param,
                      IsState = TRUE, Qobs = BasinObs$Qmm)

## results preview
rangeQsimEns <- apply(result$QsimEns, MARGIN = 2, FUN = range)
plot(x = InputsPert$DatesR[IndRun], y = colMeans(result$QsimEns),
     ylim = range(rangeQsimEns),
     type = "l", col = "red", lwd = 2,
     main = "EnKF-based discharge simulations",
     xlab = "Time [day]", ylab = "Discharge [mm/day]",
     panel.first = polygon(x = c(as.numeric(InputsPert$DatesR[IndRun]),
                                 rev(as.numeric(InputsPert$DatesR[IndRun]))),
                           y = c(rangeQsimEns[1, ], rev(rangeQsimEns[2, ])),
                           col = "pale turquoise", border = NA),
     panel.last = lines(x = InputsPert$DatesR[IndRun],
                        y = BasinObs$Qmm[IndRun],
                        type = "l", col = "black", lwd = 2))
legend("topright", legend = c("DA-based", "obs"), lty = 1, col = c("red", "black"))
}

\author{
Gaia Piazzi, Olivier Delaigue
}

\references{
Clark, M. P., Rupp, D. E., Woods, R. A., Zheng, X., Ibbitt, R. P., Slater, A. G. et al. (2008). Hydrological data assimilation with the ensemble Kalman filter: Use of streamflow observations to update states in a distributed hydrological model. Advances in Water Resources, 31(10), 1309-1324.
\cr
Piazzi, G., Thirel, G.,  Perrin, C. and Delaigue, O. (2020). Sequential data assimilation for streamflow forecasting: assessing the sensitivity to uncertainties and updated variables of a conceptual hydrological model. Water Resources Research. (submitted).
\cr  
Salamon, P., and Feyen, L. (2009). Assessing parameter, precipitation, and predictive uncertainty in a distributed hydrological model using sequential data assimilation with the particle filter. Journal of Hydrology, 376(3-4), 428-442.
}


\seealso{
\code{\link{CreateInputsPerturb}}
}
