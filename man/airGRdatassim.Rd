\name{airGRdatassim}
\alias{airGRdatassim}
\docType{package}
\encoding{UTF-8}
\title{Suite of tools to perform ensemble-based discharge assimilation with GR Hydrological Models}
\description{
airGRdatassim is a package based on the airGR hydrological modelling package. It provides the tools to assimilate observed discharges in the GR daily hydrological models (GR4J, GR5J and GR6J, with and without the CemaNeige snow model). The package is developed at INRAE-Antony (\href{https://webgr.inrae.fr/en/}{Catchment Hydrology research group} of the HYCAR Research Unit, France). More information about the efficiency of these data assimilation schemes with GR4J can be found in Piazzi et al. (in review). \cr\cr


## --- Functions and objects

The airGRdatassim package allows users of GR hydrological models to assimilate discharge observations with the aim of improving streamflow simulations.
The package has been designed to allow the choice between two sequential ensemble-based data assimilation (DA) techniques, namely the Ensemble Kalman filter (EnKF) and the Particle filter (PF). \cr
The functions are coded in R and both their names and arguments are consistent with the airGR package. \cr

With the aim of providing a user-friendly package, airGRdatassim relies on two main functions: \cr
- \code{\link{CreateInputsPert}} generates the probabilistic model inputs to perform ensemble-based DA when accounting for the uncertainty in meteorological forcings; \cr
- \code{\link{RunModel_DA}} performs streamflow ensemble simulations with the assimilation of observed discharges through the EnKF or the PF schemes. \cr \cr

Consistently with the airGR package, both structure and class of function arguments are specifically defined to prevent from mis-use and to ensure the flexibility of functions. Advanced users wishing to apply the package to their own models will need to comply with these imposed structures and refer to the package source codes to get all the specification requirements. \cr\cr


## --- Models

DA schemes are designed to be coupled with GR daily hydrological models, which are implemented in the airGR package. These models can be called within the airGRdatassim package using the following airGR functions: \cr 
  - \code{\link[airGR]{RunModel_GR4J}}: four-parameter daily lumped hydrological model (Perrin et al., 2003) \cr
  - \code{\link[airGR]{RunModel_GR5J}}: five-parameter daily lumped hydrological model (Le Moine, 2008) \cr
  - \code{\link[airGR]{RunModel_GR6J}}: six-parameter daily lumped hydrological model (Pushpalatha et al., 2011) \cr
  - \code{\link[airGR]{RunModel_CemaNeigeGR4J}}: combined use of GR4J and CemaNeige \cr
  - \code{\link[airGR]{RunModel_CemaNeigeGR5J}}: combined use of GR5J and CemaNeige \cr
  - \code{\link[airGR]{RunModel_CemaNeigeGR6J}}: combined use of GR6J and CemaNeige \cr\cr


## --- How to get started

Because airGRdatassim is an airGR-based package, specific airGR functions should be jointly used to ensure the proper use of the airGRdatassim tools. Indeed, before performing the DA-based streamflow simulations, the hydrological model needs to be calibrated through the airGR calibration function. Therefore, the following steps are recommended: \cr  

  1. refer to the help for \code{\link[airGR]{Calibration_Michel}} in the airGR package, run the provided example and then refer to the help for \code{\link{CreateCalibOptions}} to understand how a model calibration is prepared/made; \cr
  2. refer to the help for \code{\link{CreateInputsPert}} to understand how the probabilistic model inputs are generated, if the uncertainty in meteorological forcings is taken into account; \cr
  3. refer to the help for \code{\link{RunModel_DA}} to understand how to perform the DA-based streamflow simulations; \cr
  4. refer to the help for \code{\link[airGR]{ErrorCrit_NSE}} and \code{\link[airGR]{CreateInputsCrit}} in the airGR package to understand how the computation of an error criterion is prepared/made. \cr

For more information and to get started with the package, you can refer to the vignette: \cr
\code{vignette("get_started", package = "airGRdatassim")}. \cr\cr


## --- References

- Clark, M. P., Rupp, D. E., Woods, R. A., Zheng, X., Ibbitt, R. P., Slater, A. G. et al. (2008). Hydrological data assimilation with the ensemble Kalman filter: Use of streamflow observations to update states in a distributed hydrological model. Advances in Water Resources, 31(10), 1309-1324, doi: \href{https://www.doi.org/10.1016/j.advwatres.2008.06.005}{10.1016/j.advwatres.2008.06.005} \cr
- Le Moine, N. (2008). Le bassin versant de surface vu par le souterrain : une voie d'amélioration des performances et du réalisme des modèles pluie-débit ?, PhD thesis (in French), UPMC - Cemagref Antony, Paris, France, 324 pp. \cr
- Perrin, C., Michel, C. and Andréassian, V. (2003). Improvement of a parsimonious model for streamflow simulation, Journal of Hydrology, 279(1-4), 275-289, doi: \href{https://www.doi.org/10.1016/S0022-1694(03)00225-7}{10.1016/S0022-1694(03)00225-7}
- Piazzi, G., Thirel, G., Perrin, C. and Delaigue, O. (in review). Sequential data assimilation for streamflow forecasting: assessing the sensitivity to uncertainties and updated variables of a conceptual hydrological model. Water Resources Research. \cr
- Pushpalatha, R., Perrin, C., Le Moine, N., Mathevet, T. and Andréassian, V. (2011). A downward structural sensitivity analysis of hydrological models to improve low-flow simulation, Journal of Hydrology, 411(1-2), 66-76, doi: \href{https://www.doi.org/10.1016/j.jhydrol.2011.09.034}{10.1016/j.jhydrol.2011.09.034} \cr
- Salamon, P. and Feyen, L. (2009). Assessing parameter, precipitation, and predictive uncertainty in a distributed hydrological model using sequential data assimilation with the particle filter. Journal of Hydrology, 376(3-4), 428-442, doi: \href{https://www.doi.org/10.1016/j.jhydrol.2009.07.051}{10.1016/j.jhydrol.2009.07.051}

}



\concept{hydrology}
\concept{model}
\concept{data assimilation}
\concept{ensemble}
\concept{GR4J}
