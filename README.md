
# airGRdatassim: Suite of tools to perform ensemble-based discharge assimilation in GR Hydrological Models

## Overview

airGRdatassim is a package based on the airGR hydrological modeling package. It provides the tools to assimilate observed discharges in the **GR5J** hydrological model. The package is developed at INRAE-Antony ([Catchment Hydrology research group](https://webgr.inrae.fr/en/) of the HYCAR Research Unit, France). 


## Installation

To download the version of the airGRdatassim package that is on GitLab, you have first install the [Git software](https://git-scm.com/downloads). Then you can install the package in the R environment, using the following command lines:

``` r
Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS="true")
install.packages("remotes")
remotes::install_git(url = "https://gitlab.irstea.fr/HYCAR-Hydro/airgrdatassim")
```


## Functions and objects

The airGRdatassim package allows users of GR Hydrological models to assimilate discharge observations with the aim of improving streamflow simulations.
The data assimilation (DA) scheme has been designed to allow the choice between two sequential ensemble-based DA techniques, namely the Ensemble Kalman filter (EnKF) and the Particle filter (PF).
The functions are coded in R and both their names and arguments are consistent with the airGR package.

With the aim of providing an user-friendly package, airGRdatassim relies on two main functions :

- `CreateInputsPerturb()` generates the probabilistic model inputs to perform the ensemble-based DA when accounting for the uncertainty in meteorological forcings. The function requires three arguments : `FUN_MOD`, `DatesR`, `NbMbr`. Optional arguments are `Seed` and the meteorological variable/s to be perturbed, namely `Precip` and/or `PotEvap`. Please refer to the help page `CreateInputsPerturb()` for further details and examples;

- `RunModel_DA()` performs streamflow ensemble simulations with the assimilation of observed discharges through the EnKF or the PF scheme. The function requires eight arguments: `DaMethod`, `IndRun`, `IndState`, `NbMbr`, `FUN_MOD`, `InputsModel`, `Param` and `IsState`. Optional arguments are: `InputsPert` (if the uncertainty in meteorological forcings is taken into account), `Qobs` and `Seed`. Please refer to the help page `RunModel_DA()` for further details and examples. 

Consistently with the airGR package, both structure and class of function arguments are specifically defined to prevent the risk of mis-use and ensure the flexibility of functions. Advanced users wishing to apply the package to their own models will need to comply with these imposed structures and refer to the package source codes to get all the specification requirements.


## Hydrological model

DA schemes are designed to be coupled with **GR5J** hydrological model, which is implemented in the airGR package. This model can be called within the airGRdatassim package using the following function: 

  - `RunModel_GR5J()`: five-parameter daily lumped hydrological model (Le Moine, 2008)


## How to get started

Because airGRdatassim is an airGR-based package, specific airGR functions should be jointly used to ensure the proper use of the airGRdatassim tools. Indeed, before performing the DA-based streamflow simulations, the hydrological model needs to be calibrated through the airGR calibration function. Therefore, the following steps are recommended:    

  1. refer to the help for `Calibration_Michel()` in the airGR package, run the provided example and then refer to the help for `CreateCalibOptions()` to understand how a model calibration is prepared/made.
  2. refer to the help for `CreateInputsPerturb()` to understand how the probabilistic model inputs are generated, if the uncertainty in meteorological forcings is taken into account;
  3. refer to the help for `RunModel_DA()` to understand how to perform the DA-based streamflow simulations
  4. refer to the help for `ErrorCrit_NSE()` and `CreateInputsCrit()` in the airGR package to understand how the computation of an error criterion is prepared/made.


For more information and to get started with the package, you can refer to the vignette (`vignette("get_started")`).


## References

- Clark, M. P., Rupp, D. E., Woods, R. A., Zheng, X., Ibbitt, R. P., Slater, A. G. et al. (2008). Hydrological data assimilation with the ensemble Kalman filter: Use of streamflow observations to update states in a distributed hydrological model. Advances in Water Resources, 31(10), 1309-1324. 
- Le Moine, N. (2008). Le bassin versant de surface vu par le souterrain : une voie d'amélioration des performances et du réalisme des modèles pluie-débit ?, PhD thesis (in French), UPMC - Cemagref Antony, Paris, France, 324 pp.
- Piazzi, G., Thirel, G., Perrin, C. and Delaigue, O. (2020). Sequential data assimilation for streamflow forecasting: assessing the sensitivity to uncertainties and updated variables of a conceptual hydrological model. Water Resources Research.(submitted). 
- Salamon, P., and Feyen, L. (2009). Assessing parameter, precipitation, and predictive uncertainty in a distributed hydrological model using sequential data assimilation with the particle filter. Journal of Hydrology, 376(3-4), 428-442.
