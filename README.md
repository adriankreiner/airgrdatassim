# airGRdatassim: Modified airGRdatassim Package for Operational Use with Glacier Module

This package is a modification of the original [airGRdatassim](https://cran.r-project.org/web/packages/airGRdatassim/index.html) version 0.1.3 package available on CRAN. Both the original and this modified package are built on the `airGR` hydrological modeling framework. These packages provide tools for assimilating observed discharges into the GR daily hydrological models (GR4J, GR5J, and GR6J, with and without the CemaNeige snow model). The original package was developed by the Catchment Hydrology research group at INRAE-Antony, part of the HYCAR Research Unit, France. For more information on the effectiveness of these data assimilation schemes with GR5J, refer to Piazzi et al. (2021).

## Modifications in This Version:

1.  **Glacier Module Integration**: Added Glacier modules for the GR4J-CemaNeige and GR6J-CemaNeige models, allowing them to function with data assimilation techniques.
2.  **Operational Enhancements**: Enabled the model to start with specified initial conditions, a feature not available in the original version, improving its utility in operational settings.
3.  **Forcing Perturbation Customization**: Introduced the ability to specify basin-specific temperature and precipitation lapse rates within the forcing perturbation process, providing more tailored simulations.
4.  **Adjustable Forcing Perturbation Spread**: Added functionality to adjust the spread of the forcing perturbation, offering greater control over the uncertainty in input data.

## Installation

### Prerequisites

Before installing airGR_GM, ensure you have the following prerequisites installed on your system:

-   **R**: Version 4.1.1 or higher.

-   [RTools](https://cran.r-project.org/bin/windows/Rtools/) (Windows only): Required for building packages from source.

-   **airGR**: The customised `airGR` package must be preinstalled. Please refer to [airGR_GM](https://github.com/hydrosolutions/airGR_GM)
