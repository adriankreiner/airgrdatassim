
CreateInputsPerturb <- function(FUN_MOD, DatesR, Precip = NULL, PotEvap = NULL, NbMbr, Seed = NULL) {
    
  # ------ Checks
  
  FUN_MOD <- match.fun(FUN_MOD)
  if (!identical(FUN_MOD, RunModel_GR5J)) {
    stop("incorrect 'FUN_MOD', only 'RunModel_GR5J' can be used")
  }
  
  if (!(is.atomic(NbMbr) && is.numeric(NbMbr) && length(NbMbr) == 1 && NbMbr >= 2)) {
    stop("'NbMbr' should be a single vector of a numeric value >= 2")
  }
  
  Seed0 <- as.numeric(Seed)
  
  
  #***************************************************************************************************************
  
  # Settings
  
  # variables to perturbate
  MeteoNames <- c("Precip", "PotEvap")
  
  # length of the input vectors
  Nt <- length(DatesR)
  
  # model time step [day]
  Dt <- 1
  
  # temporal decorrelation length for Precip and PotEvap [day]
  Tao  <- c(Precip = 1, PotEvap = 2)                                  
  Alfa <- 1 - (Dt/Tao)
  
  # fractional error parameter for Precip and PotEvap
  Eps  <- c(Precip = 0.65, PotEvap = 0.65)                            
  
  # complementary error function
  Erfc <- function(x) {
    2 * pnorm(q = x * sqrt(2), lower.tail = FALSE)
  }
  
  # check variables to perturbate
  if (is.null(Precip) & is.null(PotEvap)) {
    stop("'Precip' and 'PotEvap' are both missing. You must provide at least one of them")
  }
  skipVarMeteo <- NULL
  if (is.null(Precip)) {
    skipVarMeteo <- c(skipVarMeteo, "Precip")
    Precip <- rep(0, times = Nt)
  }
  if (is.null(PotEvap)) {
    skipVarMeteo <- c(skipVarMeteo, "PotEvap")
    PotEvap <- rep(0, times = Nt)
  }
  
  # select MeteoNames and Eps in function of the provided variables
  MeteoNames <- MeteoNames[!MeteoNames %in% skipVarMeteo]
  Eps <- Eps[MeteoNames]
  Alfa <- Alfa[MeteoNames]
  
  # number of variable to perturbate
  NbMeteo <- length(MeteoNames)
  
  #***************************************************************************************************************
  # Generation of the InputsModel object
  
  InputsPert <- airGR::CreateInputsModel(FUN_MOD = FUN_MOD, DatesR = DatesR,
                                         Precip = Precip, PotEvap = PotEvap)
  

  
  #***************************************************************************************************************
  # Initialisation of the parameters of the first-order autoregressive model 
  
  # member names
  MbrNames <- sprintf("Mbr_%s", seq_len(NbMbr))
  
  # time names
  TimeNames <- sprintf("Time_%s", seq_len(Nt))
  
  
  # error time evolution
  S <- array(data = rep(NaN, times = NbMeteo*NbMbr*Nt), #olivier : is it necessary to initialize s, u, fi?
             dim = c(NbMeteo, NbMbr, Nt),
             dimnames = list(MeteoNames, MbrNames, TimeNames))
  # uniform random numbers
  U <- array(data = rep(NaN, times = NbMeteo*NbMbr*Nt),
             dim = c(NbMeteo, NbMbr, Nt),
             dimnames = list(MeteoNames, MbrNames, TimeNames))
  
  # multiplicative perturbations of model inputs
  Fi <- array(data = rep(NaN, times = NbMeteo*NbMbr*Nt),
              dim = c(NbMeteo, NbMbr, Nt),
              dimnames = list(MeteoNames, MbrNames, TimeNames))
  
  
  #***************************************************************************************************************
  # Initialisation of meteorological ensembles
  
  MeteoEns <- sapply(MeteoNames, function(iMeteo) {
    matrix(data = rep(InputsPert[[iMeteo]], each = NbMbr), 
           nrow = Nt, byrow = TRUE,
           dimnames = list(TimeNames, MbrNames))
  }, simplify = "array")
  
  # same dimensions as Fi
  MeteoEns <- aperm(MeteoEns, perm = 3:1)
  
  
  #***************************************************************************************************************
  # Perturbation of the time series of model forcings (i.e., precipitation and potential evapotranspiration)
  
  for (iTime in seq_len(Nt)) { # olivier : can we do apply here?
    
    # white noise
    if (!is.null(Seed)) {
      Seed <- Seed0 + iTime
      set.seed(seed = Seed)
      on.exit(set.seed(seed = NULL))
    }
    
    W <- matrix(data = rnorm(NbMeteo*NbMbr, mean = 0, sd = 1), byrow = TRUE,
                nrow = NbMeteo, ncol = NbMbr,
                dimnames = list(MeteoNames, MbrNames))  
    
    if (iTime == 1) {
      # error initialisation
      S0 <- matrix(data = runif(NbMeteo*NbMbr, min = 0, max = 1), byrow = TRUE,
                   nrow = NbMeteo, ncol = NbMbr,
                   dimnames = list(MeteoNames, MbrNames)) 
      
      S[, , iTime] <- Alfa*S0+sqrt(1-Alfa^2) * W    
      
    } else {
      # error time evolution
      S[, , iTime] <- Alfa * S[, , iTime-1] + sqrt(1-Alfa^2) * W               
    }
    
    # generation of uniform random numbers
    U[, , iTime] <- 0.5 * Erfc(S[, , iTime] / sqrt(2))   
    
    # generation of the multiplicative perturbations of model inputs
    Fi[, , iTime] <- (1-Eps) + (2 * U[, , iTime] * Eps)       

    MeteoEns[, , iTime] <- MeteoEns[, , iTime]  * Fi[, , iTime]
    
    
  } # FOR time
  
  # split MeteoEns array into a list of matrix
  MeteoEns <- asplit(MeteoEns, MARGIN = 1)
  
  # update InputsPert
  for (iName in c(names(MeteoEns), skipVarMeteo)) {
    if (iName %in% MeteoNames) {
      InputsPert[[iName]] <- t(MeteoEns[[iName]])
    } else {
      InputsPert[[iName]] <- NULL
    }
  }
  
  # class
  class(InputsPert) <- c(class(InputsPert), "InputsPert")
  
  return(InputsPert)
}
