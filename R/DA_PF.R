
DA_PF <- function(Obs, Qsim, States, IsState, IndState, Param, VarThr, NbMbr, StateNames) {
  
  # ------ Checks
  
  IndState <- match.arg(IndState, choices = StateNames, several.ok = TRUE)
  IndState <- as.numeric(StateNames %in% IndState)
  
  
  # ------ Settings
  
  NbState <- length(StateNames)
  
  # member names
  MbrNames <- sprintf("Mbr_%s", seq_len(NbMbr))
  
  Weights <- rep(0, times = NbMbr)
  names(Weights) <- MbrNames
  
  Indices <- rep(NA, times = NbMbr)
  
  CurrentState     <- list()
  CurrentStatePert <- list() 
  
  #***************************************************************************************************************
  # PARTICLES WEIGHTING 
  
  VarObs <- max(VarThr^2, (0.1*Obs)^2)
  
  # evaluation of innovations
  Innov <- Obs - Qsim
  names(Innov) <- MbrNames
  
  # evaluation of weights
  Weights <- dnorm(Innov, mean = 0, sd = sqrt(VarObs))
  
  # normalisation of weights 
  Weights <- Weights/sum(Weights)
  
  # if the ensemble is squeezed, NA-valued weights can be generated --> all particles are assigned equal weights
  if (!all(is.finite(Weights))) {
    Weights <- rep(1/NbMbr, times = NbMbr)
  } 
  
  #***************************************************************************************************************
  # PARTICLES RESAMPLING 
  
  # evaluation of the cumulative density function of weights
  CdfW <- cumsum(Weights)
  
  A <- CdfW[1]
  B <- min(1, A + (1/(NbMbr+1)))
  
  Urand0    <- A + ((B - A)*(runif(1)))
  StepUrand <- (1-Urand0)/NbMbr
  
  Urand <- seq(from = Urand0, length.out = NbMbr, by = StepUrand)
  Indices <- findInterval(Urand, vec = CdfW, rightmost.closed = TRUE) + 1 # indices of particles to be resampled
  
  # if the ensemble is squeezed, almost equivalent and low weight values 
  # --> indices of most likely particles can not be identified 
  # --> all the particles are assigned equal weights and the resampling is re-evaluated according to the new weights
  
  if (!all(is.finite(Indices))) { 
    Weights <- rep(1/NbMbr, times = NbMbr)
    CdfW    <- cumsum(Weights)
    
    A <- CdfW[1]
    B <- min(1, A + (1/(NbMbr+1)))
    
    Urand0    <- A + ((B - A)*(runif(1)))
    StepUrand <- (1-Urand0)/NbMbr
    
    Urand <- seq(from = Urand0, length.out = NbMbr, by = StepUrand)
    Indices <- findInterval(Urand, vec = CdfW, rightmost.closed = TRUE) + 1 # indices of particles to be resampled
  }
  
  Repeats <- as.data.frame(table(Indices)) # it indicates how many time each selected particle must be replicated 
  
  #***************************************************************************************************************
  # STATE PERTURBATION
  
  if (IsState) {
    EnsState <- matrix(data = NA, nrow = NbState, ncol = NbMbr,
                       dimnames = list(StateNames,
                                       MbrNames))
    
    EnsState["Prod", ] <- sapply(seq_along(States), function(x) States[[x]]$Store$Prod)
    EnsState["Rout", ] <- sapply(seq_along(States), function(x) States[[x]]$Store$Rout)
    EnsState["UH2"  , ] <- sapply(seq_along(States), function(x) States[[x]]$UH$UH2[1])
    if ("UH1" %in% StateNames){
      EnsState["UH1"  , ] <- sapply(seq_along(States), function(x) States[[x]]$UH$UH1[1])  
    }
   
    MuState <- rep(0, times = NbState)
    names(MuState) <- StateNames
    
    # evaluation of the variance of state variables
    SdState <- pmin(3, 
                     pmax(1.2, 
                          apply(EnsState[, as.numeric(as.character(Repeats$Indices)), drop = FALSE], MARGIN = 1, sd, na.rm = TRUE),
                          na.rm = TRUE))
    names(SdState) <- StateNames
  }   # END IF(IsState)
  
  #***************************************************************************************************************
  # RESAMPLING  
  
  for (iPart in seq_len(nrow(Repeats))) {
    
    IndexParticle <- as.numeric(as.character(Repeats$Indices[iPart]))
    RepParticle   <- as.numeric(as.character(Repeats$Freq[iPart]))
    
    # the particle identified by the 'IndexParticle' is replicated 'RepParticle' times 
    TempState        <- rep(States[IndexParticle], times = RepParticle)
    names(TempState) <- sprintf("Rep%s_Part%s", seq_len(RepParticle), IndexParticle)
    
    if (IsState) { # state perturbation
      
      TempStatePert <- TempState  # if the selected particle is NOT replicated --> it is not perturbed
      
      if (RepParticle > 1) {      # if the selected particle is replicated --> its replications are perturbed
        
        StateRep <- matrix(data = NA, nrow = NbState, ncol = RepParticle,
                           dimnames = list(StateNames,
                                           sprintf("Rep_%s", seq_len(RepParticle))))
        
        StateRep["Prod", ] <- rep(States[[IndexParticle]]$Store$Prod, RepParticle)
        StateRep["Rout", ] <- rep(States[[IndexParticle]]$Store$Rout, RepParticle)
        StateRep["UH2", ]   <- rep(States[[IndexParticle]]$UH$UH2[1],  RepParticle)
        if ("UH1" %in% StateNames){
          StateRep["UH1", ]   <- rep(States[[IndexParticle]]$UH$UH1[1],  RepParticle)  
        }
        
        # noise generation
        NoiseState <- matrix(data = rnorm(RepParticle*NbState, mean = MuState, sd = SdState),
                             nrow = NbState, ncol = RepParticle, byrow = FALSE,
                             dimnames = list(StateNames,
                                             sprintf("Rep_%s", seq_len(RepParticle))))
        
        NoiseState[IndState == 0, ] <- 0      # null noise for the i-th variable if its uncertainty is not considered
        StateRepPert <- StateRep + NoiseState
        
        # perturbation constraints
        StateRepPert["Prod", StateRepPert["Prod", ] > Param[1]] <- Param[1]
        StateRepPert["Rout", StateRepPert["Rout", ] > Param[3]] <- Param[3]
        for (iRep in seq_len(RepParticle)) {
          TempStatePert[[iRep]]$Store$Prod <- StateRepPert["Prod", iRep]
          TempStatePert[[iRep]]$Store$Rout <- StateRepPert["Rout", iRep]
          TempStatePert[[iRep]]$UH$UH2[1]  <- StateRepPert["UH2",   iRep]
          if ("UH1" %in% StateNames){
            TempStatePert[[iRep]]$UH$UH1[1]  <- StateRepPert["UH1",   iRep]  
          }
        }
      } # END IF RepParticle 
      
      CurrentStatePert <- c(CurrentStatePert, TempStatePert)
      
    } # END IF IsState
    CurrentState <- c(CurrentState, TempState)
    
    rm(TempState, TempStatePert)
    
  } # END FOR repeats
  
  #***************************************************************************************************************
  
  EnsStatePf <- CurrentState
  
  if (IsState) {
    EnsStatePert <- CurrentStatePert
  }
  
  #***************************************************************************************************************
  # OUTPUT
  
  ans <- list(EnsStatePf = EnsStatePf)
  if (IsState) {
    ans$EnsStatePert <- EnsStatePert
  }
  return(ans)
}
