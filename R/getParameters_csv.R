#' Export EM parameters for [TMB::MakeADFun()] based on csv files with parameters
#'
#' @param trueparms Logical. TRUE if random parameter initialization
#'
#' @return A list of parameters
#' @export
getParameters_csv <- function(trueparms = TRUE){

  if (trueparms == TRUE){
    initN <- read.csv(system.file("extdata/Ninit_MLE.csv",
                                  package = "pacifichakemse",
                                  mustWork = TRUE))[,1]
    Rdev <- read.csv(system.file("extdata/Rdev_MLE.csv",
                                 package = "pacifichakemse",
                                 mustWork = TRUE))[,1]
    PSEL <- as.matrix(read.csv(system.file("extdata/p_MLE.csv",
                                           package = "pacifichakemse",
                                           mustWork = TRUE)))
    F0 <- assessment$F0

    # assessment <- read.csv('data/assessment_MLE.csv')
    # assessment <- assessment[assessment$year > 1965 &assessment$year < 2018 ,]

    parms <- list( # Just start all the simulations with the same initial conditions
      logRinit = 14.5614,
      logh = log(0.861909),
      logMinit = log(0.213686),
      logSDsurv = log(0.257246),
      #logSDR = log(1.4),
      logphi_catch = log(0.8276),
      #logphi_survey = log(11.33),
      # logSDF = log(0.1),
      # Selectivity parameters
      psel_fish = c(2.486490, 0.928255,0.392144,0.214365,0.475473),
      psel_surv = c(0.568618,-0.216172,0.305286 ,0.373829),
      initN = rev(initN),
      Rin = Rdev[1:(length(Rdev)-1)],
      PSEL = PSEL,
      F0 = F0)
  }else{

    PSEL <- matrix(0,5, length(1991:years[length(years)]))
    initN <- rep(0,df$nage-1)
    F0 <- rep(0.01, df$tEnd)
    Rdev <- rep(0, df$tEnd-1)
    #Rdev <- read.csv('Rdev_MLE.csv')[,1]
    # Just start all the simluations with the same initial conditions
    parms <- list(logRinit = 16,
                  logh = log(0.7),
                  logMinit = log(0.3),
                  logSDsurv = log(0.3),
                  # logSDR = log(1.4),
                  logphi_catch = log(0.8276),
                  # logphi_survey = log(11.33),
                  # logSDF = log(0.1),
                  # Selectivity parameters
                  psel_fish = c(2.486490, 0.928255,0.392144,0.214365,0.475473),
                  psel_surv = c(0.568618,-0.216172,0.305286 ,0.373829),
                  initN = initN,
                  Rin = Rdev,
                  F0 = F0,
                  PSEL = PSEL)
  }
  parms
}
