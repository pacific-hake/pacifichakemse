#' Load the hake data for seasons (TODO: Improve docs on this function)
#'
#' @param nseason number of seasons
#' @param nspace number of spatial areas
#' @param syear first year of historical simulations
#' @param myear last year of historical simulations
#' @param ages A vector of ages
#' @param sel_change_yr A year in which a selectivity change took place
#' @param movemaxinit max movement rate
#' @param movefiftyinit age at 50percent max movement rate
#' @param nsurvey survey frequency
#' @param logSDR recruitment deviations
#' @param bfuture bias adjustment in the future
#' @param moveout fraction of individuals that travel south in the last season
#' @param movesouth fraction of individuals that move south during the year
#' @param moveinit Initial distribution of fish
#' @param moveslope Slope of the movement function
#' @param selectivity_change should selectivity change?
#' @param yr_future how many years into the future should there be stochastic values
#' @param sel_hist use historical selectivity?
#'
#' @return A list of Parameters, Input parameters, Survey, Catch, and others
#' @export
#'
#' @examples
#' \dontrun{
#' df <- load_data_seasons(nseason = 12, nspace = 2)
#' }
load_data_seasons <- function(nseason = 4,
                              nspace = 2,
                              syear = 1966,
                              myear = 2018,
                              ages = 0:20,
                              sel_change_yr = 1991,
                              movemaxinit = 0.35,
                              movefiftyinit = 6,
                              nsurvey = 2,
                              logSDR = 1.4,
                              bfuture = 0.5,
                              moveout = 0.85,
                              movesouth = 0.05,
                              moveinit = NA,
                              moveslope = 0.9,
                              selectivity_change = 0,
                              yr_future  = 0,
                              sel_hist = TRUE){

  csv_data <- csv_data(sel_hist)
  browser()
  if(is.na(moveinit)){
    if(nspace == 2){
      moveinit <-  c(0.25, 0.75)
    }
  }
  years <- syear:(myear + yr_future)
  nyear <- length(years)
  tEnd <- length(years) * nseason

  # Age stuff
  nage <- length(ages)
  msel <- rep(1,nage)

  # TODO: for later use
  # 4 is seasonality
  recruitmat <- matrix(0, nspace)
  # 10 percent change of spawning north
  recruitmat[1] <- 1
  # 90 percent of spawning south
  recruitmat[2] <- 1

  # Maturity
  movefifty <- movefiftyinit
  movemax <- rep(movemaxinit, nseason)
  # Chances of moving in to the other grid cell
  movemat <- array(0, dim = c(nspace, nage, nseason, nyear))
  move <- ifelse(nspace == 1, FALSE, TRUE)
  if(move){
    for(j in 1:nspace){
      for(i in 1:nseason){
        movemat[j,,i,] <- movemax[i] / (1 + exp(-moveslope * (ages - movefifty)))
      }
    }
    # Recruits and 1 year olds don't move
    movemat[,1:2,,] <- 0
    # For the standard model
    if(nseason == 4){
      # Don't move south during the year
      movemat[1, 3:nage, 2:3,] <- movesouth
      # continuing south movement at spawning time
      movemat[1, 3:nage, 1,] <- movesouth
      movemat[1, 3:nage, nseason,] <- moveout
      movemat[2, 3:nage, nseason,] <- movesouth
    }
    move.init <- moveinit
  }else{
    move.init <- 1
  }
  # weight at age
  wage_ss <- wage_ss[wage_ss$Yr %in% years,]
  wage_ssb <- wage_ss[wage_ss$Fleet == -2,paste('X',ages, sep = '')]
  wage_ssb[[1]] <- unname(wage_ssb[[1]])
  wage_catch <- wage_ss[wage_ss$Fleet == 1, paste('X', ages, sep = '')]
  wage_survey <- wage_ss[wage_ss$Fleet == 2, paste('X', ages, sep = '')]
  wage_mid <- wage_ss[wage_ss$Fleet == -1, paste('X', ages, sep = '')]
  # Maturity
  mat <- wage_ssb[1,]
  # Age comps
  age_survey.df$flag <- 1
  age_catch.df$flag <- 1
  if(nseason == 1){
    surveyseason <-  1
  }else if(nseason == 4){
    surveyseason <- 3
  }else{
    surveyseason <- floor(nseason/2)
  }
  if(!sel_hist){
    PSEL <- matrix(0, 5, 28)
  }

  if(nseason == 4 & nspace == 2){
    Fnseason <- matrix(NA, 2, 4)
    #Fnseason[1,] <- c(0.0,0.4,0.50,0.1) # Must add to one
    Fnseason[1,] <- c(0.001, 0.188, 0.603, 0.208)
    #Fnseason[2,] <- c(0.0,0.4,0.50,0.1) # Must add to onec
    Fnseason[2,] <- c(0.000, 0.317, 0.382, 0.302) / sum(c(0.000, 0.317, 0.382, 0.302)) # Divide by sum to sum to 1
  }else{
    Fnseason <- matrix(NA, nspace, nseason)
    Fnseason[1:nspace,] <- 1/nseason # Equally distributed catch
  }
  rmul <- ifelse(nspace == 2, 1.1, 1)
  # Just start all the simluations with the same initial conditions
  parms <- list(logRinit = parms.scalar$logRinit + log(rmul),
                logh = parms.scalar$logh,
                logMinit = parms.scalar$logMinit,
                logSDsurv = parms.scalar$logSDsurv,
                #logSDR = log(1.4),
                logphi_catch = parms.scalar$logphi_catch,
                #logphi_survey = log(11.33),
                # logSDF = log(0.1),
                # Selectivity parameters
                psel_fish = parms.sel$value[parms.sel$source == 'fish'],
                psel_surv = parms.sel$value[parms.sel$source == 'survey'],
                initN = initN,
                Rin = Rdev,
                PSEL = PSEL)

  psel<- matrix(NA,nspace, 5)

  for(i in 1:nspace){
    #psel[i,] <- c(2.8476, 0.973,0.3861,0.1775,0.5048) # USA selectivity
    psel[i,] <- parms$psel_fish
  }
  if(nspace == 2){
    psel[1,] <- c(1, 1, 1, 1, 1)
  }

  # Selectivity change in that year
  flag_sel <- rep(0, nyear)
  flag_sel[which(years == sel_change_yr):which(years == myear)] <- 1
  df <-list(# Parameters
            wage_ssb = t(wage_ssb),
            wage_catch = t(wage_catch),
            wage_survey = t(wage_survey),
            wage_mid = t(wage_mid),
            selidx = which(years == sel_change_yr),
            # Input parameters
            # Years to model time varying sel
            year_sel = length(sel_change_yr:max(years)),
            Msel = msel,
            Matsel= as.numeric(mat),
            nage = nage,
            ages = ages,
            nseason = nseason,
            nyear = nyear,
            # The extra year is to initialize
            tEnd = tEnd,
            # Analytical solution
            logQ = log(1.14135),
            # Selectivity
            Smin = 1,
            Smin_survey = 2,
            Smax = 6,
            Smax_survey = 6,
            flag_sel = flag_sel,
            surveyseason = surveyseason,
            # Frequency of survey years (e.g., 2 is every second year)
            nsurvey = nsurvey,
            # Make sure the survey has the same length as the catch time series
            survey = survey,
            # Is there a survey in that year?
            survey_x = ac.data$survey_x,
            # Make sure the survey has the same length as the catch time series
            survey_err = ac.data$ss.error,
            ss_survey = ac.data$ss.survey,
            flag_survey =ac.data$sflag,
            age_survey = age_survey.tmp,
            # Max age for age comps
            age_maxage = 15,
            # Catch
            # Catchobs = catch$Fishery, # Convert to kg
            ss_catch = ac.data$ss.catch,
            flag_catch =ac.data$cflag,
            age_catch = age_catch.tmp,
            # variance parameters
            logSDcatch = log(0.01),
            # Fixed in stock assessment
            logSDR = log(logSDR),
            logphi_survey = log(11.46),
            years = years,
            b = b,
            bfuture = bfuture,
            #logh = log(0.8),
            # Space parameters
            # Annual survey timing
            smul = 0.5,
            sigma_psel = 1.4,
            sum_zero = 0,
            nspace = nspace,
            #TAC = TAC,
            movemat = movemat,
            move = move,
            recruitmat = recruitmat,
            move.init = move.init,
            movefifty = movefifty,
            movemax = movemax,
            movesouth = movesouth,
            moveout = moveout,
            moveslope = moveslope,
            # F0 = Fin,
            psel = psel,
            parms = parms,
            Fnseason = Fnseason,
            selectivity_change = selectivity_change,
            Catch = catch)
            # Parameters from the estimation model

  Catch.country <- read.csv(system.file("extdata/catch_per_country.csv",
                                        package = "PacifichakeMSE",
                                        mustWork = TRUE))
  df$Catch.country <- as.matrix(Catch.country[,2:3])[,c(2,1)]
  df$Catch <- rowSums(df$Catch.country)
  if(nyear > length(df$Catch)){
    df$Catch <- c(df$Catch,rep(mean(df$Catch), nyear-length(df$Catch)))
  }
  if(nyear >nrow(df$Catch.country)){
    df$Catch.country <- rbind(df$Catch.country,t(replicate(nyear-nrow(Catch.country),colMeans(df$Catch.country))))
  }
  if(yr_future > 0){
    idx.future <- length(1966:myear)+seq(2,yr_future, by = df$nsurvey) # Years where survey occurs
    df$survey_x <- c(df$survey_x,rep(-2, yr_future))
    df$survey_x[idx.future] <- 2
    df$survey_err <- c(df$survey_err,rep(1, yr_future))
    df$survey_err[idx.future] <- mean(df$survey_err[df$survey_err != 1])
    df$ss_survey <- c(df$ss_survey, rep(0,  yr_future))
    df$ss_survey[idx.future] <- mean(df$ss_survey[df$ss_survey != -1])
    df$flag_survey <- c(df$flag_survey, rep(-1,yr_future))
    df$flag_survey[idx.future] <- 1
    df$flag_catch[years > 2018] <- 1
    Rdevs <- rnorm(n = yr_future,mean = 0, sd = exp(df$logSDR))
    #Rdevs <- rep(0, yr_future)
    df$parms$Rin <- c(df$parms$Rin,Rdevs)
    # Bias adjustment
    df$b <- c(df$b,rep(df$bfuture, yr_future))
  }
  df
}
