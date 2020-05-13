#' Load the hake data for seasons (TODO: Improve docs on this function)
#'
#' @param nseason Number of seasons
#' @param nspace Number of spatial areas
#' @param syr First year of historical simulations
#' @param myr Last year of historical simulations
#' @param ages A vector of ages
#' @param sel_change_yr A year in which a selectivity change took place
#' @param movemaxinit Maximum movement rate
#' @param movefiftyinit Age at 50 percent maximum movement rate
#' @param nsurvey Survey frequency
#' @param logSDR Recruitment deviations
#' @param bfuture Bias adjustment in the future
#' @param moveout Fraction of individuals that travel south in the last season
#' @param movesouth Fraction of individuals that move south during the year
#' @param moveinit Initial distribution of fish
#' @param moveslope Slope of the movement function
#' @param selectivity_change Should selectivity change?
#' @param yr_future How many years into the future should there be stochastic values
#' @param sel_hist Use historical selectivity?
#'
#' @return A list of Parameters, Input parameters, Survey, Catch, and others
#' @importFrom purrr map_dfr map_dfc
#' @importFrom dplyr pull summarize_all summarize_at
#' @export
#'
#' @examples
#' \dontrun{
#' df <- load_data_seasons(nseason = 12, nspace = 2)
#' }
load_data_seasons <- function(nseason = 4,
                              nspace = 2,
                              syr = 1966,
                              myr = 2018,
                              ages = 0:20,
                              sel_change_yr = 1991,
                              movemaxinit = 0.35,
                              movefiftyinit = 6,
                              nsurveys = 2,
                              logSDR = 1.4,
                              bfuture = 0.5,
                              moveout = 0.85,
                              movesouth = 0.05,
                              moveinit = NA,
                              moveslope = 0.9,
                              selectivity_change = 0,
                              yr_future  = 0,
                              sel_hist = TRUE){

  # Throw error if yr_future is exactly 1
  stopifnot(yr_future == 0 | yr_future > 1)
  # Throw error if moveinit is NA and nspace is not 2
  stopifnot(!is.na(moveinit) | (is.na(moveinit) & nspace == 2))

  lst <- csv_data(sel_hist)

  if(is.na(moveinit)){
    # nspace must be 2 due to error check above
    moveinit <-  c(0.25, 0.75)
  }
  yrs <- syr:(myr + yr_future)
  nyr <- length(yrs)
  tEnd <- length(yrs) * nseason

  # Age stuff
  nage <- length(ages)
  msel <- rep(1, nage)

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
  movemat <- array(0, dim = c(nspace, nage, nseason, nyr))
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
    move_init <- moveinit
  }else{
    move_init <- 1
  }
  # weight at age
  wage_ss <- lst$wage_ss %>%
    filter(Yr %in% yrs)
  wage_mid <- wage_ss %>%
    filter(Fleet == -1)
  wage_ssb <- wage_ss %>%
    filter(Fleet == -2)
  wage_catch <- wage_ss %>%
    filter(Fleet == 1)
  wage_survey <- wage_ss %>%
    filter(Fleet == 2)
  # Maturity from first year only
  mat <- wage_ssb[1,]
  # Age comps
  age_survey_df <- lst$age_survey_df %>%
    mutate(flag = 1)
  age_catch_df <- lst$age_catch_df %>%
    mutate(flag = 1)
  if(nseason == 1){
    surveyseason <-  1
  }else if(nseason == 4){
    surveyseason <- 3
  }else{
    surveyseason <- floor(nseason / 2)
  }
  psel <- lst$psel
  if(!sel_hist){
    psel <- matrix(0, 5, 28)
  }

  if(nseason == 4 & nspace == 2){
    # Add new rows or columns to the data here
    catch_props <- list(c(0.001, 0.188, 0.603, 0.208),
                        c(0.000, 0.317, 0.382, 0.302))
    # Rows must sum to 1
    catch_props <- map(catch_props, ~{
      .x / sum(.x)
    }) %>%
      set_names(seq_along(.)) %>%
      bind_rows() %>%
      t()
  }else{
    catch_props <- matrix(NA, nspace, nseason)
    # Equally distributed catch
    catch_props[1:nspace,] <- 1 / nseason
  }
  rmul <- ifelse(nspace == 2, 1.1, 1)

  # Just start all the simulations with the same initial conditions
  parms <- list(logRinit = lst$parms_scalar$logRinit + log(rmul),
                logh = lst$parms_scalar$logh,
                logMinit = lst$parms_scalar$logMinit,
                logSDsurv = lst$parms_scalar$logSDsurv,
                logphi_catch = lst$parms_scalar$logphi_catch,
                # Selectivity parameters
                psel_fish = lst$parms_sel %>% filter(source == "fish") %>% pull("value"),
                psel_surv = lst$parms_sel %>% filter(source == "survey") %>% pull("value"),
                initN = lst$initN,
                Rin = lst$Rdev,
                psel = psel)

  # USA selectivity
  # psel[i,] <- c(2.8476, 0.973, 0.3861, 0.1775, 0.5048)
  dsel <- matrix(NA, 5, nspace)
  dsel <- map(seq_len(ncol(dsel)), ~{
     dsel[,.x] = parms$psel_fish
   }) %>%
    set_names(seq_along(.)) %>%
    bind_rows() %>%
    t()
  if(nspace == 2){
    dsel[1,] <- rep(1, ncol(dsel))
  }

  # Selectivity change in that year
  flag_sel <- rep(FALSE, nyr)
  flag_sel[which(yrs == sel_change_yr):which(yrs == myr)] <- TRUE
  df <-list(# Parameters
            wage_ssb = t(wage_ssb),
            wage_catch = t(wage_catch),
            wage_survey = t(wage_survey),
            wage_mid = t(wage_mid),
            selidx = which(yrs == sel_change_yr),
            # Input parameters
            # yrs to model time varying sel
            year_sel = length(sel_change_yr:max(yrs)),
            Msel = msel,
            Matsel= as.numeric(mat),
            nage = nage,
            ages = ages,
            nseason = nseason,
            nyr = nyr,
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
            # Frequency of survey yrs (e.g., 2 is every second year)
            nsurvey = nsurvey,
            # Make sure the survey has the same length as the catch time series
            survey = lst$survey,
            # Is there a survey in that year?
            survey_x = lst$ac_data$survey_x,
            # Make sure the survey has the same length as the catch time series
            survey_err = lst$ac_data$ss.error,
            ss_survey = lst$ac_data$ss.survey,
            flag_survey = lst$ac_data$sflag,
            age_survey = lst$age_survey_tmp,
            # Max age for age comps
            age_maxage = 15,
            # Catch
            # Catchobs = catch$Fishery, # Convert to kg
            ss_catch = lst$ac_data$ss.catch,
            flag_catch = lst$ac_data$cflag,
            age_catch = lst$age_catch_tmp,
            # variance parameters
            logSDcatch = log(0.01),
            # Fixed in stock assessment
            logSDR = log(logSDR),
            logphi_survey = log(11.46),
            yrs = yrs,
            b = lst$b,
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
            move_init = move_init,
            movefifty = movefifty,
            movemax = movemax,
            movesouth = movesouth,
            moveout = moveout,
            moveslope = moveslope,
            # F0 = Fin,
            psel = psel,
            parms = parms,
            catch_props = catch_props,
            selectivity_change = selectivity_change,
            catch = lst$catch)
            # Parameters from the estimation model

  df$catch_country <- lst$catch_country %>%
    select(Can, US) %>%
    mutate(tot = rowSums(.))
  df$catch <- df$catch_country$tot
  # If nyr greater than the number of catch observations, append the mean catch across
  # time series to the end yrs
  if(nyr > length(df$catch)){
    df$catch <- c(df$catch, rep(mean(df$catch), nyr - length(df$catch)))
  }

  if(nyr > nrow(df$catch_country)){
    means <- df$catch_country %>%
      summarize_all(mean)
    df$catch_country <- df$catch_country %>%
      bind_rows(means)
  }

  if(yr_future > 1){
    # yrs where survey occurs
    idx.future <- length(syr:myr) + seq(2, yr_future, by = df$nsurvey)
    df$survey_x <- c(df$survey_x, rep(-2, yr_future))
    df$survey_x[idx.future] <- 2
    df$survey_err <- c(df$survey_err, rep(1, yr_future))
    df$survey_err[idx.future] <- mean(df$survey_err[df$survey_err != 1])
    df$ss_survey <- c(df$ss_survey, rep(0,  yr_future))
    df$ss_survey[idx.future] <- mean(df$ss_survey[df$ss_survey != -1])
    df$flag_survey <- c(df$flag_survey, rep(-1, yr_future))
    df$flag_survey[idx.future] <- 1
    df$flag_catch[yrs > myr] <- 1
    Rdevs <- rnorm(n = yr_future, mean = 0, sd = exp(df$logSDR))
    df$parms$Rin <- c(df$parms$Rin, Rdevs)
    # Bias adjustment
    df$b <- c(df$b, rep(df$bfuture, yr_future))
  }
  df
}
