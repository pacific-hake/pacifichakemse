#' Create the data for [TMB::MakeADFun()]
#'
#' @param sim_data Operating model
#' @param df input parameters
#' @param history Logical. If TRUE use historical data. If FALSE, use simulated OM data
#'
#' @return A list of the data needed by [TMB::MakeADFun()]
#' @export
create_TMB_data <- function(sim_data = NULL,
                            df = NULL,
                            history = FALSE){
  verify_argument(sim_data, "list")
  verify_argument(df, "list")
  verify_argument(history, "logical", 1)

  msel <- rep(1, df$n_age)
  # Maturity
  mat <- df$mat_sel

  if(max(df$yrs) > df$m_yr){
    # Copy last year of weight-at-age data and use that as the simulated year
    df$wage_catch <- wage_add_yr(df$wage_catch)
    df$wage_survey <- wage_add_yr(df$wage_survey)
    df$wage_mid <- wage_add_yr(df$wage_mid)
    df$wage_ssb <- wage_add_yr(df$wage_ssb)
  }

  # Catch
  browser()
  catch <- sim_data$Catch

  # Survey abundance
  df.survey <- sim_data$survey
  Fnew <- sim_data$F0

  # Bias adjustment factor
  # Parameters
  b <- df$b
  #b <- matrix(1, tEnd)

  # Load parameters from the assessment
  ### h prior distribution
  hmin <- 0.2
  hmax <- 1
  hprior <- 0.777
  hsd <- 0.117

  mu <- (hprior-hmin)/(hmax-hmin)
  tau <- ((hprior-hmin)*(hmax-hprior))/hsd^2-1


  df.new <-list(      #### Parameters #####
                  wage_catch = (wage_catch),
                  wage_survey = (wage_survey),
                  wage_ssb = wage_ssb,
                  wage_mid = wage_mid,
                  year_sel = df$year_sel,
                  #  Input parameters
                  Msel = msel,
                  Matsel= mat,
                  nage = nage,
                  age = age,
                  selYear = df$selidx,
                  years = years,
                  tEnd = length(years), # The extra year is to initialize
                  logQ = df$logQ,   # Analytical solution
                  # Selectivity
                  flag_sel = df$flag_sel,
                  Smin = df$Smin,
                  Smin_survey = df$Smin_survey,
                  Smax = df$Smax,
                  Smax_survey = df$Smax_survey,
                  b = b,
                  # survey
                  survey = sim_data$survey,#df.survey, # Make sure the survey has the same length as the catch time series
                  survey_x = df$survey_x, # Is there a survey in that year?
                  ss_survey = df$ss_survey,
                  flag_survey =df$flag_survey,
                  age_survey = sim_data$age_comps_surv,
                  age_maxage = df$age_maxage, # Max age for age comps
                  # Catch
                  Catchobs = sim_data$Catch,
                  #                Catchobs = catch$Fishery, # Convert to kg
                  ss_catch = df$ss_catch,
                  flag_catch =df$flag_catch,
                  age_catch = sim_data$age_catch,
                  # variance parameters
                  logSDcatch = df$logSDcatch,
                  logSDR = df$logSDR, # Fixed in stock assessment ,
                  logphi_survey = df$logphi_survey,
                  sigma_psel = 1.4,
                  sum_zero = df$sum_zero,
                  smul = df$smul,
                  Bprior= tau*mu,
                  Aprior = tau*(1-mu),
                  survey_err = df$survey_err

  )

  if(history){
    df.new$survey <- df$survey[,1]

    if(length(df$survey[,1]) != length(df.new$survey)){
      stop('data not available')
    }

        df.new$age_catch <- as.matrix(df$age_catch)
        df.new$age_survey <- as.matrix(df$age_survey)
        df.new$Catchobs <- df$Catch

  }




  return(df.new)

}
