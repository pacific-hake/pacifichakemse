#' Prepare the data for the operating model
#'
#' @param n_season Number of seasons
#' @param n_space Number of spatial areas
#' @param s_yr First year of historical simulations
#' @param m_yr Last year of historical simulations
#' @param ages A vector of ages
#' @param sel_change_yr A year in which a selectivity change took place
#' @param move_max_init Maximum movement rate
#' @param move_fifty_init Age at 50 percent maximum movement rate
#' @param n_survey Survey frequency
#' @param rdev_sd Recruitment deviation Standard deviation
#' @param b_future Bias adjustment in the future
#' @param move_out Fraction of individuals that travel south in the last season
#' @param move_south Fraction of individuals that move south during the year
#' @param move_init Initial distribution of fish
#' @param move_slope Slope of the movement function
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
#' df <- load_data_seasons(n_season = 2, n_space = 2)
#' }
load_data_seasons <- function(n_season = 4,
                              n_space = 2,
                              n_survey = 2,
                              s_yr = 1966,
                              m_yr = 2018,
                              ages = 0:20,
                              rdev_sd = 1.4,
                              move_init = NULL,
                              move_max_init = 0.35,
                              move_fifty_init = 6,
                              move_out = 0.85,
                              move_south = 0.05,
                              move_slope = 0.9,
                              selectivity_change = 0,
                              b_future = 0.5,
                              yr_future  = 0,
                              sel_change_yr = 1991,
                              sel_hist = TRUE){

  verify_argument(n_season, "numeric", 1, 1:4)
  verify_argument(n_space, "numeric", 1, 1:2)
  verify_argument(s_yr, "numeric", 1)
  verify_argument(m_yr, "numeric", 1)
  if(!is.null(ages)){
    if(length(ages) == 1){
      verify_argument(ages, "numeric")
    }else{
      verify_argument(ages, "integer")
    }
  }
  verify_argument(rdev_sd, "numeric", 1)
  if(!is.null(move_init)){
    stopifnot(class(move_init) == "numeric")
    stopifnot(length(move_init) == 1)
  }
  verify_argument(move_max_init, "numeric", 1)
  verify_argument(move_fifty_init, "numeric", 1)
  verify_argument(move_out, "numeric", 1)
  verify_argument(move_south, "numeric", 1)
  verify_argument(move_slope, "numeric", 1)
  verify_argument(selectivity_change, "numeric", 1)
  verify_argument(b_future, "numeric", 1)
  verify_argument(yr_future, "numeric", 1)
  verify_argument(sel_change_yr, "numeric", 1)
  verify_argument(sel_hist, "logical", 1)

  # Throw error if yr_future is exactly 1
  stopifnot(yr_future == 0 | yr_future > 1)
  # Throw error if move_init is NULL and n_space is not 2
  stopifnot(!is.null(move_init) | (is.null(move_init) & n_space == 2))

  lst <- csv_data(sel_hist)

  if(is.null(move_init)){
    # n_space must be 2 due to error check above
    move_init <-  c(0.25, 0.75)
  }
  yrs <- s_yr:(m_yr + yr_future)
  n_yr <- length(yrs)
  t_end <- length(yrs) * n_season

  # Age stuff
  n_age <- length(ages)
  m_sel <- rep(1, n_age)

  # TODO: for later use
  # 4 is seasonality
  recruit_mat <- matrix(0, n_space)
  # 10 percent change of spawning north
  recruit_mat[1] <- 1
  # 90 percent of spawning south
  recruit_mat[2] <- 1

  # Maturity
  move_fifty <- move_fifty_init
  move_max <- rep(move_max_init, n_season)
  # Chances of moving in to the other grid cell
  move_mat <- array(0, dim = c(n_space, n_age, n_season, n_yr))
  move <- ifelse(n_space == 1, FALSE, TRUE)
  if(move){
    for(j in 1:n_space){
      for(i in 1:n_season){
        move_mat[j,,i,] <- move_max[i] / (1 + exp(-move_slope * (ages - move_fifty)))
      }
    }
    # Recruits and 1 year olds don't move
    move_mat[,1:2,,] <- 0
    # For the standard model
    if(n_season == 4){
      # Don't move south during the year
      move_mat[1, 3:n_age, 2:3,] <- move_south
      # continuing south movement at spawning time
      move_mat[1, 3:n_age, 1,] <- move_south
      move_mat[1, 3:n_age, n_season,] <- move_out
      move_mat[2, 3:n_age, n_season,] <- move_south
    }
    move_init <- move_init
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
  mat <- wage_ssb[1,] %>% select(-c(Yr, Fleet))
  # Age comps
  age_survey_df <- lst$age_survey_df %>%
    mutate(flag = 1)
  age_catch_df <- lst$age_catch_df %>%
    mutate(flag = 1)
  if(n_season == 1){
    survey_season <-  1
  }else if(n_season == 4){
    survey_season <- 3
  }else{
    survey_season <- floor(n_season / 2)
  }
  p_sel <- lst$p_sel
  if(!sel_hist){
    p_sel <- matrix(0, 5, 28)
  }

  if(n_season == 4 & n_space == 2){
    # Add new rows or columns to the data here
    catch_props_season <- list(c(0.001, 0.188, 0.603, 0.208),
                               c(0.000, 0.317, 0.382, 0.302))
    # Rows must sum to 1
    catch_props_season <- map(catch_props_season, ~{
      .x / sum(.x)
    }) %>%
      set_names(seq_along(.)) %>%
      bind_rows() %>%
      t()
  }else{
    catch_props_season <- matrix(NA, n_space, n_season)
    # Equally distributed catch
    catch_props_season[1:n_space,] <- 1 / n_season
  }
  r_mul <- ifelse(n_space == 2, 1.1, 1)

  # Just start all the simulations with the same initial conditions
  parms_init <- list(log_r_init = lst$parms_scalar$logRinit + log(r_mul),
                     log_h = lst$parms_scalar$logh,
                     log_m_init = lst$parms_scalar$logMinit,
                     log_sd_surv = lst$parms_scalar$logSDsurv,
                     log_phi_catch = lst$parms_scalar$logphi_catch,
                     # Selectivity parameters
                     p_sel_fish = lst$parms_sel %>% filter(source == "fish") %>% pull("value"),
                     p_sel_surv = lst$parms_sel %>% filter(source == "survey") %>% pull("value"),
                     init_n = lst$init_n,
                     r_in = lst$r_dev,
                     p_sel = p_sel)

  # USA selectivity
  # psel[i,] <- c(2.8476, 0.973, 0.3861, 0.1775, 0.5048)
  d_sel <- matrix(NA, 5, n_space)
  d_sel <- map(seq_len(ncol(d_sel)), ~{
     d_sel[,.x] = parms_init$p_sel_fish
   }) %>%
    set_names(seq_along(.)) %>%
    bind_rows() %>%
    t()
  if(n_space == 2){
    d_sel[1,] <- rep(1, ncol(d_sel))
  }

  # Selectivity change in that year
  flag_sel <- rep(FALSE, n_yr)
  flag_sel[which(yrs == sel_change_yr):which(yrs == m_yr)] <- TRUE
  df <-list(parms_init = parms_init,
            wage_ssb = wage_ssb,
            wage_catch = wage_catch,
            wage_survey = wage_survey,
            wage_mid = wage_mid,
            sel_idx = which(yrs == sel_change_yr),
            year_sel = length(sel_change_yr:max(yrs)),
            m_sel = m_sel,
            mat_sel = as.numeric(mat),
            n_season = n_season,
            s_yr = s_yr,
            m_yr = m_yr,
            n_yr = n_yr,
            ages = ages,
            n_age = n_age,
            # The extra year is to initialize
            t_end = t_end,
            # Analytical solution
            log_q = log(1.14135),
            # Selectivity
            selectivity_change = selectivity_change,
            s_min = 1,
            s_min_survey = 2,
            s_max = 6,
            s_max_survey = 6,
            flag_sel = flag_sel,
            survey_season = survey_season,
            # Frequency of survey yrs (e.g., 2 is every second year)
            n_survey = n_survey,
            # Make sure the survey has the same length as the catch time series
            survey = lst$survey,
            # Is there a survey in that year?
            survey_x = lst$ac_data$survey_x,
            # Make sure the survey has the same length as the catch time series
            survey_err = lst$ac_data$ss.error,
            ss_survey = lst$ac_data$ss.survey,
            flag_survey = lst$ac_data$sflag,
            age_survey = lst$age_survey_tmp,
            age_max_age = 15,
            ss_catch = lst$ac_data$ss.catch,
            flag_catch = lst$ac_data$cflag,
            age_catch = lst$age_catch_tmp,
            # variance parameters
            log_sd_catch = log(0.01),
            # Fixed in stock assessment
            rdev_sd = log(rdev_sd),
            log_phi_survey = log(11.46),
            yrs = yrs,
            b = lst$b,
            b_future = b_future,
            #logh = log(0.8),
            # Space parameters
            # Annual survey timing
            s_mul = 0.5,
            sigma_p_sel = 1.4,
            sum_zero = 0,
            n_space = n_space,
            recruit_mat = recruit_mat,
            move = move,
            move_mat = move_mat,
            move_init = move_init,
            move_fifty = move_fifty,
            move_max = move_max,
            move_south = move_south,
            move_out = move_out,
            move_slope = move_slope,
            catch_props_season = catch_props_season,
            catch = lst$catch,
            p_sel = p_sel)

  df$catch_country <- lst$catch_country %>%
    select(Can, US) %>%
    mutate(tot = rowSums(.))
  df$catch <- df$catch_country$tot
  # If n_yr greater than the number of catch observations, append the mean catch across
  # time series to the end yrs
  if(n_yr > length(df$catch)){
    df$catch <- c(df$catch, rep(mean(df$catch), n_yr - length(df$catch)))
  }

  if(n_yr > nrow(df$catch_country)){
    means <- df$catch_country %>%
      summarize_all(mean)
    df$catch_country <- df$catch_country %>%
      bind_rows(means)
  }

  if(yr_future > 1){
    # yrs where survey occurs
    idx_future <- length(s_yr:m_yr) + seq(2, yr_future, by = df$n_survey)
    df$survey_x <- c(df$survey_x, rep(-2, yr_future))
    df$survey_x[idx_future] <- 2
    df$survey_err <- c(df$survey_err, rep(1, yr_future))
    df$survey_err[idx_future] <- mean(df$survey_err[df$survey_err != 1])
    df$ss_survey <- c(df$ss_survey, rep(0,  yr_future))
    df$ss_survey[idx_future] <- mean(df$ss_survey[df$ss_survey != -1])
    df$flag_survey <- c(df$flag_survey, rep(-1, yr_future))
    df$flag_survey[idx_future] <- 1
    df$flag_catch[yrs > m_yr] <- 1
    r_devs <- rnorm(n = yr_future, mean = 0, sd = exp(df$rdev_sd))
    df$parms_init$r_in <- c(df$parms_init$r_in, r_devs)
    # Bias adjustment
    df$b <- c(df$b, rep(df$b_future, yr_future))
  }
  df
}
