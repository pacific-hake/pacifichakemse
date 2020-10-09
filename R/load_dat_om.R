#' Prepare the data for the operating model
#'
#' @param ss_model A model input/output list representing the SS model as returned from
#' [load_ss_model_from_rds()]
#' @param n_sim_yrs Number of years to be simulated. This is used to set up arrays dimensions
#' to include simulations so that arrays don't have to be redimensioned in the MSE loop code
#' @param n_season Number of seasons
#' @param season_names A vector of names for the seasons. Length must equal `n_season`
#' @param n_space Number of spatial areas
#' @param space_names A vector of names for the spaces. Length must equal `n_space`
#' @param s_yr First year of historical simulations
#' @param m_yr Last year of historical simulations
#' @param ages A vector of ages
#' @param age_names A vector of names for the ages. Length must equal length of `ages`
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
#' @param ages_no_move Ages of fish which do not move in the movement model
#' @param selectivity_change Should selectivity change?
#' @param s_min Minimum age in fishery selectivity
#' @param s_max Maximum age in fishery selectivity
#' @param s_min_survey Minimum age in survey selectivity
#' @param s_max_survey Maximum age in survey selectivity
#' @param sel_hist Use historical selectivity?
#' @param f_space The proportion of TAC given to each country. First value is Canada,
#' the second is the US
#' @param log_phi_survey Survey phi parameter value
#' @param ... Absorb arguments destined for other functions
#'
#' @return A list of Parameters, Input parameters, Survey, Catch, and others
#' @importFrom tibble tibble as_tibble
#' @importFrom purrr map_dfr map_dfc
#' @importFrom dplyr pull summarize_all summarize_at
#' @export
#'
#' @examples
#' \dontrun{
#' df <- load_data_om(n_season = 2, n_space = 2)
#' }
load_data_om <- function(ss_model = NULL,
                         n_sim_yrs = NULL,
                         n_season = 4,
                         season_names = c("Season1", "Season2", "Season3", "Season4"),
                         n_space = 2,
                         space_names = c("Canada", "US"),
                         n_survey = 2,
                         s_yr = 1966,
                         m_yr = 2018,
                         ages = 0:20,
                         age_names = paste("age", 0:20),
                         rdev_sd = 1.4,
                         move_init = NULL,
                         move_max_init = 0.35,
                         move_fifty_init = 6,
                         move_out = 0.85,
                         move_south = 0.05,
                         move_slope = 0.9,
                         ages_no_move = 0,
                         selectivity_change = 0,
                         s_min = 1,
                         s_max = 6,
                         s_min_survey = 2,
                         s_max_survey = 6,
                         b_future = 0.5,
                         sel_change_yr = 1991,
                         sel_hist = TRUE,
                         f_space = c(0.2612, 0.7388),
                         log_phi_survey = log(11.46),
                         zero_rdevs = FALSE,
                         ...){

  verify_argument(ss_model, "list")
  verify_argument(n_sim_yrs, "numeric", 1)
  verify_argument(n_season, c("numeric", "integer"), 1, 1:4)
  verify_argument(season_names, "character", n_season)
  verify_argument(n_space, c("numeric", "integer"), 1, 1:2)
  verify_argument(space_names, "character", n_space)
  verify_argument(s_yr, c("numeric", "integer"), 1)
  verify_argument(m_yr, c("numeric", "integer"), 1)
  if(!is.null(ages)){
    verify_argument(ages, c("numeric", "integer"))
    verify_argument(age_names, "character", length(ages))
  }
  verify_argument(rdev_sd, c("numeric", "integer"), 1)
  if(!is.null(move_init)){
    stopifnot(class(move_init) == "numeric")
    stopifnot(length(move_init) == 1)
  }
  verify_argument(move_max_init, "numeric", 1)
  verify_argument(move_fifty_init, "numeric", 1)
  verify_argument(move_out, "numeric", 1)
  verify_argument(move_south, "numeric", 1)
  verify_argument(move_slope, "numeric", 1)
  verify_argument(ages_no_move, c("numeric", "integer"))
  verify_argument(selectivity_change, c("numeric", "integer"), 1)
  verify_argument(s_min, c("numeric", "integer"), 1)
  verify_argument(s_max, c("numeric", "integer"), 1)
  verify_argument(s_min_survey, c("numeric", "integer"), 1)
  verify_argument(s_max_survey, c("numeric", "integer"), 1)
  verify_argument(b_future, "numeric", 1)
  verify_argument(sel_change_yr, c("numeric", "integer"), 1)
  verify_argument(sel_hist, "logical", 1)
  verify_argument(f_space, "numeric", n_space)
  verify_argument(log_phi_survey, "numeric", 1)

  # Throw error if the number of simulation years is exactly 1
  stopifnot(n_sim_yrs == 0 | n_sim_yrs > 1)
  # Throw error if move_init is NULL and n_space is not 2
  stopifnot(!is.null(move_init) | (is.null(move_init) & n_space == 2))

  lst <- csv_data()

  lst$yrs <- s_yr:(m_yr + n_sim_yrs)
  lst$s_yr <- s_yr
  lst$m_yr <- m_yr
  lst$n_yr <- length(lst$yrs)
  lst$sel_change_yr <- sel_change_yr
  lst$sel_idx <- which(lst$yrs == lst$sel_change_yr)
  lst$yr_sel <- length(lst$sel_change_yr:max(lst$yrs))
  lst$n_season <- n_season
  lst$season_names <- season_names
  lst$t_end <- lst$n_yr * lst$n_season
  lst$n_space <- n_space
  lst$space_names <- space_names
  lst$rdev_sd <- log(rdev_sd)
  lst$log_q <- log(1.14135)
  lst$log_sd_catch <- log(0.01)
  lst$s_mul <- 0.5
  lst$sigma_p_sel <- 1.4
  lst$sum_zero <- 0
  lst$move <- ifelse(lst$n_space == 1, FALSE, TRUE)
  lst$log_phi_survey <- log_phi_survey
  lst$s_min <- s_min
  lst$s_max <- s_max
  lst$s_min_survey <- s_min_survey
  lst$s_max_survey <- s_max_survey
  lst$selectivity_change <- selectivity_change
  lst$ages <- ages
  lst$age_names <- age_names
  lst$age_max_age <- nrow(ss_model$age_survey)
  lst$b_future <- b_future

  lst$survey <- ss_model$survey
  lst$survey_err <- ss_model$survey_err
  lst$ss_survey <- ss_model$ss_survey
  lst$flag_survey <- ss_model$flag_survey
  lst$flag_catch <- ss_model$flag_catch

  lst$wage_catch_df <- ss_model$wage_catch_df
  lst$wage_survey_df <- ss_model$wage_survey_df
  lst$wage_ssb_df <- ss_model$wage_ssb_df
  lst$wage_mid_df <- ss_model$wage_mid_df
  lst$mat_sel <- ss_model$mat_sel
  lst$age_survey <- ss_model$age_survey
  lst$age_catch <- ss_model$age_catch
  lst$ss_catch <- ss_model$ss_catch
  lst$sel_by_yrs <- ss_model$sel_by_yrs

  future_yrs <- (m_yr + 1):(m_yr + n_sim_yrs)

  lst <- append(lst, setup_blank_om_objects(yrs = lst$yrs,
                                            ages = lst$ages,
                                            max_surv_age = lst$age_max_age,
                                            n_space = lst$n_space,
                                            n_season = lst$n_season))

  lst$move_init <- move_init
  if(is.null(lst$move_init)){
    # lst$n_space must be 2 due to error check above
    lst$move_init <- c(0.25, 0.75)
  }
  names(lst$move_init) <- lst$space_names

  # Age stuff
  lst$n_age <- length(ages)
  lst$m_sel <- rep(1, lst$n_age)

  lst$recruit_mat <- rep(1, lst$n_space)
  names(lst$recruit_mat) <- lst$space_names

  # Maturity
  lst$move_fifty <- move_fifty_init
  lst$move_max <- rep(move_max_init, lst$n_season)
  names(lst$move_max) <- lst$season_names
  lst$move_south <- move_south
  lst$move_out <- move_out
  lst$move_slope <- move_slope

  move_mat_obj <- init_movement_mat(lst$n_space,
                                    lst$space_names,
                                    lst$n_season,
                                    lst$season_names,
                                    lst$n_yr,
                                    lst$yrs,
                                    lst$move_max,
                                    lst$move_slope,
                                    lst$move_fifty,
                                    lst$move_south,
                                    lst$move_out,
                                    lst$move_init,
                                    ages_no_move,
                                    lst$ages,
                                    lst$age_names,
                                    f_space)

  lst$move_mat <- move_mat_obj$move_mat
  lst$move_init <- move_mat_obj$move_init
  lst$f_space <- move_mat_obj$f_space
  names(lst$f_space) <- lst$space_names

  # Set up survey season
  if(lst$n_season == 1){
    lst$survey_season <-  1
  }else if(lst$n_season == 4){
    lst$survey_season <- 3
  }else{
    lst$survey_season <- floor(lst$n_season / 2)
  }

  if(lst$n_season == 4 & lst$n_space == 2){
    lst$catch_props_space_season <- ss_model$catch_props_space_season
  }else{
    lst$catch_props_space_season <- matrix(NA, lst$n_space, lst$n_season)
    # Equally distributed catch
    lst$catch_props_space_season[1:lst$n_space,] <- 1 / lst$n_season
  }

  lst$r_mul <- ifelse(lst$n_space == 2, 1.1, 1)

  if(zero_rdevs){
    r_devs <- rep(0, n_sim_yrs)
  }else{
    r_devs <- rnorm(n = n_sim_yrs,
                    mean = 0,
                    sd = exp(df$rdev_sd))
  }

  lst$r_dev <- lst$r_dev[, 1]
  if(n_sim_yrs > 0){
    lst$r_dev <- c(lst$r_dev, r_devs)
  }
  lst$r_dev <- lst$r_dev %>%
    as.data.frame() %>%
    as_tibble() %>%
    rename(value = 1) %>%
    mutate(yr = lst$yrs) %>%
    select(yr, everything())
  lst$init_n <- ss_model$init_n %>%
    as.data.frame() %>%
    as_tibble() %>%
    mutate(age = ages[-which(0 %in% ages)]) %>%
    select(age, everything()) %>%
    rename(value = 2)

  # Selectivity change in that year
  lst$flag_sel <- rep(FALSE, lst$n_yr)
  lst$flag_sel[which(lst$yrs == lst$sel_change_yr):which(lst$yrs == m_yr)] <- TRUE

  lst$catch_country <- lst$catch_country %>%
    select(year, Can, US) %>%
    mutate(total = rowSums(.)) %>% set_names(c("year", "space1", "space2", "total"))
  # TODO: Check why the sum of the catch country file does not add up to the total in the SS data file
  # lst$catch_obs <- lst$catch_country %>% pull(total)
  # If n_yr greater than the number of catch observations, append the mean catch across
  # time series to the end lst$yrs
  lst$catch_obs <- ss_model$catch_obs %>%
    as_tibble()
  if(lst$n_yr > nrow(lst$catch_obs)){
    new_df <- rep(mean(lst$catch_obs$value), lst$n_yr - nrow(lst$catch_obs)) %>%
      as_tibble() %>%
      mutate(yr = future_yrs) %>%
      select(yr, value)
    lst$catch_obs <- lst$catch_obs %>%
      bind_rows(new_df)
  }

  if(lst$n_yr > nrow(lst$catch_country)){
    means <- lst$catch_country %>%
      summarize_all(mean) %>%
      slice(rep(1, each = n_sim_yrs)) %>%
      mutate(year = future_yrs)
    lst$catch_country <- lst$catch_country %>%
      bind_rows(means)
  }

  add_wage_yrs <- function(df, row = 1){
    if(lst$n_yr > nrow(df)){
      yr_one_vals <- df %>%
        slice(rep(row, each = n_sim_yrs)) %>%
        mutate(Yr = future_yrs)
      df <- df %>%
        bind_rows(yr_one_vals)
    }
    df
  }
  lst$wage_catch_df <- add_wage_yrs(lst$wage_catch_df)
  lst$wage_survey_df <- add_wage_yrs(lst$wage_survey_df)
  lst$wage_ssb_df <- add_wage_yrs(lst$wage_ssb_df)
  lst$wage_mid_df <- add_wage_yrs(lst$wage_mid_df)

  # Parameters to initialize the OM with
  lst$parameters <- list(log_r_init = ss_model$parms_scalar$log_r_init + log(lst$r_mul),
                         log_h = ss_model$parms_scalar$log_h,
                         log_m_init = ss_model$parms_scalar$log_m_init,
                         log_sd_surv = ss_model$parms_scalar$log_sd_surv,
                         log_phi_survey = ss_model$parms_scalar$log_phi_survey,
                         log_phi_catch = ss_model$parms_scalar$log_phi_catch,
                         p_sel_fish = ss_model$p_sel_fish,
                         p_sel_surv = ss_model$p_sel_surv,
                         init_n = lst$init_n,
                         r_in = lst$r_dev)

  if(n_sim_yrs > 1){
    # Assumes survey is in every odd year only into the future
    odd_future_yrs <- as.logical(future_yrs %% 2)
    idx_future <- (length(s_yr:m_yr) + 1):length(lst$yrs)
    idx_future <- idx_future[odd_future_yrs]

    lst$survey_err <- c(lst$survey_err, rep(1, n_sim_yrs))
    lst$survey_err[idx_future] <- mean(lst$survey_err[lst$survey_err != 1])
    lst$ss_survey <- c(lst$ss_survey, rep(0, n_sim_yrs))
    lst$ss_survey[idx_future] <- mean(lst$ss_survey[lst$ss_survey != -1])
    lst$flag_survey <- c(lst$flag_survey, rep(-1, n_sim_yrs))
    lst$flag_survey[idx_future] <- 1
    lst$flag_catch <- c(lst$flag_catch, rep(-1, n_sim_yrs))
    # Bias adjustment
    lst$b <- c(ss_model$b, rep(lst$b_future, n_sim_yrs))
    # Weight-at-age
  }

  lst
}

#' Initialize the movement model matrix. An alternative function should be
#' written if changes are required to the initialization assumptions
#'
#' @param n_space See [load_data_om()]
#' @param space_names See [load_data_om()]
#' @param n_season See [load_data_om()]
#' @param season_names See [load_data_om()]
#' @param n_yr The number of years in the array dimension
#' @param yrs A vector of names for the years. Length must equal `n_yr`
#' @param move_max A vector of the maximum movement rate, one for each of `n_seasons`
#' @param move_slope  See [load_data_om()]
#' @param move_fifty Age at 50 percent movement rate
#' @param move_south  See [load_data_om()]
#' @param move_out  See [load_data_om()]
#' @param move_init  See [load_data_om()]
#' @param ages_no_move See [load_data_om()]
#' @param ages See [load_data_om()]
#' @param age_names See [load_data_om()]
#' @param f_space See [load_data_om()]
#'
#' @return A list of 3 elements: The `move_mat` matrix for movement, the `move_init`
#' vector of length `n_space` and the `f_space` vector of length `n_space`
#' @export
init_movement_mat <- function(n_space = NULL,
                              space_names = NULL,
                              n_season = NULL,
                              season_names = NULL,
                              n_yr = NULL,
                              yrs = NULL,
                              move_max = NULL,
                              move_slope = NULL,
                              move_fifty = NULL,
                              move_south = NULL,
                              move_out = NULL,
                              move_init = NULL,
                              ages_no_move = NULL,
                              ages = NULL,
                              age_names = NULL,
                              f_space = NULL){

  verify_argument(n_space, c("numeric", "integer"), 1)
  verify_argument(space_names, "character", n_space)
  verify_argument(n_season, c("numeric", "integer"), 1)
  verify_argument(season_names, "character", n_season)
  verify_argument(n_yr, c("numeric", "integer"), 1)
  verify_argument(yrs, c("numeric", "integer"))
  verify_argument(move_max, "numeric", n_season)
  verify_argument(move_slope, "numeric", 1)
  verify_argument(move_fifty, "numeric", 1)
  verify_argument(move_south, "numeric", 1)
  verify_argument(move_out, "numeric", 1)
  verify_argument(move_init, "numeric", n_space)
  verify_argument(ages_no_move, c("numeric", "integer"))
  verify_argument(ages, c("numeric", "integer"))
  verify_argument(age_names, "character", length(ages))
  verify_argument(f_space, "numeric", n_space)

  n_age <- length(ages)
  move_mat <- array(0,
                    dim = c(n_space, n_age, n_season, n_yr),
                    dimnames = list(space_names,
                                    age_names,
                                    season_names,
                                    yrs))
  for(i in 1:n_space){
    for(j in 1:n_season){
      move_mat[i, , j, ] <- move_max[j] / (1 + exp(-move_slope * (ages - move_fifty)))
    }
  }

  # Some ages don't move
  move_mat[, which(ages_no_move %in% ages), , ] <- 0
  if(n_season == 4){
    # Don't move south during the year
    move_mat[1, 3:n_age, 2:3,] <- move_south
    # continuing south movement at spawning time
    move_mat[1, 3:n_age, 1,] <- move_south
    # The following two lines are co-dependent. The fish that move_out of Canada
    # move into the US
    move_mat[1, 3:n_age, 4,] <- move_out
    move_mat[2, 3:n_age, 4,] <- move_south

  }else{
    move_init <- 1
    # All F occurs in US
    f_space <- c(0, 1)
  }
  list(move_mat = move_mat,
       move_init = move_init,
       f_space = f_space)
}