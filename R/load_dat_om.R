#' Prepare the data for the operating model
#'
#' @param ss_model A model input/output list representing the SS model as returned from
#' [load_ss_model_from_rds()]
#' @param yr_future Number of years to run the OM into the future
#' @param n_sim_yrs Number of years to allocate for the movement matrix.
#' @param n_season Number of seasons
#' @param season_names A vector of names for the seasons. Length must equal `n_season`
#' @param n_space Number of spatial areas
#' @param space_names A vector of names for the spaces. Length must equal `n_space`
#' @param ages A vector of ages
#' @param age_names A vector of names for the ages. Length must equal length of `ages`
#' @param sel_change_yr A year in which a selectivity change took place
#' @param move_max_init Maximum movement rate
#' @param move_fifty_init Age at 50 percent maximum movement rate
#' @param n_survey Survey frequency
#' @param rdev_sd Recruitment deviation Standard deviation
#' @param rdev_seed Random seed to use for all future random recruitment deviations
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
#' @param f_space The proportion of TAC given to each country. First value is Canada,
#' the second is the US
#' @param log_phi_survey Survey phi parameter value
#' @param ... Absorb arguments destined for other functions
#' @param zero_rdevs Logical. If TRUE, make all recruitment deviations into the future zero.
#' If FALSE, they will be given random normal values based on rdev_sd
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
                         yr_future = 0,
                         n_sim_yrs = 0,
                         n_season = 4,
                         season_names = c("Season1", "Season2", "Season3", "Season4"),
                         n_space = 2,
                         space_names = c("Canada", "US"),
                         n_survey = 2,
                         ages = 0:20,
                         age_names = paste("age", 0:20),
                         rdev_sd = 1.4,
                         rdev_seed = 42,
                         move_init = NULL,
                         move_max_init = 0.35,
                         move_fifty_init = 6,
                         move_out = 0.85,
                         move_south = 0.05,
                         move_slope = 0.9,
                         ages_no_move = 0:1,
                         selectivity_change = 0,
                         s_min = 1,
                         s_max = 6,
                         s_min_survey = 2,
                         s_max_survey = 6,
                         b_future = 0.5,
                         sel_change_yr = 1991,
                         f_space = c(0.2612, 0.7388),
                         log_phi_survey = log(11.46),
                         zero_rdevs = FALSE,
                         ...){

  verify_argument(ss_model, "list")
  verify_argument(yr_future, "numeric", 1)
  verify_argument(n_season, c("numeric", "integer"), 1, 1:4)
  verify_argument(season_names, "character", n_season)
  verify_argument(n_space, c("numeric", "integer"), 1, 1:2)
  verify_argument(space_names, "character", n_space)
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
  verify_argument(f_space, "numeric", n_space)
  verify_argument(log_phi_survey, "numeric", 1)

  # Throw error if the number of simulation years is exactly 1
  stopifnot(yr_future >= 0 & yr_future != 1)
  # Throw error if move_init is NULL and n_space is not 2
  stopifnot(!is.null(move_init) | (is.null(move_init) & n_space == 2))

  # Only one of n_sim_yrs or yr_future can be used
  if(n_sim_yrs > 0 & yr_future > 0){
    stop("Only one of n_sim_yrs or yr_future can be used.", call. = FALSE)
  }
  populate_future <- TRUE
  if(n_sim_yrs > 0){
    populate_future <- FALSE
    yr_future <- n_sim_yrs
  }

  lst <- NULL
  lst$s_yr <- ss_model$s_yr
  lst$m_yr <- ss_model$m_yr
  lst$yrs <- lst$s_yr:(lst$m_yr + yr_future)
  lst$n_yr <- length(lst$yrs)
  lst$sel_change_yr <- sel_change_yr
  lst$sel_idx <- which(lst$yrs == lst$sel_change_yr)
  lst$yr_sel <- length(lst$sel_change_yr:lst$m_yr)
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

  future_yrs <- (lst$m_yr + 1):(lst$m_yr + yr_future)

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

  move_mat_obj <- init_movement_mat(n_space = lst$n_space,
                                    space_names = lst$space_names,
                                    n_season = lst$n_season,
                                    season_names = lst$season_names,
                                    m_yr = lst$m_yr,
                                    n_yr = lst$n_yr,
                                    yrs = lst$yrs,
                                    move_max = lst$move_max,
                                    move_slope = lst$move_slope,
                                    move_fifty = lst$move_fifty,
                                    move_south = lst$move_south,
                                    move_out = lst$move_out,
                                    move_init = lst$move_init,
                                    ages_no_move = ages_no_move,
                                    ages = lst$ages,
                                    age_names = lst$age_names,
                                    f_space = f_space)

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

  set.seed(rdev_seed)
  if(zero_rdevs){
    r_devs <- rep(0, yr_future)
  }else{
    r_devs <- rnorm(n = yr_future,
                    mean = 0,
                    sd = exp(lst$rdev_sd))
  }

  lst$r_dev <- ss_model$r_dev
  # Add a zero for the final year
  # TODO: Why is a zero assumed here? It is like this in the original code
  last_yr_rdev <- tibble(yr = lst$m_yr, value = 0)
  lst$r_dev <- lst$r_dev %>%
    bind_rows(last_yr_rdev)

  if(yr_future > 0){
    # yrs is the years after lst$m_yrs
    yrs <- (lst$m_yr + 1):(lst$m_yr + yr_future)
    new_df <- tibble(yr = yrs, value = r_devs)
    lst$r_dev <- lst$r_dev %>% bind_rows(new_df)
  }

  lst$init_n <- ss_model$init_n %>%
    as.data.frame() %>%
    as_tibble() %>%
    mutate(age = ages[-which(0 %in% ages)]) %>%
    select(age, everything()) %>%
    rename(value = 2)

  # Selectivity change in that year
  lst$flag_sel <- rep(FALSE, lst$n_yr)
  lst$flag_sel[which(lst$yrs == lst$sel_change_yr):which(lst$yrs == lst$m_yr)] <- TRUE

  lst$catch_country <- ss_model$catch_country %>%
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
      slice(rep(1, each = yr_future)) %>%
      mutate(year = future_yrs)
    lst$catch_country <- lst$catch_country %>%
      bind_rows(means)
  }

  add_wage_yrs <- function(df, row = 1){
    if(lst$n_yr > nrow(df)){
      #if(populate_future){
        yr_one_vals <- df %>%
          slice(rep(row, each = yr_future)) %>%
          mutate(Yr = future_yrs)
      #}else{
      #   yr_one_vals <- df %>%
      #     slice(rep(row, each = yr_future)) %>%
      #     mutate_at(.vars = vars(-Yr), ~{NA}) %>%
      #     mutate(Yr = future_yrs)
      # }
      df <- df %>%
        bind_rows(yr_one_vals)
    }
    df
  }

  lst$wage_catch_df <- add_wage_yrs(lst$wage_catch_df)
  lst$wage_survey_df <- add_wage_yrs(lst$wage_survey_df)
  lst$wage_ssb_df <- add_wage_yrs(lst$wage_ssb_df)
  lst$wage_mid_df <- add_wage_yrs(lst$wage_mid_df)

  lst$b <- ss_model$b

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

  if(yr_future > 1){
    # Assumes survey is in every nth year only into the future
    idx_future <- rep(FALSE, length(future_yrs))
    ind <- 0
    if(future_yrs[1] %% 2){
      # If first future year is odd, set that as a survey year
      ind <- 1
      idx_future[1] <- TRUE
    }
    # Add n_survey years to the indices after the first odd year
    while(ind <= length(future_yrs)){
      ind <- ind + n_survey
      if(ind <= length(future_yrs)){
        idx_future[ind] <- TRUE
      }
    }
    # TODO: Remove these two idx_future lines, it is set up to match the old mse code
    # and overwrites idx_future above. Used to compare to the old code output
    idx_future <- rep(FALSE, length(future_yrs))
    idx_future[seq(2,n_sim_yrs, by = n_survey)] <- TRUE
    # TODO: Remove this, it is set up to match the old mse code

    future_survey_err <- map_dbl(idx_future, ~{
      # if(!populate_future){
      #   return(NA)
      # }
      if(.x) mean(lst$survey_err[lst$survey_err != 1]) else 1
    })
    lst$survey_err <- c(lst$survey_err, future_survey_err)

    future_ss_survey <- map_dbl(idx_future, ~{
      # if(!populate_future){
      #   return(NA)
      # }
      if(.x) mean(lst$ss_survey[lst$ss_survey != -1]) else 0
    })
    lst$ss_survey <- c(lst$ss_survey, future_ss_survey)

    future_flag_survey <- map_dbl(idx_future, ~{
      if(.x) 1 else -1
    })
    lst$flag_survey <- c(lst$flag_survey, future_flag_survey)

    lst$flag_catch <- c(lst$flag_catch, rep(-1, yr_future))
    # Bias adjustment
    lst$b <- c(ss_model$b, rep(lst$b_future, yr_future))
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
#' @param m_yr The last non-future year. Used to de-populate future values if `populate_future` is FALSE
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
                              m_yr = NULL,
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
  # if(!populate_future){
  #   m_ind <- which(yrs == m_yr)
  #   move_mat[, , , (m_ind + 1):n_yr] <- NA
  # }
  list(move_mat = move_mat,
       move_init = move_init,
       f_space = f_space)
}