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
#' @param yr_future Years to go into the future
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
#' @param yr_future How many years into the future should there be stochastic values
#' @param sel_hist Use historical selectivity?
#' @param f_space The proportion of TAC given to each country. First value is Canada,
#' the second is the US
#' @param log_phi_survey Survey phi parameter value
#' @param catch_props_space_season Proportion of catch to take by season and space
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
                         season_names = NULL,
                         n_space = 2,
                         space_names = NULL,
                         n_survey = 2,
                         s_yr = 1966,
                         m_yr = 2018,
                         ages = 0:20,
                         age_names = NULL,
                         rdev_sd = 1.4,
                         move_init = NULL,
                         move_max_init = 0.35,
                         move_fifty_init = 6,
                         move_out = 0.85,
                         move_south = 0.05,
                         move_slope = 0.9,
                         ages_no_move = c(0, 1),
                         selectivity_change = 0,
                         s_min = 1,
                         s_max = 6,
                         s_min_survey = 2,
                         s_max_survey = 6,
                         b_future = 0.5,
                         yr_future = 0,
                         sel_change_yr = 1991,
                         sel_hist = TRUE,
                         f_space = c(0.2612, 0.7388),
                         log_phi_survey = log(11.46),
                         catch_props_space_season = NULL,
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
  verify_argument(selectivity_change, "numeric", 1)
  verify_argument(s_min, c("numeric", "integer"), 1)
  verify_argument(s_max, c("numeric", "integer"), 1)
  verify_argument(s_min_survey, c("numeric", "integer"), 1)
  verify_argument(s_max_survey, c("numeric", "integer"), 1)
  verify_argument(b_future, "numeric", 1)
  verify_argument(yr_future, "numeric", 1)
  verify_argument(sel_change_yr, c("numeric", "integer"), 1)
  verify_argument(sel_hist, "logical", 1)
  verify_argument(f_space, "numeric", n_space)
  verify_argument(log_phi_survey, "numeric", 1)
  if(!is.null(catch_props_space_season)){
    verify_argument(catch_props_space_season, "list", n_space)
    map(catch_props_space_season, ~{
      verify_argument(.x, "numeric", n_season)
    })
    if(!(n_space == 2 && n_season == 4)){
      stop("`catch_props_space_season` must be NULL unless `n_season` = 4 and `n_space` = 2",
           call. = FALSE)
    }
  }

  # Throw error if yr_future is exactly 1
  stopifnot(yr_future == 0 | yr_future > 1)
  # Throw error if move_init is NULL and n_space is not 2
  stopifnot(!is.null(move_init) | (is.null(move_init) & n_space == 2))

  lst <- csv_data()

  lst$yrs <- s_yr:(m_yr + yr_future)
  lst$s_yr <- s_yr
  lst$m_yr <- m_yr
  lst$n_yr <- length(lst$yrs)
  lst$sel_change_yr <- sel_change_yr
  lst$sel_idx <- which(lst$yrs == lst$sel_change_yr)
  lst$yr_sel <- length(lst$sel_change_yr:max(lst$yrs))
  lst$n_season <- n_season
  lst$t_end <- lst$n_yr * lst$n_season
  lst$season_names <- season_names
  lst$n_space <- n_space
  lst$space_names <- space_names
  lst$ages <- ages
  lst$age_names <- age_names
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

  # Initialize the movement matrix
  # Add the sim yrs in so that we don't have to redimension the array later. This makes it much faster
  # and the code cleaner in the MSE loop later
  yrs_all <- c(lst$yrs, (lst$yrs[length(lst$yrs)] + 1):(lst$yrs[length(lst$yrs)] + n_sim_yrs))
  move_mat_obj <- init_movement_mat(lst$n_space,
                                    lst$space_names,
                                    lst$n_season,
                                    lst$season_names,
                                    lst$n_yr + n_sim_yrs,
                                    yrs_all,
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
    # Standardize row values to equal 1
    lst$catch_props_space_season <- map(catch_props_space_season, ~{
      .x / sum(.x)
    }) %>%
      set_names(seq_along(.)) %>%
      bind_rows() %>%
      t()
  }else{
    lst$catch_props_space_season <- matrix(NA, lst$n_space, lst$n_season)
    # Equally distributed catch
    lst$catch_props_space_season[1:lst$n_space,] <- 1 / lst$n_season
  }
  lst$r_mul <- ifelse(lst$n_space == 2, 1.1, 1)

  # Just start all the simulations with the same initial conditions
  lst$r_dev <- lst$r_dev %>%
    as.data.frame() %>%
    mutate(yr = lst$yrs) %>%
    select(yr, everything())
  lst$init_n <- ss_model$init_n %>%
    as.data.frame() %>%
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
  lst$catch_obs <- ss_model$catch_obs
  if(lst$n_yr > nrow(lst$catch_obs)){
    lst$catch_obs <- c(lst$catch_obs, rep(mean(lst$catch_obs), lst$n_yr - length(lst$catch_obs)))
  }

  if(lst$n_yr > nrow(lst$catch_country)){
    means <- lst$catch_country %>%
      summarize_all(mean)
    lst$catch_country <- lst$catch_country %>%
      bind_rows(means)
  }
  lst$b_future <- b_future

  # if(yr_future > 1){
  #   # TODO: Need to debug this, make sure it works
  #   # lst$yrs where survey occurs
  #   idx_future <- length(s_yr:m_yr) + seq(2, yr_future, by = lst$n_survey)
  #   #lst$survey_x <- c(lst$survey_x, rep(-2, yr_future))
  #   #lst$survey_x[idx_future] <- 2
  #   lst$survey_err <- c(lst$survey_err, rep(1, yr_future))
  #   lst$survey_err[idx_future] <- mean(lst$survey_err[lst$survey_err != 1])
  #   lst$ss_survey <- c(lst$ss_survey, rep(0,  yr_future))
  #   lst$ss_survey[idx_future] <- mean(lst$ss_survey[lst$ss_survey != -1])
  #   lst$flag_survey <- c(lst$flag_survey, rep(-1, yr_future))
  #   lst$flag_survey[idx_future] <- 1
  #   lst$flag_catch[lst$yrs > m_yr] <- 1
  #   r_devs <- rnorm(n = yr_future, mean = 0, sd = exp(lst$rdev_sd))
  #   lst$parameters$r_in <- c(lst$parameters$r_in, r_devs)
  #   # Bias adjustment
  #   lst$b <- c(lst$b, rep(lst$b_future, yr_future))
  # }

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
  verify_argument(yrs, c("numeric", "integer"), n_yr)
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