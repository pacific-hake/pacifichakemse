#' Prepare the data for the operating model
#'
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
#' @param yr_future How many years into the future should there be stochastic values
#' @param sel_hist Use historical selectivity?
#' @param f_space The proportion of TAC given to each country. First value is Canada,
#' the second is the US
#' @param catch_props_space_season Proportion of catch to take by season and space
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
                              b_future = 0.5,
                              yr_future  = 0,
                              sel_change_yr = 1991,
                              sel_hist = TRUE,
                              f_space = c(0.2612, 0.7388),
                              catch_props_space_season = NULL,
                              ...){

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
  verify_argument(b_future, "numeric", 1)
  verify_argument(yr_future, "numeric", 1)
  verify_argument(sel_change_yr, c("numeric", "integer"), 1)
  verify_argument(sel_hist, "logical", 1)
  verify_argument(f_space, "numeric", n_space)
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

  lst <- csv_data(sel_hist)

  if(is.null(move_init)){
    # n_space must be 2 due to error check above
    move_init <-  c(0.25, 0.75)
  }
  names(move_init) <- space_names

  yrs <- s_yr:(m_yr + yr_future)
  n_yr <- length(yrs)
  t_end <- length(yrs) * n_season

  # Age stuff
  n_age <- length(ages)
  m_sel <- rep(1, n_age)

  recruit_mat <- rep(1, n_space)
  names(recruit_mat) <- space_names

  # Maturity
  move_fifty <- move_fifty_init
  move_max <- rep(move_max_init, n_season)
  names(move_max) <- season_names
  # Initialize the movement matrix
  move_mat_obj <- init_movement_mat(n_space,
                                    space_names,
                                    n_season,
                                    season_names,
                                    n_yr,
                                    yrs,
                                    move_max,
                                    move_slope,
                                    move_fifty,
                                    move_south,
                                    move_out,
                                    move_init,
                                    ages_no_move,
                                    ages,
                                    age_names,
                                    f_space)
  move_mat <- move_mat_obj$move_mat
  move_init <- move_mat_obj$move_init
  f_space <- move_mat_obj$f_space
  names(f_space) <- space_names

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
  # Set up age comps
  age_survey_df <- lst$age_survey_df %>%
    mutate(flag = 1)
  age_catch_df <- lst$age_catch_df %>%
    mutate(flag = 1)
  # Set up survey season
  if(n_season == 1){
    survey_season <-  1
  }else if(n_season == 4){
    survey_season <- 3
  }else{
    survey_season <- floor(n_season / 2)
  }
  # Set up selectivity
  p_sel <- lst$p_sel
  if(!sel_hist){
    n_surv_yrs <- nrow(lst$parms_sel %>% filter(source == "survey")) + 1
    n_sel_yrs <- sum(sel_change_yr <= yrs)
    p_sel <- matrix(0, n_surv_yrs, n_sel_yrs)
  }

  if(n_season == 4 & n_space == 2){
    # Standardize row values to equal 1
    catch_props_space_season <- map(catch_props_space_season, ~{
      .x / sum(.x)
    }) %>%
      set_names(seq_along(.)) %>%
      bind_rows() %>%
      t()
  }else{
    catch_props_space_season <- matrix(NA, n_space, n_season)
    # Equally distributed catch
    catch_props_space_season[1:n_space,] <- 1 / n_season
  }
  r_mul <- ifelse(n_space == 2, 1.1, 1)

  # Just start all the simulations with the same initial conditions
  lst$r_dev <- lst$r_dev %>%
    as.data.frame() %>%
    mutate(yr = yrs) %>%
    select(yr, everything())
  lst$init_n <- lst$init_n %>%
    as.data.frame() %>%
    mutate(age = ages[-which(ages_no_move %in% ages)]) %>%
    select(age, everything()) %>%
    rename(val = 2)
  parms_init <- list(log_r_init = lst$parms_scalar$logRinit + log(r_mul),
                     log_h = lst$parms_scalar$logh,
                     log_m_init = lst$parms_scalar$logMinit,
                     log_sd_surv = lst$parms_scalar$logSDsurv,
                     log_phi_catch = lst$parms_scalar$logphi_catch,
                     # Selectivity parameters
                     p_sel_fish = lst$parms_sel %>% filter(source == "fish"),
                     p_sel_surv = lst$parms_sel %>% filter(source == "survey"),
                     init_n = lst$init_n,
                     r_in = lst$r_dev,
                     p_sel = p_sel)

  # USA selectivity
  # psel[i,] <- c(2.8476, 0.973, 0.3861, 0.1775, 0.5048)
  d_sel <- matrix(NA, 5, n_space)
  d_sel <- map(seq_len(ncol(d_sel)), ~{
     d_sel[,.x] = parms_init$p_sel_fish$value
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
            move = ifelse(n_space == 1, FALSE, TRUE),
            move_mat = move_mat,
            move_init = move_init,
            move_fifty = move_fifty,
            move_max = move_max,
            move_south = move_south,
            move_out = move_out,
            move_slope = move_slope,
            catch_props_space_season = catch_props_space_season,
            catch = lst$catch,
            p_sel = p_sel,
            f_space = f_space)

  df$catch_country <- lst$catch_country %>%
    select(year, Can, US) %>%
    mutate(total = rowSums(.)) %>% set_names(c("year", "space1", "space2", "total"))
  df$catch <- df$catch_country$total
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
  # Add names for dimensions
  df$space_names <- space_names
  df$season_names <- season_names
  df$age_names <- age_names
  df
}

#' Initialize the movement model matrix. An alternative function should be
#' written if changes are required to the initialization assumptions
#'
#' @param n_space See [load_data_seasons()]
#' @param space_names See [load_data_seasons()]
#' @param n_season See [load_data_seasons()]
#' @param season_names See [load_data_seasons()]
#' @param n_yr The number of years
#' @param yrs A vector of names for the years. Length must equal `n_yr`
#' @param move_max A vector of the maximum movement rate, one for each of `n_seasons`
#' @param move_slope  See [load_data_seasons()]
#' @param move_fifty Age at 50 percent movement rate
#' @param move_south  See [load_data_seasons()]
#' @param move_out  See [load_data_seasons()]
#' @param move_init  See [load_data_seasons()]
#' @param ages_no_move See [load_data_seasons()]
#' @param ages See [load_data_seasons()]
#' @param age_names See [load_data_seasons()]
#' @param f_space See [load_data_seasons()]
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