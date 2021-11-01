#' Prepare the data for the operating model
#'
#' @param ss_model A model input/output list representing the SS model as as found in
#' the RDS file created by  [create_rds_file()]
#' @param yr_future Number of years to run the OM into the future in an OM-only run.
#' If this is > 0, `n_sim_yrs` cannot be. This must be 0 when running from an MSE context.
#' Use `n_sim_yrs` for that instead.
#' @param n_sim_yrs Number of years to allocate OM objects for the future. If this is > 0,
#' `yr_future` cannot be. All future values will be `NA` except for the movement matrix values.
#' @param n_season Number of seasons
#' @param season_names A vector of names for the seasons. Length must equal `n_season`
#' @param n_space Number of spatial areas
#' @param space_names A vector of names for the spaces. Length must equal `n_space`
#' @param ages A vector of ages
#' @param age_plus_grp Plus group for ages. Required for [setup_blank_om_objects()]
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
#' @param f_space The proportion of TAC given to each country. First value is Canada,
#' the second is the US
#' @param log_phi_survey Survey phi parameter value
#' If FALSE, they will be given random normal values based on rdev_sd
#' @param random_recruitment Logical. If `FALSE`, recruitment deviations will be
#' set to zero in the projection period. If `TRUE`, recruitment deviations will be set
#' to random values in the projection period

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
                         yr_future = 0,
                         n_sim_yrs = 0,
                         n_season = 4,
                         season_names = c("Season1", "Season2", "Season3", "Season4"),
                         n_space = 2,
                         space_names = c("Canada", "US"),
                         n_survey = 2,
                         ages = 0:20,
                         age_plus_grp = 15,
                         age_names = paste("age", 0:20),
                         rdev_sd = 1.4,
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
                         random_recruitment = TRUE,
                         ...){

  # Throw error if the number of simulation years is exactly 1
  stopifnot(yr_future >= 0 & yr_future != 1)
  # Throw error if move_init is NULL and n_space is not 2
  stopifnot(!is.null(move_init) | (is.null(move_init) & n_space == 2))

  # Only one of n_sim_yrs or yr_future can be used
  if(n_sim_yrs > 0 & yr_future > 0){
    stop("Only one of n_sim_yrs or yr_future can be used.", call. = FALSE)
  }

  lst <- NULL
  # Only populate the future years if the `yr_future` is >0 which means this
  # is going to be run in OM-only model
  populate_future <- yr_future > 0

  # Years in OM ---------------------------------------------------------------
  lst$s_yr <- ss_model$s_yr
  lst$m_yr <- ss_model$m_yr
  if(yr_future == 0){
    lst$n_future_yrs <- n_sim_yrs
  }else{
    lst$n_future_yrs <- yr_future
  }
  lst$future_yrs <- (lst$m_yr + 1):(lst$m_yr + lst$n_future_yrs)
  lst$yrs <- lst$s_yr:(lst$m_yr + lst$n_future_yrs)
  lst$n_sim_yrs <- n_sim_yrs
  lst$yr_future <- yr_future
  lst$n_yr <- length(lst$yrs)

  # Seasons and Space in OM ---------------------------------------------------
  lst$n_season <- n_season
  lst$season_names <- season_names
  lst$n_space <- n_space
  lst$space_names <- space_names
  lst$t_end <- lst$n_yr * lst$n_season

  # Ages in OM ----------------------------------------------------------------
  lst$ages <- ages
  lst$age_names <- age_names
  lst$age_max_age <- nrow(ss_model$age_survey)
  lst$s_min <- s_min
  lst$s_max <- s_max
  lst$s_min_survey <- s_min_survey
  lst$s_max_survey <- s_max_survey
  lst$n_age <- length(ages)
  lst$m_sel <- rep(1, lst$n_age)

  # Selectivity in OM ---------------------------------------------------------
  lst$sel_change_yr <- sel_change_yr
  lst$sel_idx <- which(lst$yrs == lst$sel_change_yr)
  lst$yr_sel <- length(lst$sel_change_yr:lst$m_yr)
  lst$selectivity_change <- selectivity_change
  lst$sel_by_yrs <- ss_model$sel_by_yrs
  # Selectivity change in that year
  lst$flag_sel <- rep(FALSE, lst$n_yr)
  lst$flag_sel[which(lst$yrs == lst$sel_change_yr):which(lst$yrs == lst$m_yr)] <- TRUE

  # Maturity in OM ------------------------------------------------------------
  lst$mat_sel <- ss_model$mat_sel
  lst$recruit_mat <- rep(1, lst$n_space)
  names(lst$recruit_mat) <- lst$space_names

  # Parameters in OM ----------------------------------------------------------
  lst$rdev_sd <- log(rdev_sd)
  lst$log_q <- log(1.14135)
  lst$log_sd_catch <- log(0.01)
  lst$s_mul <- 0.5
  lst$sigma_p_sel <- 1.4
  lst$sum_zero <- 0
  lst$move <- lst$n_space > 1
  lst$log_phi_survey <- log_phi_survey
  if(lst$n_space > 1){
    lst$r_mul <- 1.1
  }else{
    lst$r_mul <- 1
  }

  # Bias adjustments in OM ----------------------------------------------------
  lst$b <- ss_model$b
  lst$b_future <- b_future

  # Survey in OM --------------------------------------------------------------
  lst$n_survey <- n_survey
  lst$survey <- ss_model$survey
  lst$survey_err <- ss_model$survey_err
  # TODO: Remove these hardwired values. They are here to match the former MSE
  # code. These are only good for 2018 and it will break if you try 2020.
  # lst$survey_err <- c(rep(1, 29),
  #                     0.08929600000000000037,
  #                     rep(1, 2),
  #                     0.05259600000000010100,
  #                     rep(1, 2),
  #                     0.10589600000000000402,
  #                     1,
  #                     0.06419599999999990592,
  #                     1,
  #                     0.06379600000000000548,
  #                     1,
  #                     0.07659599999999990028,
  #                     1,
  #                     0.09949600000000000111,
  #                     1,
  #                     0.11769599999999999507,
  #                     0.06729599999999999471,
  #                     0.06459600000000009778,
  #                     1,
  #                     0.08289600000000009461,
  #                     1,
  #                     0.06319600000000000217,
  #                     1)
  lst$ss_survey <- ss_model$ss_survey
  lst$flag_survey <- ss_model$flag_survey
  lst$age_survey <- ss_model$age_survey
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
    # TODO: This hasn't been tested
    lst$catch_props_space_season <- matrix(NA, lst$n_space, lst$n_season)
    # Equally distributed catch
    lst$catch_props_space_season[1:lst$n_space,] <- 1 / lst$n_season
  }

  # Catch in OM ---------------------------------------------------------------
  lst$flag_catch <- ss_model$flag_catch
  lst$age_catch <- ss_model$age_catch
  lst$ss_catch <- ss_model$ss_catch
  lst$catch_obs <- ss_model$catch_obs

  #n_row <- dim(lst$catch_obs)[1]
  if(lst$n_yr > nrow(lst$catch_obs)){
    if(populate_future){
      new_val <- mean(lst$catch_obs[, "value"])
    }else{
      new_val <- NA
    }
    new_mat <- matrix(lst$future_yrs, ncol = 1)
    new_mat <- cbind(new_mat, rep(new_val, lst$n_yr - nrow(lst$catch_obs)))
    lst$catch_obs <- rbind(lst$catch_obs, new_mat)
  }

  # `catch` differs from catch_obs: catch will contain the mean values for all years into the future.
  # Those will be used in the catch-age calculations. `catch-obs` will contain catch calculated from the
  # estimated fishing mortality from the EM for future years and used as removals in the MSE.
  # `catch` and `catch_obs` will contain the same values for the years extracted from the SS model in this
  # loading phase.
  #lst$catch <- lst$catch_obs

  lst$catch_country <- ss_model$catch_country %>% cbind(rowSums(ss_model$catch_country[, -1]))
  colnames(lst$catch_country) <- c("year", "space1", "space2", "total")

  # TODO: Check why the sum of the catch country file does not add up to the total in the SS data file
  # lst$catch_obs <- lst$catch_country %>% pull(total)
  # If n_yr greater than the number of catch observations, append the mean catch across
  # time series to the end lst$yrs
  if(lst$n_yr > nrow(lst$catch_country)){
    mean_catch <- apply(lst$catch_country[, -1], 2, mean)
    new_df <- matrix(mean_catch, nrow = 1)
    new_df <- new_df[rep(1, times = lst$n_future_yrs), ]
    new_df <- cbind(lst$future_yrs, new_df)
    colnames(new_df) <- c("year", "space1", "space2", "total")

    if(!populate_future){
      new_df[,-1] <- NA
    }
    lst$catch_country <- rbind(lst$catch_country, new_df)
  }

  # Weight-at-age in OM -------------------------------------------------------
  lst$wage_catch_df <- ss_model$wage_catch_df
  lst$wage_survey_df <- ss_model$wage_survey_df
  lst$wage_ssb_df <- ss_model$wage_ssb_df
  lst$wage_mid_df <- ss_model$wage_mid_df

  # Add `lst$n_future_yrs` new rows to the data frame, which are copies of row `row`.
  # It is assumed the column `Yr` exists and `lst$future_yrs` will be used as the names
  # for the new years in the appended rows.
  add_yrs <- function(df, row = 1){
    if(lst$n_yr > nrow(df)){
      new_df <- as.matrix(df[row, ])
      new_df <- new_df[rep(1, times = lst$n_future_yrs), ]
      new_df[, 1] <- lst$future_yrs
      new_df <- as_tibble(new_df)
      if(!populate_future){
        new_df[, -1] <- NA
      }
      df <- df %>%
        bind_rows(new_df)
    }
    df %>% as.matrix()
  }
  lst$wage_catch_df <- add_yrs(lst$wage_catch_df)
  lst$wage_survey_df <- add_yrs(lst$wage_survey_df)
  lst$wage_ssb_df <- add_yrs(lst$wage_ssb_df)
  lst$wage_mid_df <- add_yrs(lst$wage_mid_df)

  # Create empty objects for OM------------------------------------------------
  lst <- append(lst,
                setup_blank_om_objects(yrs = lst$yrs,
                                       ages = lst$ages,
                                       age_plus_grp = age_plus_grp,
                                       max_surv_age = lst$age_max_age,
                                       n_space = lst$n_space,
                                       n_season = lst$n_season))

  # Movement in OM ------------------------------------------------------------
  lst$move_init <- move_init
  if(is.null(lst$move_init)){
    # Note: lst$n_space must be 2 due to error check above. This assumes
    # space == 2
    lst$move_init <- c(0.25, 0.75)
  }
  names(lst$move_init) <- lst$space_names
  lst$move_fifty <- move_fifty_init
  lst$move_max <- rep(move_max_init, lst$n_season)
  names(lst$move_max) <- lst$season_names
  lst$move_south <- move_south
  lst$move_out <- move_out
  lst$move_slope <- move_slope

  # Movement matrix will be initialized with values even if yr_future == 0,
  # It is assumed int he MSE code that all values in it have been
  # initialized previously
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


  # Assign the proportion of F allocated to each space into the movement
  # model object
  lst$f_space <- move_mat_obj$f_space
  names(lst$f_space) <- lst$space_names

  # Recdevs in future years of OM ---------------------------------------------
  r_devs <- rep(NA, lst$n_future_yrs)
  if(populate_future){
    if(random_recruitment){
      r_devs <- rnorm(n = lst$n_future_yrs,
                      mean = 0,
                      sd = exp(lst$rdev_sd))
    }else{
      r_devs <- rep(0, lst$n_future_yrs)
    }
    # rds <- readRDS("rec_devs.rds")
    # r_devs <- c(rds[[iter]], 1.1, 0.02)
  }

  lst$r_dev <- ss_model$r_dev
  # Add a zero for the final year
  # TODO: Why is a zero assumed here? It is like this in the original code so
  # added here
  last_yr_rdev <- matrix(c(lst$m_yr, 0), nrow = 1)
  colnames(last_yr_rdev) <- c("yr", "value")
  lst$r_dev <- rbind(lst$r_dev, last_yr_rdev)
  rownames(lst$r_dev) <- NULL

  # future_yrs is declared above
  new_mat <- matrix(lst$future_yrs, ncol = 1)
  if(populate_future){
    new_mat <- cbind(new_mat, r_devs)
  }else{
    new_mat <- cbind(new_mat, rep(NA, length(lst$future_yrs)))
  }
  colnames(new_mat) <- c("yr", "value")
  lst$r_dev <- rbind(lst$r_dev, new_mat)

  # Initial Numbers-at-age in OM ----------------------------------------------
  lst$init_n <- matrix(ages[-which(0 %in% ages)], ncol = 1)
  lst$init_n <- cbind(lst$init_n, ss_model$init_n)
  colnames(lst$init_n) <- c("age", "value")

  # Parameters to initialize the OM -------------------------------------------
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

  # Future year modifications -------------------------------------------------
  if(lst$n_future_yrs > 1){
    # Assumes survey is in every nth year only into the future
    idx_future <- rep(FALSE, length(lst$future_yrs))
    ind <- 0
    if(lst$future_yrs[1] %% 2){
      # If first future year is odd, set that as a survey year
      ind <- 1
      idx_future[1] <- TRUE
    }
    # Add n_survey years to the indices after the first odd year
    while(ind <= length(lst$future_yrs)){
      ind <- ind + n_survey
      if(ind <= length(lst$future_yrs)){
        idx_future[ind] <- TRUE
      }
    }
    # TODO: Remove these two idx_future lines, it is set up to match the old mse code
    # and overwrites idx_future calculated above. Used to compare to the old code output
    idx_future <- rep(TRUE, length(lst$future_yrs))
    idx_future[seq(2, lst$n_future_yrs, by = n_survey)] <- FALSE

    future_survey_err <- map_dbl(idx_future, ~{
      #ifelse(populate_future, if(.x) mean(lst$survey_err[lst$survey_err != 1]) else 1, NA)
      if(.x) mean(lst$survey_err[lst$survey_err != 1]) else 1
    })
    lst$survey_err <- c(lst$survey_err, future_survey_err)

    future_ss_survey <- map_dbl(idx_future, ~{
      #ifelse(populate_future, if(.x) mean(lst$ss_survey[lst$ss_survey != -1]) else 0, NA)
      if(.x) mean(lst$ss_survey[lst$ss_survey != -1]) else 0
    })
    lst$ss_survey <- c(lst$ss_survey, future_ss_survey)

    future_flag_survey <- map_dbl(idx_future, ~{
      #ifelse(populate_future, if(.x) 1 else -1, NA)
      if(.x) 1 else -1
    })
    lst$flag_survey <- c(lst$flag_survey, future_flag_survey)
    lst$flag_catch <- c(lst$flag_catch, rep(-1, lst$n_future_yrs))
    # These lines remove the zero at the end and replace it with b_future
    # This was testing to try and make it the same as the old code but broke all tests
    # b_tmp <- ss_model$b
    # b_tmp[length(b_tmp)] <- b_future
    # lst$b <- c(b_tmp, rep(lst$b_future, lst$n_future_yrs))
    lst$b <- c(ss_model$b, rep(lst$b_future, lst$n_future_yrs))
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