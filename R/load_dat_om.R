#' Prepare the data for the operating model
#'
#' @param ss_model A model input/output list representing the SS model as
#' found in the RDS file created by  [create_rds_file()]
#' @param yr_future Number of years to run the OM into the future in an OM
#' only run. If this is > 0, `n_sim_yrs` cannot be. This must be 0 when
#' running from an MSE context. Use `n_sim_yrs` for that instead.
#' @param n_sim_yrs Number of years to allocate OM objects for the future.
#' If this is > 0, `yr_future` cannot be. All future values will be `NA`
#' except for the movement matrix values.
#' @param n_season Number of seasons
#' @param season_names A vector of names for the seasons. Length must
#' equal `n_season`
#' @param n_space Number of spatial areas
#' @param space_names A vector of names for the spaces. Length must
#' equal `n_space`
#' @param ages A vector of ages
#' @param age_plus_grp Plus group for ages. Required for
#' [setup_blank_om_objects()]
#' @param age_names A vector of names for the ages. Length must equal
#' length of `ages`
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
#' @param f_space The proportion of TAC given to each country. First value
#' is Canada, the second is the US
#' @param log_phi_survey Survey phi parameter value
#' If FALSE, they will be given random normal values based on rdev_sd
#' @param random_recruitment Logical. If `FALSE`, recruitment deviations
#' will be set to zero in the projection period. If `TRUE`, recruitment
#' deviations will be set to random values in the projection period

#' @param ... Absorb arguments destined for other functions
#'
#' @return A list of Parameters, Input parameters, Survey, Catch, and others
#'
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
                         season_names = c("Season1",
                                          "Season2",
                                          "Season3",
                                          "Season4"),
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
  stopifnot(yr_future >= 0 && yr_future != 1)
  # Throw error if move_init is NULL and n_space is not 2
  stopifnot(!is.null(move_init) || (is.null(move_init) && n_space == 2))

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
  if(lst$n_season == 4 && lst$n_space == 2){
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

  # `catch` differs from catch_obs: catch will contain the mean values for
  #  all years into the future. Those will be used in the catch-age
  #  calculations. `catch-obs` will contain catch calculated from the
  #  estimated fishing mortality from the EM for future years and used as
  #  removals in the MSE. `catch` and `catch_obs` will contain the same
  #  values for the years extracted from the SS model in this loading phase.
  # lst$catch <- lst$catch_obs

  lst$catch_country <- ss_model$catch_country |>
    mutate(total = can + usa)
  names(lst$catch_country) <- c("year", "space1", "space2", "total")

  # If n_yr greater than the number of catch observations, append the mean
  #  catch across time series to the end lst$yrs
  if(lst$n_yr > nrow(lst$catch_country)){
    mean_catch <- apply(lst$catch_country[, -1], 2, mean)
    new_df <- tibble(year = lst$future_yrs,
                     space1 = mean_catch["space1"],
                     space2 = mean_catch["space2"],
                     total = mean_catch["total"])

    if(!populate_future){
      new_df[ ,-1] <- NA
    }
    lst$catch_country <- lst$catch_country |>
      bind_rows(new_df)
  }
  lst$catch_country <- lst$catch_country |>
    as.matrix()

  # Weight-at-age in OM -------------------------------------------------------
  lst$wage_catch_df <- ss_model$wage_catch_df
  lst$wage_survey_df <- ss_model$wage_survey_df
  lst$wage_ssb_df <- ss_model$wage_ssb_df
  lst$wage_mid_df <- ss_model$wage_mid_df

  # Add `lst$n_future_yrs` new rows to the data frame, which are copies of
  #  row `row`. It is assumed the column `Yr` exists and `lst$future_yrs`
  #  will be used as the names for the new years in the appended rows.
  add_yrs <- function(df, row = 1){

    if(lst$n_yr > nrow(df)){
      new_df <- as.matrix(df[row, ])
      new_df <- new_df[rep(1, times = lst$n_future_yrs), ]
      new_df[, 1] <- lst$future_yrs
      new_df <- new_df |>
        as_tibble()
      if(!populate_future){
        new_df[, -1] <- NA
      }
      df <- df |>
        bind_rows(new_df)
    }
    df |>
      as.matrix()
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
  # It is assumed in the MSE code that all values in it have been
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
  }

  lst$r_dev <- ss_model$r_dev

  # Add the future years to the recruitment deviations
  yr_min <- min(lst$r_dev$yr)
  yr_max <- max(lst$future_yrs)
  lst$r_dev <- lst$r_dev |>
    complete(yr = yr_min:yr_max)

  # Initial Numbers-at-age in OM ----------------------------------------------
  lst$init_n <- tibble(age = ages[ages != 0],
                       value = ss_model$init_n[names(ss_model$init_n) != "0"]) |>
    as.matrix()

  # Parameters to initialize the OM -------------------------------------------
  lst$parameters <- list(log_r_init = exp(ss_model$mcmccalcs$rinit["50%"]) +
                           log(lst$r_mul),
                         log_h = log(ss_model$mcmccalcs$steep["50%"]),
                         log_m_init = log(ss_model$mcmccalcs$m["50%"]),
                         log_sd_surv = log(ss_model$mcmccalcs$survey_sd["50%"]),
                         log_phi_survey = ss_model$mcmccalcs$dm_survey["50%"],
                         log_phi_catch = ss_model$mcmccalcs$dm_fishery["50%"],
                         p_sel_fish = ss_model$p_sel_fish,
                         p_sel_surv = ss_model$p_sel_surv,
                         init_n = lst$init_n,
                         r_in = lst$r_dev)

  # Future year modifications -------------------------------------------------
  if(lst$n_future_yrs > 1){
    # Assumes survey is in every nth year (`n_survey`) into the future
    idx_future <- as.logical(lst$future_yrs %% n_survey)

    mean_survey_err <- mean(lst$survey_err[lst$survey_err != 1])
    future_survey_err <- map_dbl(idx_future, ~{
      ifelse(.x, mean_survey_err, 1)
    })
    lst$survey_err <- c(lst$survey_err, future_survey_err)

    mean_survey <- mean(lst$ss_survey[lst$ss_survey > 0])
    future_ss_survey <- map_dbl(idx_future, ~{
      ifelse(.x, mean_survey, 0)
    })

    lst$ss_survey <- c(lst$ss_survey, future_ss_survey)

    future_flag_survey <- map_dbl(idx_future, ~{
      ifelse(.x, 1, -1)
    })
    lst$flag_survey <- c(lst$flag_survey, future_flag_survey)
    lst$flag_catch <- c(lst$flag_catch, rep(-1, lst$n_future_yrs))
    lst$b <- c(ss_model$b, rep(lst$b_future, lst$n_future_yrs))
  }

  lst
}
