#' Initialize values used in the age based model
#'
#' @param om A [list] as returned by [load_data_om()]
#'
#' @return A [list] with the same structure as `om`, but with a new element,
#' `n_init` added, and some values changed for the initialization of the model
#' @export
init_agebased_model <- function(om = NULL){

  verify_argument(om, "list")

  # Natural mortality - males = females
  om$m_sel <- om$m_sel
  om$m0 <- exp(om$parameters$log_m_init)
  om$m_age <- om$m0 * om$m_sel
  om$m_cumu_age <- c(0, cumsum(om$m_age[1:(om$n_age - 1)]))
  # Recruitment
  om$r0 <- exp(om$parameters$log_r_init)
  om$rdev_sd <- exp(om$rdev_sd)
  om$r0_space <- om$r0 * om$move_init
  # Survey selectivity - constant over time

  om$surv_sel <- get_select(om$ages,
                             om$parameters$p_sel_surv,
                             om$s_min_survey,
                             om$s_max_survey)

  om$surv_sd <- exp(om$parameters$log_sd_surv) # Survey error
  # Catchability -  constant over time
  om$q <- exp(om$log_q)
  # Maturity and fecundity
  om$mat_sel <- om$mat_sel
  om$h <- exp(om$parameters$log_h)
  # Numbers-at-age - calculate n0 based on r0
  # Extrapolate the N0 out to three times the max age for sum for max age
  ages_3 <- min(om$ages):(om$n_age * 3)
  n_age_3 <- length(ages_3)

  n0_tmp <- NULL
  n0_tmp[1:(n_age_3 - 1)] = om$r0 * exp(-ages_3[1:(n_age_3 - 1)] * om$m0)
  n0_tmp[n_age_3] =  om$r0 * exp(-om$m0 * ages_3[n_age_3]) / (1 - exp(-om$m0))

  om$n0 <- matrix(NA, om$n_age)
  om$n0[1:(om$n_age - 1)] <- n0_tmp[1:(om$n_age - 1)]
  om$n0[om$n_age] <- sum(n0_tmp[om$n_age:n_age_3])

  # Weight-at-age
  om$wage_ssb <- get_age_dat(om$wage_ssb_df, om$s_yr)
  om$wage_survey <- get_age_dat(om$wage_survey_df, om$s_yr)
  # Initial biomass
  om$ssb_0 <- map_dbl(seq_len(om$n_space), ~{
    sum(om$n0 * om$move_init[.x] * om$wage_ssb) * 0.5
  }) %>% set_names(paste0(rep("space", each = om$n_space), seq_len(om$n_space)))

  # Set m-at-age for year 1, space 1, season 1
  om$z_save[, 1, 1, 1] <- om$m_age
  # Assumed no fishing before data started
  # An alternate way of doing it, where the year column is used
  #  instead of the first column:
  # om$catch_age <- om$catch_age %>%
  #   as.data.frame() %>%
  #   mutate(!!sym(as.character(om$s_yr)) := m)
  # Set catch and catch-at-age for first year to 0
  om$catch_age[, 1] <- 0
  om$catch[1] <- 0
  om$catch_n[1] <- 0
  om$catch_n_age[, 1] <- 0
  # Set first survey year to 1, surveys start later
  om$survey[1] <- 1
  # Distribute over space
  om$n_init <- rep(NA, om$n_age)
  om$n_init_dev <- om$parameters$init_n
  om$age_1_ind <- which(om$ages == 1)

  # Initial numbers-at-age for all older than age 0
  om$n_init[om$age_1_ind:(om$n_age - 1)] <- om$r0 *
    exp(-om$m_cumu_age[om$age_1_ind:(om$n_age - 1)]) *
    exp(-0.5 * om$rdev_sd ^ 2 * 0 + om$n_init_dev[1:(om$n_age - 2),]$value)

  # Plus group (ignore recruitment devs in first yrs )
  om$n_init[om$n_age] <- om$r0 *
    exp(-(om$m_age[om$n_age] * om$ages[om$n_age])) / (1 - exp(-om$m_age[om$n_age])) *
    exp(-0.5 * om$rdev_sd ^ 2 * 0 + om$n_init_dev[om$n_age - 1,]$value)

  for(space in seq_len(om$n_space)){
    # Initialize only
    # Set numbers-at-age for year 1, space, season 1
    om$n_save_age[, 1, space, 1] <- om$n_init * om$move_init[space]
    # Set mid-year numbers-at-age by equally partitioning M across
    #  seasons and dividing by 2
    om$n_save_age_mid[, 1, space, 1] <- om$n_save_age[, 1, space, 1] *
      exp(-0.5 * (om$m_age / om$n_season))
    # Survey value in the first year will be NA because surveys
    # don't start until later years
    om$survey_true[space, 1] <- sum(om$n_save_age[, 1, space, om$survey_season] *
                                       om$surv_sel * om$q * om$wage_survey)
  }

  om
}