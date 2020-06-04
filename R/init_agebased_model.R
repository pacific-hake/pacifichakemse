#' Initialize values used in the age based model
#'
#' @param df A [list] as returned by [load_data_seasons()]
#' @param lst A [list] as returned by [setup_blank_om_objects()]
#'
#' @return A [list] with the same structure os `lst`, but with a new element,
#' `n_init` added, and some values chaged for the initializeation of the model
#' @export
init_agebased_model <- function(df = NULL,
                                lst = NULL){
  verify_argument(df, "list")
  verify_argument(lst, "list")

  # Natural mortality - males = females
  lst$m_sel <- df$m_sel
  lst$m0 <- exp(df$parms_init$log_m_init)
  lst$m_age <- lst$m0 * lst$m_sel
  lst$m_cumu_age <- c(0, cumsum(lst$m_age[1:(df$n_age - 1)]))
  # Recruitment
  lst$r0 <- exp(df$parms_init$log_r_init)
  lst$rdev_sd <- exp(df$rdev_sd)
  lst$r0_space <- lst$r0 * df$move_init
  # Survey selectivity - constant over time
  lst$surv_sel <- get_select(df$ages,
                             df$parms_init$p_sel_surv,
                             df$s_min_survey,
                             df$s_max_survey)
  lst$surv_sd <- exp(df$parms_init$log_sd_surv) # Survey error
  # Catchability -  constant over time
  lst$q <- exp(df$log_q)
  # Maturity and fecundity
  lst$mat_sel <- df$mat_sel
  lst$h <- exp(df$parms_init$log_h)
  # Numbers-at-age - calculate n0 based on r0
  lst$n0 <- NULL
  lst$n0[1:(df$n_age - 1)] <- lst$r0 * exp(-df$ages[1:(df$n_age - 1)] * lst$m0)
  # Weight-at-age
  lst$wage_ssb <- get_age_dat(df$wage_ssb_df, df$s_yr)
  lst$wage_survey <- get_age_dat(df$wage_survey_df, df$s_yr)
  # Initial biomass
  lst$ssb_0 <- map_dbl(seq_len(df$n_space), ~{
    sum(lst$n0 * df$move_init[.x] * lst$wage_ssb) * 0.5
  }) %>% set_names(paste0(rep("space", each = df$n_space), seq_len(df$n_space)))

  # Set m-at-age for year 1, space 1, season 1
  lst$z_save[, 1, 1, 1] <- lst$m_age
  # Assumed no fishing before data started
  # An alternate way of doing it, where the year column is used
  #  instead of the first column:
  # lst$catch_age <- lst$catch_age %>%
  #   as.data.frame() %>%
  #   mutate(!!sym(as.character(df$s_yr)) := m)
  # Set catch and catch-at-age for first year to 0
  lst$catch_age[, 1] <- 0
  lst$catch[1] <- 0
  lst$catch_n[1] <- 0
  lst$catch_n_age[, 1] <- 0
  # Set first survey year to 1, surveys start later
  lst$survey[1] <- 1
  # Distribute over space
  lst$n_init <- rep(NA, df$n_age)
  lst$n_init_dev <- df$parms_init$init_n
  lst$age_1_ind <- which(df$ages == 1)

  # Initial numbers-at-age for all older than age 0
  lst$n_init[lst$age_1_ind:(df$n_age - 1)] <- lst$r0 *
    exp(-lst$m_cumu_age[lst$age_1_ind:(df$n_age - 1)]) *
    exp(-0.5 * lst$rdev_sd ^ 2 * 0 + lst$n_init_dev[1:(df$n_age - 2),]$val)

  # Plus group (ignore recruitment devs in first yrs )
  lst$n_init[df$n_age] <- lst$r0 *
    exp(-(lst$m_age[df$n_age] * df$ages[df$n_age])) / (1 - exp(-lst$m_age[df$n_age])) *
    exp(-0.5 * lst$rdev_sd ^ 2 * 0 + lst$n_init_dev[df$n_age - lst$age_1_ind - 1,]$val)

  for(space in seq_len(df$n_space)){
    # Initialize only
    # Set numbers-at-age for year 1, space, season 1
    lst$n_save_age[, 1, space, 1] <- lst$n_init * df$move_init[space]
    # Set mid-year numbers-at-age by equally partitioning M across
    #  seasons and dividing by 2
    lst$n_save_age_mid[, 1, space, 1] <- lst$n_save_age[, 1, space, 1] *
      exp(-0.5 * (lst$m_age / df$n_season))
    # Survey value in the first year will be NA because surveys
    # don't start until later years
    lst$survey_true[space, 1] <- sum(lst$n_save_age[, 1, space, df$survey_season] *
                                       lst$surv_sel * lst$q * lst$wage_survey)
  }
  lst
}