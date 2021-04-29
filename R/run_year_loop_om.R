#' Run the Operating model for all years
#'
#' @param om See [run_om()]
#' @param yrs A vector of years for which to run the operating model
#' @param verbose Print the loop information to the console
#' @param ... Additional arguments to be passed to [run_season_loop_om()]
#'
#' @return A list
#' @importFrom crayon green
#' @export
run_year_loop_om <- function(om = NULL,
                             yrs = NULL,
                             verbose = TRUE,
                             ...){

  verify_argument(om, "list")
  verify_argument(yrs, c("integer", "numerical"))
  verify_argument(verbose, "logical", 1)

  # Begin year loop -----------------------------------------------------------
  map(yrs, function(yr = .x){
    if(verbose){
      cat(green(paste0(yr, ":\n")))
    }
    # Extract data for the year
    yr_ind <- which(yr == om$yrs)
    wage <- get_wa_dfs(om, yr)

    r_y <- om$parameters$r_in %>% filter(yr == !!yr) %>% pull(value)
    m_yrs <- om$m_age
    # M is distributed throughout the yrs
    m_season <- m_yrs / om$n_season

    # Setup initial recruitment -----------------------------------------------
    init_rec <- map_dbl(seq_len(om$n_space), function(space = .x){
      # Recruitment only in season 1
      if(yr_ind == 1){
        om$ssb_initial[1, space] <<- om$init_ssb[space]
      }else{
        wage <- get_wa_dfs(om, yr)
        om$ssb_initial[yr_ind, space] <<- sum(om$n_save_age[, yr_ind, space, 1] * wage$ssb, na.rm = TRUE) * 0.5
      }
      rec <- (4 * om$h * om$r0_space[space] * om$ssb_initial[yr_ind, space] /
          (om$ssb_0[space] * (1 - om$h) + om$ssb_initial[yr_ind, space] *
             (5 * om$h - 1))) * exp(-0.5 * om$b[yr_ind] *
                                      om$rdev_sd ^ 2 + r_y) #*recruit_mat[space]
      rec
    }) %>% set_names(om$space_names)

    om$n_save_age[1, yr_ind, , 1] <<- init_rec

    om$r_save[yr_ind, ] <<- init_rec
    # Run season loop code ----------------------------------------------------
    om <<- run_season_loop_om(om = om,
                              yr = yr,
                              yr_ind = yr_ind,
                              m_season = m_season,
                              verbose = verbose,
                              ...)
    # Return from season loop -------------------------------------------------


    # Calculate catch and catch-age -------------------------------------------
    if(om$n_season > 1){
      om$catch_age[, yr_ind] <<- rowSums(om$catch_save_age[, yr_ind,,])
      om$catch[yr_ind] <<- sum(om$catch_save_age[,yr_ind,,])
      om$catch_n_age[, yr_ind] <<- rowSums(om$catch_n_save_age[,yr_ind,,])
      om$catch_n[yr_ind] <<- sum(om$catch_n_save_age[,yr_ind,,])
    }else{
      if(om$n_space == 1){
        om$catch_age[,yr_ind] <<- om$catch_save_age[,yr_ind,,]
        om$catch[yr_ind] <<- sum(om$catch_save_age[,yr_ind,,])
        om$catch_n_age[,yr_ind] <<- om$catch_n_save_age[,yr_ind,,]
        om$catch_n[yr_ind] <<- sum(om$catch_n_save_age[,yr_ind,,])
      }else{
        om$catch_age[,yr_ind] <<- rowSums(om$catch_savebr_age[,yr_ind,,])
        om$catch[yr_ind] <<- sum(om$catch_save_age[,yr_ind,,])
        om$catch_n_age[,yr_ind] <<- rowSums(om$catch_n_save_age[,yr_ind,,])
        om$catch_n[yr_ind] <<- sum(om$catch_n_save_age[,yr_ind,,])
      }
    }

    # Calculate survey true biomass -------------------------------------------
    m_surv_mul <- 0
    if(om$n_season == 1){
      m_surv_mul <- 0.5
    }
    om$survey_true[,yr_ind] <<- map_dbl(seq_len(om$n_space), ~{
      sum(om$n_save_age[, yr_ind, .x, om$survey_season] *
          exp(-m_surv_mul * om$z_save[, yr_ind, .x, om$survey_season]) *
            om$surv_sel * om$q * wage$survey)
    })

    # Calculate numbers in survey biomass -------------------------------------
    n_surv <- map(seq_len(om$n_space), ~{
      om$n_save_age[, yr_ind, .x, om$survey_season] *
        exp(-m_surv_mul * om$z_save[, yr_ind, .x, om$survey_season])
    })

    if(om$move){
      n_surv <- n_surv %>%
        set_names(seq_len(length(n_surv))) %>%
        bind_rows()
      # Take the sum of all the spaces (areas)
      n_surv <- n_surv %>% apply(2, sum)
    }

    # Calculate survey abundance -------------------------------------------------
    if(om$flag_survey[yr_ind] == 1){
      if(yr > om$m_yr){
        err <- exp(rnorm(n = 1, mean = 0, sd = om$surv_sd))
        surv <- sum(n_surv * om$surv_sel *
                      om$q * (wage$survey %>% unlist(use.names = FALSE)) * err)
      }else{
        surv <- sum(n_surv * om$surv_sel *
                      om$q * wage$survey)
      }
      om$survey[yr_ind] <<- surv
    }else{
      om$survey[yr_ind] <<- 1
    }
    # Calculate survey age-comps ----------------------------------------------
    surv_tmp <- sum(n_surv * om$surv_sel * om$q)
    age_1_ind <- which(om$ages == 1)
    if(om$flag_survey[yr_ind] == 1){
      om$age_comps_surv[1:(om$age_max_age - 1), yr_ind] <<-
        (n_surv[age_1_ind:(om$age_max_age)] *
           om$surv_sel[age_1_ind:(om$age_max_age)] * om$q) / surv_tmp
      om$age_comps_surv[om$age_max_age, yr_ind] <<-
        sum(n_surv[(om$age_max_age + 1):om$n_age] *
              om$surv_sel[(om$age_max_age + 1):om$n_age] * om$q) / surv_tmp
    }else{
      om$age_comps_surv[,yr_ind] <<- NA
    }

    # Calculate survey biomass by space -------------------------------------
    om$surv_tot[yr_ind,] <<- map_dbl(seq_len(om$n_space), ~{
      n_surv <- om$n_save_age[,yr_ind, .x, om$survey_season]
      om$surv_tot[yr_ind, .x] <<- sum(n_surv *
                                         om$surv_sel * om$q *
                                         exp(-m_surv_mul * om$z_save[,yr_ind, .x, om$survey_season]))
    })

    # Calculate survey age-comps by space -------------------------------------
    surv_age_comps_tmp <- map(seq_len(om$n_space), ~{
      c((n_surv[age_1_ind:(om$age_max_age)] *
         om$surv_sel[age_1_ind:(om$age_max_age)] * om$q) / om$surv_tot[yr_ind, .x],
        # Plus group
        sum(n_surv[(om$age_max_age + 1):om$n_age] *
              om$surv_sel[(om$age_max_age + 1):om$n_age] * om$q) / om$surv_tot[yr_ind, .x])
    })
    om$age_comps_surv_space[1:om$age_max_age, yr_ind, ] <<- surv_age_comps_tmp %>%
      set_names(seq_len(length(surv_age_comps_tmp))) %>%
      bind_rows() %>%
      as.matrix()

    # Calculate catch value by space -----------------------------------------
    # rowSums sums all seasons within a space
    catch_tmp <- map(seq_len(om$n_space), ~{
      tmp <- om$catch_n_save_age[, yr_ind, .x,]
      rowSums(tmp)
    })
    # Aggregate catch-at-age by space
    catch_tmp <- catch_tmp %>%
      set_names(seq_len(length(catch_tmp))) %>%
      bind_rows() %>%
      as.matrix()
    # Calculate catch totals by space by summing the catch-at-age for each space
    catch_tot <- map_dbl(seq_len(om$n_space), ~{
      sum(om$catch_n_save_age[, yr_ind, .x,])
    })
    catch_age_comps_tmp <- map(seq_len(om$n_space), ~{
      c(catch_tmp[.x, age_1_ind :om$age_max_age],
        # Plus group
        sum(catch_tmp[.x, (om$age_max_age + 1):om$n_age])) / catch_tot[.x]
    })

    om$age_comps_catch_space[1:om$age_max_age, yr_ind,] <<- catch_age_comps_tmp %>%
      set_names(seq_len(length(catch_age_comps_tmp))) %>%
      bind_rows() %>%
      as.matrix()

    if(om$flag_catch[yr_ind] == 1){
      om$age_comps_catch[1:(om$age_max_age - 1), yr_ind] <<-
        om$catch_n_age[age_1_ind:(om$age_max_age), yr_ind] / om$catch_n[yr_ind]
      om$age_comps_catch[om$age_max_age, yr_ind] <<-
        sum(om$catch_n_age[(om$age_max_age + 1):om$n_age, yr_ind]) / om$catch_n[yr_ind]
    }

    if(verbose){
      cat("\n")
    }
  })
  # End year loop -------------------------------------------------------------
  om
}
