#' Run the Operating model for all years
#'
#' @param df See [run_om()]
#' @param lst See [run_om()]
#' @param verbose Print the loop information to the console
#' @param ... Additional arguments to be passed to [run_season_loop_om()]
#'
#' @return A list
#' @importFrom crayon green
#' @export
run_year_loop_om <- function(df = NULL,
                             lst = NULL,
                             verbose = TRUE,
                             ...){
  verify_argument(df, "list")
  verify_argument(lst, "list")

  map(df$yrs, function(yr = .x){
    if(verbose){
      cat(green(paste0(yr, ":\n")))
    }
    # Extract data for the year
    yr_ind <- which(yr == df$yrs)
    wage <- get_wa_dfs(df, yr)

    r_y <- df$parameters$r_in %>% filter(yr == !!yr) %>% pull(value)
    m_yrs <- lst$m_age
    # M is distributed throughout the yrs
    m_season <- m_yrs / df$n_season

    init_rec <- map_dbl(seq_len(df$n_space), function(space = .x){
      # Recruitment only in season 1
      if(yr_ind == 1){
        lst$ssb[1, space] <<- lst$init_ssb[space]
      }else{
        wage <- get_wa_dfs(df, yr)
        lst$ssb[yr_ind, space] <<- sum(lst$n_save_age[, yr_ind, space, 1] * wage$ssb, na.rm = TRUE) * 0.5
      }
      j <- (4 * lst$h * lst$r0_space[space] * lst$ssb[yr_ind, space] /
          (lst$ssb_0[space] * (1 - lst$h) + lst$ssb[yr_ind, space] *
             (5 * lst$h - 1))) * exp(-0.5 * df$b[yr_ind] *
                                       lst$rdev_sd ^ 2 + r_y) #*recruit_mat[space]
      j
    }) %>% set_names(df$space_names)
    # Sanity check - these should equal the proportions in df$move_init:
    # c(rec[1] / sum(rec), rec[2] / sum(rec))
    lst$n_save_age[1, yr_ind, , 1] <<- init_rec
#if(yr_ind == 54) browser()
    lst$r_save[yr_ind, ] <<- init_rec

    # -------------------------------------------------------------------------
    lst <<- run_season_loop_om(df = df,
                               lst = lst,
                               yr = yr,
                               yr_ind = yr_ind,
                               m_season = m_season,
                               verbose = verbose,
                               ...)

    if(df$n_season > 1){
      lst$catch_age[, yr_ind] <<- apply(lst$catch_n_save_age[, yr_ind,,],
                                       MARGIN = 1,
                                       FUN = sum)
      lst$catch[yr_ind] <<- sum(lst$catch_n_save_age[,yr_ind,,])

      lst$catch_n_age[, yr_ind] <<- apply(lst$catch_n_save_age[,yr_ind,,],
                                         MARGIN = 1,
                                         FUN = sum)
      lst$catch_n[yr_ind] <<- sum(lst$catch_n_save_age[,yr_ind,,])
    }else{
      if(df$n_space == 1){
        lst$catch_age[,yr_ind] <<- lst$catch_n_save_age[,yr_ind,,]
        lst$catch[yr_ind] <<- sum(lst$catch_n_save_age[,yr_ind,,])
        lst$catch_n_age[,yr_ind] <<- lst$catch_n_save_age[,yr_ind,,]
        lst$catch_n[yr_ind] <<- sum(lst$catch_n_save_age[,yr_ind,,])
      }else{
        lst$catch_age[,yr_ind] <<- rowSums(lst$catch_n_save_age[,yr_ind,,])
        lst$catch[yr_ind] <<- sum(lst$catch_n_save_age[,yr_ind,,])
        lst$catch_n_age[,yr_ind] <<- rowSums(lst$catch_n_save_age[,yr_ind,,])
        lst$catch_n[yr_ind] <<- sum(lst$catch_n_save_age[,yr_ind,,])
      }
    }

    m_surv_mul <- 0
    if(df$n_season == 1){
      m_surv_mul <- 0.5
    }

    # Calculate survey biomass for all spaces
    lst$survey_true[,yr_ind] <<- map_dbl(seq_len(df$n_space), ~{
      sum(lst$n_save_age[, yr_ind, .x, df$survey_season] *
          exp(-m_surv_mul * lst$z_save[, yr_ind, .x, df$survey_season]) *
            lst$surv_sel * lst$q * wage$survey)
    })

    # Calculate numbers in the survey
    n_surv <- map(seq_len(df$n_space), ~{
      lst$n_save_age[, yr_ind, .x, df$survey_season] *
        exp(-m_surv_mul * lst$z_save[, yr_ind, .x, df$survey_season])
    })
    if(df$move){
      n_surv <- n_surv %>%
        set_names(seq_len(length(n_surv))) %>%
        bind_rows() #%>%
        #mutate(sum = rowSums(.))
      # Take the sum of all the spaces (areas)
      n_surv <- n_surv %>% apply(2, sum)
    }

    if(df$flag_survey[yr_ind] == 1){
      if(yr > df$m_yr){
        err <- exp(rnorm(n = 1, mean = 0, sd = lst$surv_sd))
        # If the extra factor is not included the mean is > 1
        # TODO: Check this calculation. See line 538 of run_agedbased_true_catch.R in original code
        surv <- exp(log(sum(n_surv * lst$surv_sel *
                              lst$q * wage$survey)) + err)
      }else{
        surv <- sum(n_surv * lst$surv_sel *
                      lst$q * wage$survey)
      }
      lst$survey[yr_ind] <<- surv
    }else{
      lst$survey[yr_ind] <<- 1
    }
    surv_tmp <- sum(n_surv * lst$surv_sel * lst$q)
    age_1_ind <- which(df$ages == 1)

    if(df$flag_survey[yr_ind] == 1){
      lst$age_comps_surv[1:(df$age_max_age - 1), yr_ind] <<-
        (n_surv[age_1_ind:(df$age_max_age)] *
           lst$surv_sel[age_1_ind:(df$age_max_age)] * lst$q) / surv_tmp

      lst$age_comps_surv[df$age_max_age, yr_ind] <<-
        sum(n_surv[(df$age_max_age + 1):df$n_age] *
              lst$surv_sel[(df$age_max_age + 1):df$n_age] * lst$q) / surv_tmp
    }else{
      lst$age_comps_surv[,yr_ind] <<- NA
    }
    #lst$age_comps_surv[is.na(lst$age_comps_surv)] <<- -1

    lst$surv_tot[yr_ind,] <<- map_dbl(seq_len(df$n_space), ~{
      n_surv <- lst$n_save_age[,yr_ind, .x, df$survey_season]
      lst$surv_tot[yr_ind, .x] <<- sum(n_surv *
                                         lst$surv_sel * lst$q *
                                         exp(-m_surv_mul * lst$z_save[,yr_ind, .x, df$survey_season]))
    })

    # Calculate age comps for the survey by space
    surv_age_comps_tmp <- map(seq_len(df$n_space), ~{
      c((n_surv[age_1_ind:(df$age_max_age)] *
         lst$surv_sel[age_1_ind:(df$age_max_age)] * lst$q) / lst$surv_tot[yr_ind, .x],
        # Plus group
        sum(n_surv[(df$age_max_age + 1):df$n_age] *
              lst$surv_sel[(df$age_max_age + 1):df$n_age] * lst$q) / lst$surv_tot[yr_ind, .x])
    })

    lst$age_comps_surv_space[1:df$age_max_age, yr_ind, ] <<- surv_age_comps_tmp %>%
      set_names(seq_len(length(surv_age_comps_tmp))) %>%
      bind_rows() %>%
      as.matrix()

    # Calculate catch-at-age by space. rowSums sums all seasons within a space
    catch_tmp <- map(seq_len(df$n_space), ~{
      tmp <- lst$catch_n_save_age[, yr_ind, .x,]
      rowSums(tmp)
    })
    # Aggregate catch-at-age by space into a matrix
    catch_tmp <- catch_tmp %>%
      set_names(seq_len(length(catch_tmp))) %>%
      bind_rows() %>%
      as.matrix()

    # Calculate catch totals by space by summing the catch-at-age for each space
    catch_tot <- map_dbl(seq_len(df$n_space), ~{
      sum(lst$catch_n_save_age[, yr_ind, .x,])
    })

    catch_age_comps_tmp <- map(seq_len(df$n_space), ~{
      c(catch_tmp[.x, age_1_ind:(df$age_max_age)] / catch_tot[.x],
        # Plus group
        sum(catch_tmp[(df$age_max_age + 1):df$n_age]) / catch_tot[.x])
    })
    lst$age_comps_catch_space[1:df$age_max_age, yr_ind,] <<- catch_age_comps_tmp %>%
      set_names(seq_len(length(catch_age_comps_tmp))) %>%
      bind_rows() %>%
      as.matrix()

    if(df$flag_catch[yr_ind] == 1){
      lst$age_comps_catch[1:(df$age_max_age - 1), yr_ind] <<-
        lst$catch_n_age[age_1_ind:(df$age_max_age), yr_ind] / lst$catch_n[yr_ind]
      lst$age_comps_catch[df$age_max_age, yr_ind] <<-
        sum(lst$catch_n_age[(df$age_max_age + 1):df$n_age, yr_ind]) / lst$catch_n[yr_ind]
    }
    #lst$age_comps_catch[is.na(lst$age_comps_catch)] <<- -1

    if(verbose){
      cat("\n")
    }
  })
  lst
}
