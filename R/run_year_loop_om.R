#' Run the Operating model for all years
#'
#' @param df See [run_agebased_true_catch()]
#' @param lst See [run_agebased_true_catch()]
#' @param ... Additional arguments to be passed to [run_season_loop_om()]
#'
#' @return A list
#' @export
run_year_loop_om <- function(df = NULL,
                             lst = NULL,
                             ...){
  verify_argument(df, "list")
  verify_argument(lst, "list")

  map(df$yrs, function(yr = .x){
    # Extract data for the year
    yr_ind <- which(yr == df$yrs)
    wage <- get_wa_dfs(df, yr)
    r_y <- df$parms_init$r_in %>% filter(yr == !!yr) %>% pull(x)
    m_yrs <- lst$m_age
    # M is distributed throughout the yrs
    m_season <- m_yrs / df$n_season

    # -------------------------------------------------------------------------
    # Fix SSB and recruitment in all areas
    # Extract a list of ages for the year `yr_ind` in season 1
    n_save_age <- lst$n_save_age[, yr_ind, , 1] %>% as.data.frame() %>% map(~{.x})
    # Calculate SSB for each space
    ssb_weight <- map(n_save_age, function(space_at_age = .x){
      sum(space_at_age * as.numeric(wage$ssb), na.rm = TRUE) * 0.5
    })
    ssb <- ssb_weight
    # Calculate SSB with selectivity applied for the year `yr_ind` in season 1
    ssb_all <- map(n_save_age, function(space_at_age = .x){
      sum(space_at_age * df$mat_sel, na.rm = TRUE) * 0.5
    })
    rec <- map_dbl(seq_len(df$n_space), function(space = .x){
      # Recruitment only in season 1
      (4 * lst$h * lst$r0_space[space] * ssb[[space]] /
          (lst$ssb_0[space] * (1 - lst$h) + ssb[[space]] *
             (5 * lst$h - 1))) * exp(-0.5 * df$b[yr_ind] *
                                       lst$rdev_sd ^ 2 + r_y) #*recruit_mat[space]
    }) %>% set_names(df$space_names)
    # Sanity check
    # rec[[1]] / (rec[[1]] + rec[[2]])
    # rec[[2]] / (rec[[1]] + rec[[2]])
    lst$n_save_age[1, yr_ind, , 1] <- rec
    lst$r_save[yr_ind, ] <- rec

    # -------------------------------------------------------------------------
    lst <- run_season_loop_om(df, lst, yr, yr_ind, m_season, ...)

    if(df$n_season > 1){
      lst$catch_age[, yr_ind] <- apply(lst$catch_n_save_age[, yr_ind,,],
                                       MARGIN = 1,
                                       FUN = sum)
      lst$catch[yr_ind] <- sum(lst$catch_n_save_age[,yr_ind,,])

      lst$catch_n_age[, yr_ind] <- apply(lst$catch_n_save_age[,yr_ind,,],
                                         MARGIN = 1,
                                         FUN = sum)
      lst$catch_n[yr_ind] <- sum(lst$catch_n_save_age[,yr_ind,,])
    }else{
      if(df$n_space == 1){
        lst$catch_age[,yr_ind] <- lst$catch_n_save_age[,yr_ind,,]
        lst$catch[yr_ind] <- sum(lst$catch_n_save_age[,yr_ind,,])
        lst$catch_n_age[,yr_ind] <- lst$catch_n_save_age[,yr_ind,,]
        lst$catch_n[yr_ind] <- sum(lst$catch_n_save_age[,yr_ind,,])
      }else{
        lst$catch_age[,yr_ind] <- rowSums(lst$catch_n_save_age[,yr_ind,,])
        lst$catch[yr_ind] <- sum(lst$catch_n_save_age[,yr_ind,,])
        lst$catch_n_age[,yr_ind] <- rowSums(lst$catch_n_save_age[,yr_ind,,])
        lst$catch_n[yr_ind] <- sum(lst$catch_n_save_age[,yr_ind,,])
      }
    }

    m_surv_mul <- 0
    if(df$n_season == 1){
      m_surv_mul <- 0.5
    }

    # Calculate survey biomass for all spaces
    lst$survey_true[,yr_ind] <- map_dbl(seq_len(df$n_space), ~{
        sum(lst$n_save_age[, yr_ind, .x, df$survey_season] *
              exp(-m_surv_mul * lst$z_save[, yr_ind, .x, df$survey_season]) *
              lst$surv_sel * lst$q * df$wage_survey)
      })
    browser()

  #   if(!df$move){
  #     Nsurv <- N.save.age[,yr_ind,,df$surveyseason]*
  #       exp(-m_surv_mul*Z.save[,yr_ind,space,df$surveyseason])
  #   }else{
  #     Nsurv <- rowSums(N.save.age[,yr_ind,,df$surveyseason]*
  #                        exp(-m_surv_mul*Z.save[,yr_ind,space,df$surveyseason]))
  #   }
  #   if (df$flag_survey[yr_ind] == 1){
  #     if(df$yrs[yr] > 2018){
  #       err <- rnorm(n = 1,mean = 0, sd = surv_sd)
  #       surv <- exp(log(sum(Nsurv*surv_sel*q*wage$surv))+err) # If the xtra factor is not included the mean is > 1
  #     }else{
  #       surv <- sum(Nsurv*surv_sel*q*wage$surv)
  #     }
  #     survey[yr_ind] <- surv
  #   }else{
  #     survey[yr_ind] <- 1
  #   }
  #   Ntot.yrs <- Nsurv
  #   surv.tmp <- sum(Ntot.yrs*surv_sel*q)
  #   if(df$flag_survey[yr_ind] == 1){
  #     age_comps_surv[1,yr_ind] <- 0 # No yrs 1 recorded
  #     age_comps_surv[1:(df$age_maxage-1),yr_ind] <-  (Ntot.yrs[2:(df$age_maxage)]*surv_sel[2:(df$age_maxage)]*q)/surv.tmp
  #     age_comps_surv[df$age_maxage,yr_ind] <- sum(Ntot.yrs[(df$age_maxage+1):n_age]*surv_sel[(df$age_maxage+1):n_age]*q)/surv.tmp
  #   }else{
  #     age_comps_surv[,yr_ind] <- NA
  #   }
  #   for(space in 1:df$n_space){
  #     Ntot.yrs <- N.save.age[,yr_ind,space,df$surveyseason]
  #     surv.tot[yr_ind,space]  <- sum(Ntot.yrs*surv_sel*q*exp(-m_surv_mul*Z.save[,yr_ind,space,df$surveyseason]))
  #     age_comps_surv_space[1,yr_ind,space] <- 0 # No yrs 1 recorded
  #     age_comps_surv_space[1:(df$age_maxage-1),yr_ind,space] <-
  #       (Ntot.yrs[2:(df$age_maxage)]*surv_sel[2:(df$age_maxage)]*q)/surv.tot[yr_ind,space]
  #     age_comps_surv_space[df$age_maxage,yr_ind,space] <-
  #       sum(Ntot.yrs[(df$age_maxage+1):n_age]*surv_sel[(df$age_maxage+1):n_age]*q)/surv.tot[yr_ind,space]
  #     if(df$n_season>1){
  #       Catch.tmp <- rowSums(lst$catch_n_save_age[, yr_ind,space,])
  #     }else{
  #       Catch.tmp <- lst$catch_n_save_age[, yr_ind,space,]
  #     }
  #     Catch.tot <- sum(lst$catch_n_save_age[,yr_ind,space,])
  #     age_comps_catch_space[1:(df$age_maxage-1),yr_ind,space] <- Catch.tmp[2:(df$age_maxage)]/Catch.tot
  #     age_comps_catch_space[df$age_maxage,yr_ind,space] <- sum(Catch.tmp[(df$age_maxage+1):n_age])/Catch.tot
  #   }
  #   if(df$flag_catch[yr_ind] == 1){
  #     age_comps_catch[1:(df$age_maxage-1),yr_ind] <-  lst$catch_n_age[2:(df$age_maxage),yr_ind]/lst$catch_n[yr_ind]
  #     age_comps_catch[df$age_maxage,yr_ind] <- sum(lst$catch_n_age[(df$age_maxage+1):n_age,yr_ind])/lst$catch_n[yr_ind]
  #   }else{
  #     age_comps_catch[,yr_ind] <- NA
  #   }
  # }# End of yrs loop
  # if(!df$move){
  #   Nsave <- N.save.age[,,,df$n_space]
  #   SSB.save <- SSB
  # }else{
  #   Nsave <- apply(N.save.age[,,,1],2,rowSums)
  #   SSB.save <- rowSums(SSB)
  # }
  # # Add names to output
  # yrs_1 <- c(df$yrs,max(df$yrs+1))
  # df.out   <- list(N.save = Nsave,
  #                  SSB = SSB,
  #                  N.save.age = N.save.age,
  #                  R.save = R.save,
  #                  V.save = V.save,
  #                  SSB.all = SSB.all,
  #                  catch_n_save_age = lst$catch_n_save_age,
  #                  lst$catch_n_save_age = lst$catch_n_save_age,
  #                  Catch = Catch,
  #                  catch_age = lst$catch_age,
  #                  Catch.quota = Catch.quota,
  #                  Catch.quota.N = Catch.quota.N,
  #                  Fout = Fout.save,
  #                  age_comps_OM = age_comps_OM,
  #                  age_catch = age_comps_catch,
  #                  ssb_0 = ssb_0,
  #                  n0 = n0,
  #                  SSB.weight = SSB.weight,
  #                  survey.true = survey.true,
  #                  Z = Z.save,
  #                  survey = as.numeric(survey),
  #                  age_comps_surv = age_comps_surv,
  #                  age_comps_countr_y = age_comps_surv_space,
  #                  age_comps_catch_space = age_comps_catch_space,
  #                  Fseason = Fseason.save,
  #                  f_sel = f_sel_save,
  #                  n_init = n_init,
  #                  SSB0 = ssb_0)
  })
}
