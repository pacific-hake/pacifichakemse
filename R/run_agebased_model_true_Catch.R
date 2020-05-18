#' Run an agebased model
#'
#' @param df data frame of parameters and life histor_y values
#' @param seed seed for survey error and recruitment deviations
#'
#' @return A list of model outputs (TODO)
#' @importFrom purrr map_dbl
#' @export
#'
#' @examples
#' \dontrun{
#' run_agebased_true_catch(df)
#' }
run_agebased_true_catch <- function(df = NULL,
                                    seed = 100){
  verify_argument(df, "list")
  verify_argument(seed, "numeric", 1)

  set.seed(seed)

  n_season <- df$n_season
  df$t_end <- length(df$yrs) * n_season
  n_yr <-df$t_end / df$n_season
  yrs <- df$yrs
  t_end <- n_yr * n_season
  # Set up the data spatially
  n_space <- df$n_space
  recruit_mat <- df$recruit_mat
  if(!df$move){
    recruit_mat <- 1
  }
  move_mat <- df$move_mat
  move_init <- df$move_init
  # M selectivity
  m_sel <- df$m_sel # no difference between males and females
  m0 <- exp(df$parms_init$log_m_init)
  M <- m0 * m_sel # Natural mortality at age
  rdev_sd <- exp(df$rdev_sd)
  b <- rep(1, n_yr)
  # Survey selectivity - constant over time
  surv_sel <- get_select(df$ages,
                         df$parms_init$p_sel_surv,
                         df$s_min_survey,
                         df$s_max_survey)
  # Catchability
  q <- exp(df$log_q) # Constant over time
  surv_sd <- exp(df$parms_init$log_sd_surv) # Survey error
  # Maturity and fecundity
  mat_sel <- df$mat_sel
  h <- exp(df$parms_init$log_h)
  # Age
  n_age <- df$n_age
  age <- df$ages
  r0 <- exp(df$parms_init$log_r_init)
  m_age <- c(0, cumsum(M[1:(n_age - 1)]))
  # Calculate n0 based on r0
  n0 <- NULL
  n0[1:(n_age - 1)] <- r0 * exp(-df$ages[1:(n_age - 1)] * m0)
  n0[n_age] <- r0 * exp(-m0 * df$ages[n_age]) / (1 - exp(-m0))

  r0_space <- r0 * move_init

  wage_ssb <- get_age_dat(df$wage_ssb, df$s_yr)
  ssb_0 <- map_dbl(seq_len(n_space), ~{
    sum(n0 * move_init[.x] * wage_ssb) * 0.5
  }) %>% set_names(paste0(rep('space', each = df$n_space), seq_len(n_space)))

  lst <- setup_blank_om_objects(yrs = yrs,
                                ages = df$ages,
                                n_space = n_space,
                                n_season = n_season)

  lst$z_save[, 1, 1, 1] <- M
  # Assumed no fishing before data started
  lst$catch_age[, 1] <- 0
  lst$catch[1] <- 0
  lst$catch_n[1] <- 0
  lst$catch_n_age[, 1] <- 0
  # Surveys start later
  lst$survey[1] <- 1
  idx_save <- seq(1, t_end, by = n_season)
  # Distribute over space
  n_init <- NULL
  n_init_dev <- (df$parms_init$init_n)
  age_1_ind <- which(df$ages == 1)

  n_init[age_1_ind:(n_age - 1)] <- r0 * exp(-m_age[age_1_ind:(n_age - 1)]) *
    exp(-0.5 * rdev_sd ^ 2 * 0 + n_init_dev[1:(n_age - 2)])
  # Plus group (ignore recruitment dev's in first yrs )
  n_init[n_age] <- r0 * exp(-(M[n_age] * df$ages[n_age])) / (1 - exp(-M[n_age])) *
    exp(-0.5 * rdev_sd ^ 2 * 0 + n_init_dev[n_age - 1])

  for(space in seq_len(n_space)){
    # Initialize only
    lst$n_save_age[, 1, space, 1] <- n_init * move_init[space]
    lst$n_save_age_mid[, 1, space, 1] <- lst$n_save_age[, 1, space, 1] * exp(-0.5 * (M / n_season))
    wage_survey <- get_age_dat(df$wage_survey, df$s_yr)
    lst$survey_true[space, 1] <- sum(lst$n_save_age[, 1, space, df$survey_season] *
                                       surv_sel * q * wage_survey)
  }
  # Contribution of Total catch (add to one) # Z <- (Fyrs + m_yrs)
  catch_props_season <- df$catch_props_season
  #pope_mul <- n_season / 1 * 0.5
  pope_mul <- 0.50
  # if(n_season == 1){
  #   catch_props_season <- matrix(rep(1, df$n_space))
  # }
  # map(yrs, ~{
  #   map(seq_len(n_season), ~{
  #     map(seq_len(n_space), ~{
  #
  #     })
  #   })
  # })
  for(yr in yrs){
    yr_ind <- which(yr == yrs)
    wage <- get_wa_dfs(df, yr)
    r_y <- df$parms_init$r_in %>% filter(yr == !!yr) %>% pull(x)
    m_yrs <- M
    # M is distributed throughout the yrs
    m_season <- m_yrs / n_season
    # fix SSB and recruitment in all areas
    for(space in seq_len(n_space)){
      lst$ssb_weight[yr_ind, space] <- sum(lst$n_save_age[, yr_ind, space, 1] *
                                     as.numeric(wage$ssb), na.rm = TRUE) * 0.5
      #sum(N.save.age[,yr,space,1]*mat_sel, na.rm = TRUE)
      lst$ssb[yr_ind, space] <- lst$ssb_weight[yr_ind, space]
      lst$ssb_all[1, space, 1]<- sum(lst$n_save_age[, 1, space, 1] *
                                     mat_sel, na.rm = TRUE) * 0.5
      # Recruitment only in season 1
      R <- (4 * h * r0_space[space] * lst$ssb[yr_ind, space] /
              (ssb_0[space] * (1 - h) + lst$ssb[yr_ind, space] *
                 (5 * h - 1))) * exp(-0.5 * df$b[yr_ind] * rdev_sd ^ 2 + r_y) #*recruit_mat[space]
      lst$n_save_age[1, yr_ind, space, 1] <- R
      lst$r_save[yr_ind, space] <- R
    }
    browser()
    for(season in seq_len(n_season)){
      for(space in seq_len(n_space)){
        # Get the selectivity of the season and area
        p_sel <- df$p_sel[space,]
        if(df$flag_sel[yr_ind] == 1){
          p_sel_tmp <- p_sel + df$parms_init$p_sel[, yr_ind - df$sel_idx + 1] * df$sigma_p_sel
        }else{
          p_sel_tmp <- p_sel
        }
        if(yrs[yr_ind] > df$m_yr){
          if(df$selectivity_change == 1){
            if(space == 1){
              p_sel_tmp <- rep(1, df$s_max_survey - df$s_min_survey + 1)
            }else{
              p_sel_tmp <- c(rep(0.05, df$s_min_survey),
                             rep(0, df$s_max_survey - 2 * df$s_min_survey + 1))
            }
          }
          if(df$selectivity_change == 2){
            p_sel_tmp <- df$p_sel[2,] + df$parms_init$p_sel[,ncol(df$parms_init$p_sel)] * df$sigma_p_sel
          }
        }
        # Constant over space right now
        browser()
        f_sel <- get_select(age,
                            p_sel_tmp,
                            df$s_min,
                            df$s_max)
        #rm(p_sel_tmp)
        f_sel_save[,yr,space] <- f_sel
        if(n_space > 1){
          if(df$yrs[yr_ind] <= df$m_yr){
            catch_space <- df$catch_country[yr_ind, space]
          }else{
            catch_space <- df$catch[yr_ind] * df$f_space[space]
          }
        }else{
          catch_space <- df$catch[yr_ind]
        }
        browser()
        E.temp <- Catch_space*catch_props_season[space, season]#*df$f_space[space] # Catch distribution in the yrs
        B.tmp <-  sum(N.save.age[,yr,space,season]*exp(-m_season*pope.mul)*wage$catch*f_sel) # Get biomass from previous yrs
        N.tmp <- N.save.age[,yr,space,season]#
        V.save[yr,space,season] <- B.tmp
        Catch.quota[yr,space,season] <- E.temp
        if(E.temp/B.tmp >= .9){
          if(df$yrss[yr] < 2018){
            stop(paste('Catch exceeds available biomass in yrs:',yrs,' and season', season, 'area', space)) # Stop if in the past
          }
          #pr_int(paste('Catch exceeds available biomass in yrs:',yrs,' and season', season, 'area', space))
          E.temp <- 0.75*B.tmp
          Catch.quota.N[yr,space,season] <- 1
          #if(df$yrss[yr] > 2026){
          #stop('danger')
          #  }
        }
        Fout <- get_f(e_temp = E.temp,
                      b_temp = B.tmp,
                      m_season = m_season,
                      f_sel = f_sel,
                      n_temp = N.tmp,
                      w_catch = wage$catch,
                      method = 'Hybrid')
        Fout <- Fout
        #Fout <- df$parms_init$F0[yr]

        if(E.temp>0){
          Fseason <- Fout*f_sel
          Fnew <- Fout
          Z <- Fnew*f_sel+m_season
          Fseason <- Fnew*f_sel
        }else{
          Fseason <- 0
        }
        Fout.save[yr,season,space] <- Fout # terminal fishing mortality
        Fseason.save[,yr,space,season] <- Fseason
        Z <- m_season+Fseason
        Z.save[,yr,space,season]<- Z
        if(((space - 1) == 0)){
          spaceidx <- 2
        }
        if(space == n_space){
          spaceidx <- n_space-1
        }
        if(space > 1 & space < n_space){
          spaceidx <- c(space-1,space+1)
        }
        if(!df$move){
          spaceidx <- 1
        }
        if(season <n_season){
          N.save.age[,yr,space,season+1] <- N.save.age[,yr,space,season]*exp(-Z)-
            N.save.age[, yr,space,season]*exp(-Z)*(move_mat[space,,season,yr])+ # Remove the ones that leave
            N.save.age[, yr,spaceidx,season]*exp(-Z)*(move_mat[spaceidx,,season,yr])# add the ones come to the surrounding areas
          age_comps_OM[,yr,space,season] <- N.save.age[, yr,space,season]/sum(N.save.age[, yr,space,season])
          SSB.all[yr,space,season]<- sum(N.save.age[,yr,space,season]*mat_sel, na.rm = T)
          Catch.save.age[, yr,space, season] <- (Fseason/(Z))*(1-exp(-(Z)))*N.save.age[,yr,space,season]*wage$catch
          CatchN.save.age[, yr,space, season] <- (Fseason/(Z))*(1-exp(-(Z)))*N.save.age[,yr,space,season]
        }else{
          N.save.age[2:(n_age-1),yr+1,space,1] <- N.save.age[1:(n_age-2),yr,space,season]*exp(-Z[1:(n_age-2)])-
            N.save.age[1:(n_age-2), yr,space,season]*exp(-Z[1:(n_age-2)])*(move_mat[space,1:(n_age-2),season,yr])+ # Remove the ones that leave
            N.save.age[1:(n_age-2), yr,spaceidx,season]*exp(-Z[1:(n_age-2)])*(move_mat[spaceidx,1:(n_age-2),season,yr])# add the ones come to the surrounding areas

          # Plus group
          Nsurvive.plus <- (N.save.age[n_age-1, yr,space, n_season]*exp(-Z[n_age-1])+
                              N.save.age[n_age, yr,space, n_season]*exp(-Z[n_age]))
          Nout.plus <- Nsurvive.plus*(move_mat[space,n_age, season,yr]) # Leaving
          Nin.plus <- (N.save.age[n_age-1, yr,spaceidx,n_season]*exp(-Z[n_age-1])+
                         N.save.age[n_age, yr,spaceidx,n_season]*exp(-Z[n_age]))*
            (move_mat[spaceidx,n_age, season,yr]) # Incoming
          N.save.age[n_age,yr+1,space,1] <- Nsurvive.plus- Nout.plus + Nin.plus
          age_comps_OM[,yr,space,season] <- N.save.age[, yr,space,season]/sum(N.save.age[, yr,space,season])
          SSB.all[yr,space,season]<- sum(N.save.age[,yr,space,season]*mat_sel, na.rm = T)
          Catch.save.age[, yr,space, season] <- (Fseason/(Z))*(1-exp(-(Z)))*N.save.age[,yr,space,season]*wage$catch
          CatchN.save.age[, yr,space, season] <- (Fseason/(Z))*(1-exp(-(Z)))*N.save.age[,yr,space,season]
        }
        if(is.na(SSB[yr,space])){
          stop('SSB is NA')
        }
      }

      if(Catch.quota[yr,space,season]>0){
        if((sum(Catch.save.age[, yr,space, season])/Catch.quota[yr,space,season]) > 1.05){
          stop('F estimation overshoots more than 10%')
        }
      }
    } # End of season loop
    #Catch.age[,idx]  <- (Fyrs/(Fyrs+m_yrs))*(1-exp(-(Fyrs+m_yrs)))*rowSums(N.save.age[,idx,,1])*wage$catch # Calculate the catch in kg
    if(n_season>1){
      Catch.age[,yr] <- apply(Catch.save.age[,yr,,],MARGIN = 1,FUN = sum)
      Catch[yr] <- sum(Catch.save.age[,yr,,])

      CatchN.age[,yr] <- apply(CatchN.save.age[,yr,,],MARGIN = 1,FUN = sum)
      CatchN[yr] <- sum(CatchN.save.age[,yr,,])
    }else{

      if(n_space == 1){
        Catch.age[,yr] <- Catch.save.age[,yr,,]
        Catch[yr] <- sum(Catch.save.age[,yr,,])

        CatchN.age[,yr] <- CatchN.save.age[,yr,,]
        CatchN[yr] <- sum(CatchN.save.age[,yr,,])
      }else{
        Catch.age[,yr] <- rowSums(Catch.save.age[,yr,,])
        Catch[yr] <- sum(Catch.save.age[,yr,,])

        CatchN.age[,yr] <- rowSums(CatchN.save.age[,yr,,])
        CatchN[yr] <- sum(CatchN.save.age[,yr,,])
      }
    }
    if(n_season == 1){
      Msurveymul <- 0.5
    }else{
      Msurveymul <- 0
    }
    for (space in 1:n_space){
      survey.true[space,yr] <- sum(N.save.age[,yr,space,df$surveyseason]*
                                     exp(-Msurveymul*Z.save[,yr,space,df$surveyseason])*surv_sel*q*wage$surv)
    }
    #  }
    # Save the survey
    # Survey is conducted in the start of the yrs
    # }else{
    #   Msurveymul <- 0.5
    # }
    if(!df$move){
      Nsurv <- N.save.age[,yr,,df$surveyseason]*
        exp(-Msurveymul*Z.save[,yr,space,df$surveyseason])
    }else{
      Nsurv <- rowSums(N.save.age[,yr,,df$surveyseason]*
                         exp(-Msurveymul*Z.save[,yr,space,df$surveyseason]))
    }
    if (df$flag_survey[yr] == 1){
      if(yrs[yr] > 2018){
        err <- rnorm(n = 1,mean = 0, sd = surv_sd)
        surv <- exp(log(sum(Nsurv*surv_sel*q*wage$surv))+err) # If the xtra factor is not included the mean is > 1
      }else{
        surv <- sum(Nsurv*surv_sel*q*wage$surv)
      }
      survey[yr] <- surv
    }else{
      survey[yr] <- 1
    }
    Ntot.yrs <- Nsurv
    surv.tmp <- sum(Ntot.yrs*surv_sel*q)
    if(df$flag_survey[yr] == 1){
      age_comps_surv[1,yr] <- 0 # No yrs 1 recorded
      age_comps_surv[1:(df$age_maxage-1),yr] <-  (Ntot.yrs[2:(df$age_maxage)]*surv_sel[2:(df$age_maxage)]*q)/surv.tmp
      age_comps_surv[df$age_maxage,yr] <- sum(Ntot.yrs[(df$age_maxage+1):n_age]*surv_sel[(df$age_maxage+1):n_age]*q)/surv.tmp
    }else{
      age_comps_surv[,yr] <- NA
    }
    for(space in 1:n_space){
      Ntot.yrs <- N.save.age[,yr,space,df$surveyseason]
      surv.tot[yr,space]  <- sum(Ntot.yrs*surv_sel*q*exp(-Msurveymul*Z.save[,yr,space,df$surveyseason]))
      age_comps_surv_space[1,yr,space] <- 0 # No yrs 1 recorded
      age_comps_surv_space[1:(df$age_maxage-1),yr,space] <-
        (Ntot.yrs[2:(df$age_maxage)]*surv_sel[2:(df$age_maxage)]*q)/surv.tot[yr,space]
      age_comps_surv_space[df$age_maxage,yr,space] <-
        sum(Ntot.yrs[(df$age_maxage+1):n_age]*surv_sel[(df$age_maxage+1):n_age]*q)/surv.tot[yr,space]
      if(n_season>1){
        Catch.tmp <- rowSums(CatchN.save.age[, yr,space,])
      }else{
        Catch.tmp <- CatchN.save.age[, yr,space,]
      }
      Catch.tot <- sum(CatchN.save.age[,yr,space,])
      age_comps_catch_space[1:(df$age_maxage-1),yr,space] <- Catch.tmp[2:(df$age_maxage)]/Catch.tot
      age_comps_catch_space[df$age_maxage,yr,space] <- sum(Catch.tmp[(df$age_maxage+1):n_age])/Catch.tot
    }
    if(df$flag_catch[yr] == 1){
      age_comps_catch[1:(df$age_maxage-1),yr] <-  CatchN.age[2:(df$age_maxage),yr]/CatchN[yr]
      age_comps_catch[df$age_maxage,yr] <- sum(CatchN.age[(df$age_maxage+1):n_age,yr])/CatchN[yr]
    }else{
      age_comps_catch[,yr] <- NA
    }
  }# End of yrs loop
  if(!df$move){
    Nsave <- N.save.age[,,,n_space]
    SSB.save <- SSB
  }else{
    Nsave <- apply(N.save.age[,,,1],2,rowSums)
    SSB.save <- rowSums(SSB)
  }
  # Add names to output
  yrs_1 <- c(df$yrss,max(df$yrss+1))
  df.out   <- list(N.save = Nsave,
                   SSB = SSB,
                   N.save.age = N.save.age,
                   R.save = R.save,
                   V.save = V.save,
                   SSB.all = SSB.all,
                   Catch.save.age = Catch.save.age,
                   CatchN.save.age = CatchN.save.age,
                   Catch = Catch,
                   Catch.age = Catch.age,
                   Catch.quota = Catch.quota,
                   Catch.quota.N = Catch.quota.N,
                   Fout = Fout.save,
                   age_comps_OM = age_comps_OM,
                   age_catch = age_comps_catch,
                   ssb_0 = ssb_0,
                   n0 = n0,
                   SSB.weight = SSB.weight,
                   survey.true = survey.true,
                   Z = Z.save,
                   survey = as.numeric(survey),
                   age_comps_surv = age_comps_surv,
                   age_comps_countr_y = age_comps_surv_space,
                   age_comps_catch_space = age_comps_catch_space,
                   Fseason = Fseason.save,
                   f_sel = f_sel_save,
                   n_init = n_init,
                   SSB0 = ssb_0)

  df.out
}

