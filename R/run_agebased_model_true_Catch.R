#' Run an agebased model
#'
#' @param df data frame of parameters and life history values
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
  stopifnot(!is.null(df))
  stopifnot(!is.null(seed))
  stopifnot(is.numeric(seed))
  stopifnot(length(seed) == 1)

  set.seed(seed)
  nseason <- df$nseason
  df$t_end <- length(df$yrs) * nseason
  nyr <-df$t_end / df$nseason
  yrs <- df$yrs
  t_end <- nyr * nseason
  # Set up the data spatially
  nspace <- df$nspace
  recruitmat <- df$recruitmat
  if(!df$move){
    recruitmat <- 1
  }
  movemat <- df$movemat
  move_init <- df$move_init
  # M selectivity
  Msel <- df$Msel # no difference between males and females
  M0 <- exp(df$parms$logMinit)
  M <- M0 * Msel # Naural mortality at age
  SDR <- exp(df$logSDR)
  b <- rep(1, nyr)
  # Survey selectivity - constant over time
  surv_sel <- getSelec(df$ages,
                       df$parms$psel_surv,
                       df$Smin_survey,
                       df$Smax_survey)
  # Catchability
  q <- exp(df$logQ) # Constant over time
  surv_sd <- exp(df$parms$logSDsurv) # Survey error
  # Maturity and fecundity
  mat_sel <- df$Matsel
  h <- exp(df$parms$logh)
  # Age
  nage <- df$nage
  age <- df$ages
  R0 <- exp(df$parms$logRinit)
  m_age <- c(0, cumsum(M[1:(nage - 1)]))
  # Calculate N0 based on R0
  N0 <- NULL
  N0[1:(nage - 1)] <- R0 * exp(-df$ages[1:(nage - 1)] * M0)
  N0[nage] <- R0 * exp(-M0 * df$ages[nage]) / (1 - exp(-M0))

  wage_ssb <- df$wage_ssb %>% filter(Yr == df$syr) %>% select(-c(Yr, Fleet))
  SSB_0 <- map_dbl(seq_len(nspace), ~{
    sum(N0 * move_init[.x] * wage_ssb) * 0.5
  }) %>% set_names(paste0(rep('space', each = df$nspace), seq_len(nspace)))

  R_0 <- R0 * move_init
  # Used the inital recruitment devs to get a start
  # Ninit <- rep(NA,nage)
  # Ninit_dev <- (df$parms$initN)
  # Ninit[1] <- R0
  # Ninit[2:(nage-1)] <-R0 * exp(-M[2:(nage-1)]*age[2:(nage-1)])*exp(-0.5*SDR^2*0+Ninit_dev[1:(nage-2)])
  # Ninit[nage] <- R0*exp(-(M[nage-1]*age[nage-1]))/(1-exp(-M[nage]))*exp(-0.5*SDR^2*0+Ninit_dev[nage-1])# Plus group (ignore recruitment dev's in first yrs )
  #
  # Create containers to save the data
  # SSB_init <- NA
  #
  # for(i in 1:nspace){
  #   SSB_init[i] <- sum(df$Matsel*Ninit*move_init[i], na.rm =T)*0.5
  # }
  #
  #Ninit[1] <- sum((4*h*R_0*SSB_init/(SSB_0*(1-h)+ SSB_init*(5*h-1)))*exp(-0.5*1*SDR^2+df$parms$Rin[1]), na.rm = T)
  yrs_1 <- c(yrs,max(yrs)+1)
  SSB <- matrix(NA,nyr, nspace,
                dimnames = list(yrs = df$yrss,
                                space = 1:nspace))
  SSB.all <- array(NA,
                   dim = c(nyr, nspace, nseason),
                   dimnames = list(yrs = yrs,
                                   space = 1:nspace,
                                   season = 1:nseason))
  SSB.weight <- matrix(NA,
                       nyr,
                       nspace,
                       dimnames = list(yrs = yrs,
                                       space = 1:nspace))
  Biomass.save <- matrix(NA,
                         nyr,
                         nspace,
                         dimnames = list(yrs = yrs,
                                         space = 1:nspace))
  Catch <- matrix(NA,
                  nyr,
                  dimnames = list(yrs = yrs))
  Catch.age <- matrix(NA,
                      nage,
                      nyr,
                      dimnames = list(age = age,
                                      yrs = yrs))
  CatchN <- matrix(NA,
                   nyr,
                   dimnames = list(yrs = yrs))
  CatchN.age <- matrix(NA,
                       nage,
                       nyr,
                       dimnames = list(age = age,
                                       yrs = yrs))
  R.save <- matrix(NA,
                   nyr,
                   nspace,
                   dimnames = list(yrs = yrs,
                                   space = 1:nspace))
  Fsel.save <- array(NA,
                     dim = c(nage, nyr, nspace),
                     dimnames = list(age = age,
                                     yrs = yrs,
                                     space = 1:nspace))
  Fseason.save <- array(NA,
                        dim = c(nage, nyr, nspace,nseason),
                        dimnames = list(age = age,
                                        yrs = yrs,
                                        space = 1:nspace,
                                        season = 1:nseason))
  Fout.save <- array(NA,
                     dim = c(nyr, nseason, nspace),
                     dimnames = list(yrs = yrs,
                                     season = 1:nseason,
                                     space = 1:nspace))
  N.save.age <- array(NA,
                      dim = c(nage, nyr + 1, nspace, nseason),
                      dimnames = list(age = age,
                                      yrs = yrs_1,
                                      space = 1:nspace,
                                      season = 1:nseason))
  N.save.age.mid <- array(NA,
                          dim = c(nage, nyr + 1, nspace, nseason),
                          dimnames = list(age = age,
                                          yrs = yrs_1,
                                          space = 1:nspace,
                                          season = 1:nseason))
  R.save <- matrix(NA,
                   nyr,
                   nspace)
  V.save <- array(NA,
                  dim = c(nyr, nspace, nseason),
                  dimnames = list(yrs = yrs,
                                  space = 1:nspace,
                                  season = 1:nseason))
  Catch.save.age <- array(NA,
                          dim = c(nage, nyr, nspace, nseason),
                          dimnames = list(age = age,
                                          yrs = yrs,
                                          space = 1:nspace,
                                          season = 1:nseason))
  CatchN.save.age <- array(NA,dim = c(nage, nyr, nspace, nseason),
                           dimnames = list(age = age,
                                           yrs = yrs,
                                           space = 1:nspace,
                                           season = 1:nseason))
  Catch.quota <- array(NA, dim = c(nyr, nspace, nseason),
                       dimnames = list(yrs = yrs,
                                       space = 1:nspace,
                                       season = 1:nseason))
  Catch.quota.N <- array(0,
                         dim = c(nyr, nspace, nseason),
                         dimnames = list(yrs = yrs,
                                         space = 1:nspace,
                                         season = 1:nseason))
  survey <- array(NA,
                  dim = c(nyr),
                  dimnames = list(yrs = yrs))
  survey.true <- array(NA,
                       dim = c(nspace, nyr),
                       dimnames = list(space = 1:nspace,
                                       yrs = yrs))
  surv.tot <- matrix(NA,
                     nyr,
                     nspace,
                     dimnames = list(yrs = yrs,
                                     space = 1:nspace))
  age_comps_surv <- array(NA,
                          dim = c(df$age_maxage,nyr),
                          dimnames = list(age = 1:df$age_maxage,
                                          yrs = yrs)) #
  age_comps_surv_space <- array(NA,
                                dim = c(df$age_maxage,nyr,nspace),
                                dimnames = list(age = 1:df$age_maxage,
                                                yrs = yrs))
  N.survey <- matrix(NA,
                     df$age_maxage,
                     nyr,
                     dimnames = list(age = 1:df$age_maxage,
                                     yrs= yrs))
  age_comps_catch <- array(NA,
                           dim = c(df$age_maxage,nyr),
                           dimnames = list(age = 1:df$age_maxage,
                                           yrs = yrs))
  age_comps_catch_space <- array(NA,
                                 dim = c(df$age_maxage, nyr, nspace),
                                 dimnames = list(age = 1:df$age_maxage,
                                                 yrs = yrs,
                                                 space = 1:nspace))
  age_comps_OM <- array(NA,
                        dim = c(nage, nyr, nspace,nseason),
                        dimnames = list(age = age,
                                        yrs= yrs,
                                        space = 1:nspace,
                                        season = 1:nseason))
  Z.save <- array(NA,
                  dim = c(df$nage, nyr, nspace, nseason),
                  dimnames = list(age = age,
                                  yrs = yrs,
                                  space = 1:nspace,
                                  season = 1:nseason))
  Z.save[,1,1,1] <- M
  Catch.age[,1] <- 0 # Assumed no fishing before data started
  Catch[1]<- 0
  CatchN[1] <- 0
  CatchN.age[,1] <- 0
  survey[1] <- 1 # Surveys start later
  for(space in 1:nspace){
    survey.true[space,1] <- sum(N.save.age[,1,space,df$surveyseason]*surv_sel*q*df$wage_survey[,1])
  }
  idx.save <- seq(1,t_end, by = nseason)
  # Distribute over space
  Ninit <- rep(NA,nage)
  names(Ninit) <- age
  Ninit_dev <- (df$parms$initN)
  Ninit[2:(nage-1)] <-R0 * exp(-m_age[2:(nage-1)])*exp(-0.5*SDR^2*0+Ninit_dev[1:(nage-2)])
  Ninit[nage] <- R0*exp(-(M[nage]*age[nage]))/(1-exp(-M[nage]))*exp(-0.5*SDR^2*0+Ninit_dev[nage-1])# Plus group (ignore recruitment dev's in first yrs )
  #p.save <-matrix(NA,t_end)
  for(space in 1:nspace){
      # if (season == 1){
      N.save.age[,1,space,1] <- Ninit*move_init[space] # Just to initialize
      N.save.age.mid[,1,space,1] <- N.save.age[,1,space,1]*exp(-0.5*(M/nseason))
      # }else{
      #   N.save.age[,1,space,season] <- N.save.age[,1,space,season-1]*exp(-M/nseason)
      #   N.save.age.mid[,1,space,season] <- N.save.age[,1,space,season]*exp(-0.5*(M/nseason))
      # }
      # }
  }
  Fspace <- c(0.2612,0.7388) # Contribution of Total catch (add to one)    #Z <- (Fyrs+Myrs)
  Fnseason <- df$Fnseason
  pope.mul <- nseason/1*0.5
  pope.mul <- 0.50
  if(nseason == 1){
    Fnseason <- matrix(rep(1, df$nspace))
  }
  for(yr in 1:nyr){ # Loop over yrss add one yrs for initial distribution
    #if(yrs[yr] < yrs[df$selyrs] | yrs[yr] > 2017){
    # }else{
    #   psel <- df$parms$psel_fish+df$parms$PSEL[,yr-df$selyrs+1]
    # }
    if(yrs[yr] < 2019){
      w_catch <- df$wage_catch[,yr]
      w_surv <- df$wage_survey[,yr]
      w_mid <- df$wage_mid[,yr]
      w_ssb <- df$wage_ssb[,yr]
    }else{
      w_catch <- df$wage_catch[,1]
      w_surv <- df$wage_survey[,1]
      w_mid <- df$wage_mid[,1]
      w_ssb <- df$wage_ssb[,1]
    }
    Ry <- df$parms$Rin[yr]
    # Fyrs <- F0[yr]*Fsel
    Myrs <- M # Natural mortality
    ## add these to load data seasons
    # Fnseason <- matrix(1, nseason)
    # Fnseason <- Fnseason/sum(Fnseason)
    # Fnseason <- c(0,0.5,0.5,0)
    if(df$move == FALSE){
      Fspace <- 1 # All catches in the south
    }
    Mseason <- Myrs/nseason # M is distributed throughout the yrs
    # fix Ssb and recruitment in all areas
    for(space in 1:nspace){
      SSB.weight[yr,space] <- sum(N.save.age[,yr,space,1]*as.numeric(w_ssb), na.rm = TRUE)*0.5
      SSB[yr,space] <- SSB.weight[yr,space] #sum(N.save.age[,yr,space,1]*mat_sel, na.rm = TRUE)
      SSB.all[1,space,1]<- sum(N.save.age[,1,space,1]*mat_sel, na.rm = TRUE)*0.5
      # Recruitment only in season 1
      R <- (4*h*R_0[space]*SSB[yr,space]/
              (SSB_0[space]*(1-h)+ SSB[yr,space]*(5*h-1)))*exp(-0.5*df$b[yr]*SDR^2+Ry)#*recruitmat[space]
      N.save.age[1,yr,space,1] <- R
      R.save[yr,space] <- R
    }
    for(season in 1:nseason){
      for(space in 1:nspace){
        # Get the selectivity of the season and area
        psel <- df$psel[space,]
        if(df$flag_sel[yr] == 1){
          pseltmp <- psel+df$parms$PSEL[,yr-df$selidx+1]*df$sigma_psel
        }else{
          pseltmp <- psel
        }
        if(yrs[yr] >2018){
          # if(df$selectivity_change == 0){
          #   if(space == 1){
          #     pseltmp <- c(1,1,1,1,1)
          #   }else{
          #   pseltmp <- psel
          #   }
          # }
          if(df$selectivity_change ==1){
            if(space == 1){
              #pseltmp <- psel
              pseltmp <- c(1,1,1,1,1)
            }else{
              pseltmp <- c(0.05,0.05,0,0,0)
            }
          }
          if(df$selectivity_change ==2){
            pseltmp <- df$psel[2,]+df$parms$PSEL[,ncol(df$parms$PSEL)]*df$sigma_psel}
        }
        #p.save[yr] <- sum(pseltmp)
        #
        Fsel <- getSelec(age,pseltmp,df$Smin,df$Smax) # Constant over space right now
        rm(pseltmp)
        Fsel.save[,yr,space] <- Fsel
        if(nspace > 1){
          if(df$yrss[yr]<= 2018){
            Catch_space <- df$Catch.country[yr,space]
          }else{
            Catch_space <- df$Catch[yr]*Fspace[space]
          }
        }else{
          Catch_space <- df$Catch[yr]
        }
        E.temp <- Catch_space*Fnseason[space, season]#*Fspace[space] # Catch distribution in the yrs
        B.tmp <-  sum(N.save.age[,yr,space,season]*exp(-Mseason*pope.mul)*w_catch*Fsel) # Get biomass from previous yrs
        N.tmp <- N.save.age[,yr,space,season]#
        V.save[yr,space,season] <- B.tmp
        Catch.quota[yr,space,season] <- E.temp
        if(E.temp/B.tmp >= .9){
          if(df$yrss[yr] < 2018){
            stop(paste('Catch exceeds available biomass in yrs:',yrs,' and season', season, 'area', space)) # Stop if in the past
          }
          #print(paste('Catch exceeds available biomass in yrs:',yrs,' and season', season, 'area', space))
          E.temp <- 0.75*B.tmp
          Catch.quota.N[yr,space,season] <- 1
          #if(df$yrss[yr] > 2026){
          #stop('danger')
          #  }
        }
        Fout <- getF(E.temp,
                     B.tmp,
                     Mseason = Mseason,
                     Fsel = Fsel,
                     Ntemp = N.tmp,
                     w_catch = w_catch,
                     method = 'Hybrid')
        Fout <- Fout
        #Fout <- df$parms$F0[yr]

        if(E.temp>0){
          Fseason <- Fout*Fsel
          Fnew <- Fout
          Z <- Fnew*Fsel+Mseason
          Fseason <- Fnew*Fsel
        }else{
          Fseason <- 0
        }
        Fout.save[yr,season,space] <- Fout # terminal fishing mortality
        Fseason.save[,yr,space,season] <- Fseason
        Z <- Mseason+Fseason
        Z.save[,yr,space,season]<- Z
        if(((space - 1) == 0)){
          spaceidx <- 2
        }
        if(space == nspace){
          spaceidx <- nspace-1
        }
        if(space > 1 & space < nspace){
          spaceidx <- c(space-1,space+1)
        }
        if(!df$move){
          spaceidx <- 1
        }
        if(season <nseason){
          N.save.age[,yr,space,season+1] <- N.save.age[,yr,space,season]*exp(-Z)-
            N.save.age[, yr,space,season]*exp(-Z)*(movemat[space,,season,yr])+ # Remove the ones that leave
            N.save.age[, yr,spaceidx,season]*exp(-Z)*(movemat[spaceidx,,season,yr])# add the ones come to the surrounding areas
          age_comps_OM[,yr,space,season] <- N.save.age[, yr,space,season]/sum(N.save.age[, yr,space,season])
          SSB.all[yr,space,season]<- sum(N.save.age[,yr,space,season]*mat_sel, na.rm = T)
          Catch.save.age[, yr,space, season] <- (Fseason/(Z))*(1-exp(-(Z)))*N.save.age[,yr,space,season]*w_catch
          CatchN.save.age[, yr,space, season] <- (Fseason/(Z))*(1-exp(-(Z)))*N.save.age[,yr,space,season]
        }else{
          N.save.age[2:(nage-1),yr+1,space,1] <- N.save.age[1:(nage-2),yr,space,season]*exp(-Z[1:(nage-2)])-
            N.save.age[1:(nage-2), yr,space,season]*exp(-Z[1:(nage-2)])*(movemat[space,1:(nage-2),season,yr])+ # Remove the ones that leave
            N.save.age[1:(nage-2), yr,spaceidx,season]*exp(-Z[1:(nage-2)])*(movemat[spaceidx,1:(nage-2),season,yr])# add the ones come to the surrounding areas

          # Plus group
          Nsurvive.plus <- (N.save.age[nage-1, yr,space, nseason]*exp(-Z[nage-1])+
                              N.save.age[nage, yr,space, nseason]*exp(-Z[nage]))
          Nout.plus <- Nsurvive.plus*(movemat[space,nage, season,yr]) # Leaving
          Nin.plus <- (N.save.age[nage-1, yr,spaceidx,nseason]*exp(-Z[nage-1])+
                         N.save.age[nage, yr,spaceidx,nseason]*exp(-Z[nage]))*
            (movemat[spaceidx,nage, season,yr]) # Incoming
          N.save.age[nage,yr+1,space,1] <- Nsurvive.plus- Nout.plus + Nin.plus
          age_comps_OM[,yr,space,season] <- N.save.age[, yr,space,season]/sum(N.save.age[, yr,space,season])
          SSB.all[yr,space,season]<- sum(N.save.age[,yr,space,season]*mat_sel, na.rm = T)
          Catch.save.age[, yr,space, season] <- (Fseason/(Z))*(1-exp(-(Z)))*N.save.age[,yr,space,season]*w_catch
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
    #Catch.age[,idx]  <- (Fyrs/(Fyrs+Myrs))*(1-exp(-(Fyrs+Myrs)))*rowSums(N.save.age[,idx,,1])*w_catch # Calculate the catch in kg
    if(nseason>1){
      Catch.age[,yr] <- apply(Catch.save.age[,yr,,],MARGIN = 1,FUN = sum)
      Catch[yr] <- sum(Catch.save.age[,yr,,])

      CatchN.age[,yr] <- apply(CatchN.save.age[,yr,,],MARGIN = 1,FUN = sum)
      CatchN[yr] <- sum(CatchN.save.age[,yr,,])
    }else{

      if(nspace == 1){
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
    if(nseason == 1){
      Msurveymul <- 0.5
    }else{
      Msurveymul <- 0
    }
    for (space in 1:nspace){
      survey.true[space,yr] <- sum(N.save.age[,yr,space,df$surveyseason]*
                                     exp(-Msurveymul*Z.save[,yr,space,df$surveyseason])*surv_sel*q*w_surv)
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
        surv <- exp(log(sum(Nsurv*surv_sel*q*w_surv))+err) # If the xtra factor is not included the mean is > 1
      }else{
        surv <- sum(Nsurv*surv_sel*q*w_surv)
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
      age_comps_surv[df$age_maxage,yr] <- sum(Ntot.yrs[(df$age_maxage+1):nage]*surv_sel[(df$age_maxage+1):nage]*q)/surv.tmp
    }else{
      age_comps_surv[,yr] <- NA
    }
    for(space in 1:nspace){
      Ntot.yrs <- N.save.age[,yr,space,df$surveyseason]
      surv.tot[yr,space]  <- sum(Ntot.yrs*surv_sel*q*exp(-Msurveymul*Z.save[,yr,space,df$surveyseason]))
      age_comps_surv_space[1,yr,space] <- 0 # No yrs 1 recorded
      age_comps_surv_space[1:(df$age_maxage-1),yr,space] <-
        (Ntot.yrs[2:(df$age_maxage)]*surv_sel[2:(df$age_maxage)]*q)/surv.tot[yr,space]
      age_comps_surv_space[df$age_maxage,yr,space] <-
        sum(Ntot.yrs[(df$age_maxage+1):nage]*surv_sel[(df$age_maxage+1):nage]*q)/surv.tot[yr,space]
      if(nseason>1){
        Catch.tmp <- rowSums(CatchN.save.age[, yr,space,])
      }else{
        Catch.tmp <- CatchN.save.age[, yr,space,]
      }
      Catch.tot <- sum(CatchN.save.age[,yr,space,])
      age_comps_catch_space[1:(df$age_maxage-1),yr,space] <- Catch.tmp[2:(df$age_maxage)]/Catch.tot
      age_comps_catch_space[df$age_maxage,yr,space] <- sum(Catch.tmp[(df$age_maxage+1):nage])/Catch.tot
    }
    if(df$flag_catch[yr] == 1){
      age_comps_catch[1:(df$age_maxage-1),yr] <-  CatchN.age[2:(df$age_maxage),yr]/CatchN[yr]
      age_comps_catch[df$age_maxage,yr] <- sum(CatchN.age[(df$age_maxage+1):nage,yr])/CatchN[yr]
    }else{
      age_comps_catch[,yr] <- NA
    }
  }# End of yrs loop
  if(!df$move){
    Nsave <- N.save.age[,,,nspace]
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
                   SSB_0 = SSB_0,
                   N0 = N0,
                   SSB.weight = SSB.weight,
                   survey.true = survey.true,
                   Z = Z.save,
                   survey = as.numeric(survey),
                   age_comps_surv = age_comps_surv,
                   age_comps_country = age_comps_surv_space,
                   age_comps_catch_space = age_comps_catch_space,
                   Fseason = Fseason.save,
                   Fsel = Fsel.save,
                   Ninit = Ninit,
                   SSB0 = SSB_0)

  df.out
}

