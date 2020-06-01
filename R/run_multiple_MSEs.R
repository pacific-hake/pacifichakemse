#' Run/Iterate the Pacific hake MSE
#'
#' @param n_sim_yrs Number of years to simulate
#' @param ss_model SS3 model output as created by [create_rds_file()]
#' and loaded by [load_ss_model_from_rds()]
#' @param sim_data Operating model as created by [run_agebased_true_catch()]
#' @param om_params_seed Seed for running the OM if it needs to be run (if `sim_data`
#' is `NULL`)
#' @param seed The random number seed to use for the MSE run
#' @param tac Which harvest control rule should the model use
#' @param df Data frame of parameters as output by [load_data_seasons()]
#' @param c_increase Increase in max movement
#' @param m_increase Decrease of spawners returning south
#' @param sel_change Time varying selectivity
#' @param ... Absorb arguments intended for other functions
#'
#' @return A list of Catch, Catch.quota, SSB, SSB.mid, SSB.hes, Survey.om
#' F0, parms, N, converge, ams, amc, V
#' @importFrom TMB sdreport MakeADFun
#' @importFrom stats rnorm nlminb runif predict lm median optim setNames
#' @importFrom utils read.csv read.table
#' @export
run_multiple_MSEs <- function(df = NULL,
                              ss_model = NULL,
                              sim_data = NULL,
                              om_params_seed = 12345,
                              n_sim_yrs = NULL,
                              seed = 12345,
                              tac = 1,
                              c_increase = 0,
                              m_increase = 0,
                              sel_change = 0,
                              ...){
  verify_argument(df, "list")
  verify_argument(ss_model, "list")
  verify_argument(om_params_seed, "numeric", 1)
  verify_argument(n_sim_yrs, "numeric", 1)
  verify_argument(seed, "numeric", 1)
  verify_argument(tac, "numeric", 1)
  verify_argument(c_increase, "numeric", 1)
  verify_argument(m_increase, "numeric", 1)
  verify_argument(sel_change, "numeric", 1)

  if(is.null(sim_data)){
    if(is.null(om_params_seed)){
      stop("To run the agebased OM, you must supply `om_params_seed`",
           call. = FALSE)
    }
    sim_data <- run_agebased_true_catch(df, om_params_seed, ...)
  }

  yr_start <- df$yrs[df$n_yr] + 1
  yr_end <- df$yrs[df$n_yr] + n_sim_yrs
  yr_sims <- yr_start:yr_end
  yr_all <- c(df$yrs, yr_sims)

  # Save the estimated parameters from the EM (exclude time varying)
  # em_parms_save <- array(NA, dim = c(n_sim_yrs, 4))
  # f40_save <- array(NA, n_sim_yrs)
  # ssb_save <- list()
  # r_save <- list()
  # catch_save <- list()

  # Calculate survey years, where odd years are survey years
  first_sim_surv_yr <- ifelse(yr_start %% 2 == 1, yr_start, yr_start + 1)
  yr_survey_sims <- seq(first_sim_surv_yr, yr_start + n_sim_yrs, by = df$n_survey)
  # Remove any survey years not included in the simulated years
  yr_survey_sims <- yr_survey_sims[yr_survey_sims %in% yr_sims]

  df <- create_TMB_data(sim_data, df, ss_model)

  params_new <- df$parms_init
  params_new$f_0 <- rowSums(sim_data$f_out_save)
  params_new$r_dev <- df$parms_init$r_in
  if(df$catch[df$n_yr] == 0){
    params_new$f_0[length(params_new$f_0)] <- 0
  }
  # Convert some parameter objects to base types
  params_new$p_sel_fish <- params_new$p_sel_fish %>%
    pull(value)
  params_new$p_sel_surv <- params_new$p_sel_surv %>%
    pull(value)
  params_new$init_n <- params_new$init_n %>%
    pull(value)

browser()
  obj <-MakeADFun(df, params_new, DLL = "runHakeassessment", silent = TRUE)
browser()
  # Modify survey objects in the simulated survey years and add catch for new year
  map(yr_sims, function(yr = .x){
    yr_ind <- which(yr == yr_all)
    df$flag_survey <- c(df$flag_survey, ifelse(yr %in% yr_survey_sims, 1, -1))
    df$survey_x <- c(df$survey_x, ifelse(yr %in% yr_survey_sims, 2, -2))
    df$ss_survey <- c(df$ss_survey, ifelse(yr %in% yr_survey_sims,
                                           ceiling(mean(df$ss_survey[df$ss_survey > 0])),
                                           0))
    df$survey_err <- c(df$survey_err, ifelse(yr %in% yr_survey_sims,
                                             mean(df$survey_err[df$survey_err < 1]),
                                             1))
    df$ss_catch <- c(df$ss_catch, ceiling(mean(df$ss_catch[df$ss_catch > 0])))
    df$flag_catch <- c(df$flag_catch, 1)
    # Add a survey if catches are 0
    browser()
    if(df$catch[yr_ind] == 0 & df$flag_survey[yr_ind] == -1){
      message("Stock in peril! Conducting emergency survey")
      df$flag_survey[df$catch == 0] <- 1
      # Emergency survey adds more 200 age samples
      df$ss_survey[df$catch == 0] <- ceiling(mean(df$ss_survey[df$ss_survey > 0])) + 200
      df$survey_x[df$catch == 0] <- 2
      df$survey_err[df$catch == 0] <- mean(df$survey_err[df$survey_err < 1])
    }
  })

  #       From getRefPoint()
  #       df$Catch <- c(df$Catch, Fnew[[1]])
  #
  browser()
}

#
#       df$b[length(df$b)] <- df$bfuture
#       df$b <- c(df$b,df$bfuture)
#       Rdevs <- rnorm(n = 1,mean = 0, sd = exp(df$logSDR))
#       #Rdevs <- rep(0, yr.future)
#       df$parms$Rin <- c(df$parms$Rin,Rdevs)
#
#
#       ### Add movement to the new years
#       move.tmp <- array(0, dim = c(df$nspace,df$nage, df$nseason, df$nyear))
#       move.tmp[,,,1:df$nyear-1] <- df$movemat
#
#       # Add increasing movement due to climate
#       nspace <- df$nspace
#       nage <- df$nage
#       nseason <- df$nseason
#       age <- df$age
#
#
#       movenew <-array(0, dim = c(nspace, nage, nseason)) # Chances of moving in to the other grid cell
#       movemax <- df$movemax
#       movefifty <- df$movefifty
#
#       if(yr == 2){
#         movemaxtmp <- (movemax[1]+c_increase)
#         df$moveout <- df$moveout-m_increase
#       }else{
#         movemaxtmp <- movemaxtmp+c_increase
#         df$moveout <- df$moveout-m_increase
#
#       }
#
#       if(movemaxtmp >0.9){
#         movemaxtmp <- 0.9 # Not moving more than 90% out
#
#         # (stop(paste(yr,'at max movement')))
#       }
#
#       if(df$moveout <= 0.5){
#         df$moveout <- 0.5
#       }
#
#       for(j in 1:nspace){
#         for(i in 1:nseason){
#           movenew[j,,i] <- movemaxtmp/(1+exp(-df$moveslope*(age-movefifty)))
#         }
#       }
#
#       if(nseason == 4){ # For the standard model
#         movenew[,1:2,] <- 0 # Recruits and 1 year olds don't move
#
#         movenew[1,4:nage,1:3] <- df$movesouth # Don't move south during the year
#         movenew[1,3:nage,nseason] <- df$moveout
#         movenew[2,3:nage,nseason] <- df$movesouth
#       }
#
#       move.tmp[,,,df$nyear] <- movenew
#       df$movemat <- move.tmp
#       #df$years <- c(df$years,df$years[length(df$years)]+1)
#
#       # Fix the selectivity
#       if(sel_change == 0){
#         df$flag_sel <- c(df$flag_sel,0)
#       }
#       if(sel_change == 1){
#         flag.tmp <- c(df$flag_sel,1)
#         idx <- which.min(df$flag_sel[df$flag_sel == 1])
#         flag.tmp[idx] <- 0
#         df$flag_sel <- flag.tmp
#         df$selidx <- df$selidx+1
#
#       }
#       if(sel_change == 2){
#         df$flag_sel <- c(df$flag_sel,1)
#         df$parms$PSEL <- rbind(df$parms$PSEL,rep(0,nrow(df$parms$PSEL)))
#       }
#
#       #print(df$moveout)
#       sim_data <- run_agebased_true_catch(df, seeds)
#
#     }
#
#     # PSEL <- matrix(0,5, length(1991:years[length(years)]))
#     # initN <- rep(0,df$nage-1)
#     # F0 <- rep(0.01, df$nyear)
#     # Rdev <- rep(0, df$nyear-1) # Can't predict last year
#     #
#     # parms <- list( # Just start all the simluations with the same initial conditions
#     #   logRinit = 15,
#     #   logh = log(0.5),
#     #   logMinit = log(0.3),
#     #   logSDsurv = log(0.3),
#     #   logphi_catch = log(0.8276),
#     #   logphi_survey = log(11.33),
#     #   # Selectivity parameters
#     #   psel_fish = c(2.486490, 0.928255,0.392144,0.214365,0.475473),
#     #   psel_surv = c(0.568618,-0.216172,0.305286 ,0.373829),
#     #   initN = initN,
#     #   Rin = Rdev,
#     #   F0 = F0,
#     #   PSEL = PSEL
#     # )
#     #
#
#     #parms <- getParameters(trueparms = TRUE, mod,df = df)
#     parms <- df$parms
#     ##  Create a data frame to send to runHakeassessment
#
#     df_new <- create_TMB_data(sim_data, df)
#
#     parms.new <- parms
#
#     if(yr == 1){
#       F0 <- rowSums(sim_data$Fout)
#       Rdev <- parms$Rin[1:(length(parms$Rin)-1)]
#     }else{
#       F0 <- c(F0,0.2)
#       Rdev <- c(Rdev, 0)
#     }
#
#
#     if(df$Catch[df$nyear] == 0){
#       F0[length(F0)] <- 0
#     }
#
#     parms.new$F0 <- F0#rowSums(sim_data$Fsave, na.rm = TRUE)
#     #parms.new$F0[df$Catch == 0] <- 0
#
#     parms.new$Rin <- Rdev#parms.new$Rin[1:(length(parms.new$Rin)-1)]
#
#
#
#     obj <-TMB::MakeADFun(df_new,parms.new,DLL="runHakeassessment", silent = TRUE) # Run the assessment
#
#     reps <- obj$report()
#
#     lower <- obj$par-Inf
#     upper <- obj$par+Inf
#
#     #lower[names(lower) == 'logSDsurv'] <- 0.05
#
#     upper <- obj$par+Inf
#     #upper[names(upper) == 'psel_fish' ] <- 5
#     #upper[names(upper) == 'PSEL'] <- 5
#     upper[names(upper) == 'logh'] <- log(0.999)
#     upper[names(upper) == 'F0'] <- 2
#     #upper[names(upper) == "logphi_survey"]<- log(8)
#     lower[names(lower) == 'logSDsurv'] <- log(0.01)
#     lower[names(lower) == 'F0'] <- 0.01
#     #lower[names(lower) == 'logMinit'] <- log(0.2)
#     if(df$Catch[length(df$Catch)] == 1){
#       lower[names(lower) == 'F0'] <- 1e-10
#     }
#
#
#     # if(df$Catch[df$nyear] == 0){
#     #   lower[names(lower) =='F0'][which(df$Catch ==0)] <- 0
#     #   upper[names(upper) =='F0'][which(df$Catch ==0)] <- 0
#     # }
#     #
#
#     system.time(opt<-nlminb(obj$par,obj$fn,obj$gr,lower=lower,upper=upper,
#                             control = list(iter.max = 1e6,
#                                            eval.max = 1e6))) # If error one of the random effects is unused
#
#
#     # if(opt$convergence != 0){
#     #   print(paste('year',df$years[length(df$years)], 'has convergence issues, double checking parameters'))
#     #
#     #   xx<- Check_Identifiable_vs2(obj)
#     #
#     #   # if(xx[[1]] == 'model not converging'){
#     #   #   mconverge[time] <- 1
#     #   #
#     #   #   if(sum(mconverge, na.rm = TRUE)>1){
#     #   #     print('Convergence is bad. Stopping simulation')
#     #   #     return(df.ret= NA)
#     #   #   }
#     #   #
#     #   # }else{
#     #   #   mconverge[time] <- 0
#     #   # }
#     # }else{
#     #   mconverge[time] <- 0
#     # }
#
#     reps <- obj$report()
#
#     SSB <- reps$SSB
#     Fyear <- reps$Fyear
#     N <- reps$N_beg
#     Catch <- reps$Catch
#     R <- reps$R
#
#     # plot(sim_data$Catch)
#     # lines(df$Catch)
#     #
#     # plot(reps$SSB)
#     # lines(rowSums(sim_data$SSB), col = 'red')
#     # # # #Uncertainty
#
#
#     if(yr == n_sim_yrs){
#       rep <- sdreport(obj)
#       sdrep <- summary(rep)
#       rep.values<-rownames(sdrep)
#       nyear <- df$tEnd
#
#       # Check convergence in last year
#       conv <- Check_Identifiable_vs2(obj)
#
#       if(length(conv$WhichBad) == 0){
#         mconverge <- 1
#       }else{
#         mconverge <- 0
#       }
#
#
#       R <- data.frame(value = sdrep[rep.values == 'R',1])
#
#       SSB <- data.frame(value = sdrep[rep.values == 'SSB',1])
#       SSB$SE <- sdrep[rep.values == 'SSB',2]
#       SSB$min <- SSB$value-2*SSB$SE
#       SSB$max <- SSB$value+2*SSB$SE
#       SSB$year <- df$years
#
#       if(is.na(df$move)){
#         p1 <- plotValues(SSB, data.frame(x=df$years,y = sim_data$SSB), 'SSB')
#       }else{
#         p1 <- plotValues(SSB, data.frame(x=df$years,y = rowSums(sim_data$SSB)), 'SSB')
#       }
#
#       print(p1)
#       SSB.hes <- SSB
#       SSB <- SSB$value
#
#     }
#
#
#     Vreal <- sum(sim_data$N.save.age[,df$nyear,,df$nseason]*
#                    matrix(rep(df$wage_catch[,df$nyear-1],df$nspace), ncol = df$nspace)*(sim_data$Fsel[,df$nyear,]))
#
#     Nend <- N[,dim(N)[2]]
#     Fnew <- getRefpoint(opt$par,
#                         df,
#                         ssb_y = SSB[length(SSB)],
#                         f_in =Fyear[length(Fyear)],
#                         Nend,
#                         tac = tac,
#                         Vreal)
#     #Fnew <- 0.3
#     # Update the data data frame
#     # if(Fnew[[1]] == 1){
#     #   stop('fishery closed')
#     # }
#     #
#     Ntmp <- sim_data$Nout
#
#
#     # Save some EM stuff in the last year
#     ssb_save[[yr]] <- SSB
#     r_save[[yr]] <- N[1,]
#     f40_save[yr] <- Fnew[[2]]
#     catch_save[[yr]] <- Catch
#
#     # And the fishing mortality
#     #F0.save <- Fnew
#
#     #  print(yr_all[year])
#     #SSB.test.om[[yr]] <- rowSums(sim_data$SSB)
#
#     ### Include the parameters needed to calculate SSB0
#     em_parms_save[yr, ] <- exp(opt$par)[1:4]
#
#     # year = df$years[yr]
#
#
#   }
#
#   nms <- unlist(strsplit(names(opt$par), split = 'log')[1:4])[c(2,4,6,8)]
#   names(em_parms_save) <- nms
#
#
#   end.time <- Sys.time()
#   time.taken <- end.time - start.time
#   print(time.taken)
#
#   #  Catch per country per year
#   #Catch.year <- apply(sim_data$catch_save.age, FUN = sum, MARGIN = c(2,3))
#
#   Catch.year <- sim_data$catch_save.age
#   ## Calculate the average age
#
#   #dev.off()
#
#   amc <- data.frame(year = yr_all[1:year],
#                     amc.can = calcMeanAge(sim_data$age_comps_catch_space[,,1], df$age_maxage),
#                     amc.US  = calcMeanAge(sim_data$age_comps_catch_space[,,2], df$age_maxage),
#                     amc.tot = calcMeanAge(sim_data$age_catch, df$age_maxage))
#
#   ams <- data.frame(year = yr_all[1:year],
#                     ams.can = calcMeanAge(sim_data$age_comps_country[,,1], df$age_maxage),
#                     ams.US  = calcMeanAge(sim_data$age_comps_country[,,2], df$age_maxage),
#                     ams.tot = calcMeanAge(sim_data$age_comps_surv, df$age_maxage))
#
#   df.ret <- list(Catch = Catch.year,
#                  Catch.quota = sim_data$Catch.quota,# All output is from the OM
#                  SSB = sim_data$SSB,
#                  SSB.mid = sim_data$SSB.all[,,3],
#                  SSB.hes = SSB.hes,
#                  Survey.om = sim_data$survey,
#                  F0 = apply(sim_data$Fout,c(1,3),sum),
#                  parms = em_parms_save,
#                  N = sim_data$N.save.age,
#                  converge = mconverge,
#                  ams = ams,
#                  amc = amc,
#                  V = sim_data$V.save)
#
#   df.ret
# }
