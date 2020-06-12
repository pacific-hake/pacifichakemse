#' Update the OM data for a new year
#'
#' @param yr The year to add
#' @param sim_data Operating model
#' @param df Input data and parameters
#' and loaded by [load_ss_model_from_rds()]
#'
#' @return A list of the data needed by [TMB::MakeADFun()]
#' @importFrom stringr str_split
#' @export
update_om_data <- function(yr = NULL,
                           yr_ind,
                           yr_survey_sims,
                           sim_data = NULL,
                           df = NULL){

  verify_argument(yr, c("integer", "numeric"), 1)
  verify_argument(sim_data, "list")
  verify_argument(df, "list")

  if(yr <= df$m_yr + 1){
    return(df)
  }
  df$flag_survey <- c(df$flag_survey, ifelse(yr %in% yr_survey_sims, 1, -1))
  df$ss_survey <- c(df$ss_survey, ifelse(yr %in% yr_survey_sims,
                                         ceiling(mean(df$ss_survey[df$ss_survey > 0])),
                                         0))
  df$survey_err <- c(df$survey_err, ifelse(yr %in% yr_survey_sims,
                                           mean(df$survey_err[df$survey_err < 1]),
                                           1))
  df$ss_catch <- c(df$ss_catch, ceiling(mean(df$ss_catch[df$ss_catch > 0])))
  df$flag_catch <- c(df$flag_catch, 1)

  # Copy last year of weight-at-age data and use that as the simulated year
  df$wage_catch_df <- wage_add_yr(df$wage_catch_df)
  df$wage_survey_df <- wage_add_yr(df$wage_survey_df)
  df$wage_mid_df <- wage_add_yr(df$wage_mid_df)
  df$wage_ssb_df <- wage_add_yr(df$wage_ssb_df)
  browser()
  if(df$catch_obs[yr_ind] == 0 & df$flag_survey[yr_ind] == -1){
    red(message("Stock in peril! Conducting emergency survey"))
    df$flag_survey[yr_ind] <- 1
    # Emergency survey adds 200 more age samples onto the mean value for all surveys,
    # because it is an emergency survey the hypothesis is that they will sample more
    df$ss_survey[yr_ind] <- ceiling(mean(df$ss_survey[df$ss_survey > 0])) + 200
    df$survey_err[yr_ind] <- mean(df$survey_err[df$survey_err < 1])
  }

  df$b[length(df$b)] <- df$bfuture
  df$b <- c(df$b,df$bfuture)
  Rdevs <- rnorm(n = 1,mean = 0, sd = exp(df$logSDR))
  #Rdevs <- rep(0, yr.future)
  df$parms$Rin <- c(df$parms$Rin,Rdevs)


### Add movement to the new years
move.tmp <- array(0, dim = c(df$nspace,df$nage, df$nseason, df$nyear))
move.tmp[,,,1:df$nyear-1] <- df$movemat

# Add increasing movement due to climate
nspace <- df$nspace
nage <- df$nage
nseason <- df$nseason
age <- df$age


movenew <-array(0, dim = c(nspace, nage, nseason)) # Chances of moving in to the other grid cell
movemax <- df$movemax
movefifty <- df$movefifty

if(time == 2){
  movemaxtmp <- (movemax[1]+cincrease)
  df$moveout <- df$moveout-mincrease
}else{
  movemaxtmp <- movemaxtmp+cincrease
  df$moveout <- df$moveout-mincrease

}

if(movemaxtmp >0.9){
  movemaxtmp <- 0.9 # Not moving more than 90% out

  # (stop(paste(time,'at max movement')))
}

if(df$moveout <= 0.5){
  df$moveout <- 0.5
}

for(j in 1:nspace){
  for(i in 1:nseason){
    movenew[j,,i] <- movemaxtmp/(1+exp(-df$moveslope*(age-movefifty)))
  }
}

if(nseason == 4){ # For the standard model
  movenew[,1:2,] <- 0 # Recruits and 1 year olds don't move

  movenew[1,4:nage,1:3] <- df$movesouth # Don't move south during the year
  movenew[1,3:nage,nseason] <- df$moveout
  movenew[2,3:nage,nseason] <- df$movesouth
}

move.tmp[,,,df$nyear] <- movenew
df$movemat <- move.tmp
#df$years <- c(df$years,df$years[length(df$years)]+1)

# Fix the selectivity
if(sel_change == 0){
  df$flag_sel <- c(df$flag_sel,0)
}
if(sel_change == 1){
  flag.tmp <- c(df$flag_sel,1)
  idx <- which.min(df$flag_sel[df$flag_sel == 1])
  flag.tmp[idx] <- 0
  df$flag_sel <- flag.tmp
  df$selidx <- df$selidx+1

}
if(sel_change == 2){
  df$flag_sel <- c(df$flag_sel,1)
  df$parms$PSEL <- rbind(df$parms$PSEL,rep(0,nrow(df$parms$PSEL)))
}


  df
}