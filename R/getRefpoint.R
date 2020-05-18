#' Get reference points (TODO: improve docs on this function)
#'
#' @param par_fixed estimated parameters
#' @param df data frame of non-estimated parameters
#' @param ssb_y Spawning biomass
#' @param f_in Fishing mortality
#' @param n_end Numbers at age
#' @param tac which tac function to use?
#' @param v_real Vulnerable biomass in OM
#'
#' @return A list of reference points
#' @export
getRefpoint <- function(par_fixed,
                        df,
                        ssb_y,
                        f_in = NA,
                        n_end,
                        tac = 1,
                        v_real = NA){

  R0 <- as.numeric(exp(par_fixed)['logRinit'])
  Mest <- as.numeric(exp(par_fixed)['logMinit'])
  h <- as.numeric(exp(par_fixed)['logh'])
  p_sel <- as.numeric(par_fixed[which(names(par_fixed) == 'psel_fish')])
  f_sel <- get_select(df$age,
                      p_sel,
                      df$s_min,
                      df$s_max)


  Cw <- as.numeric(df$wage_catch[,dim(df$wage_catch)[2]])

  M <- rep(Mest, df$nage)
  nage <- df$nage
  age <- 0:(nage-1)

  Mage <- M #cumsum(M)
  # N0 <- NA
  # N0[1] <- R0
  # N0[2:(nage-1)] <-R0 * exp(-Mage[1:(nage-2)]*age[2:(nage-1)])
  # N0[nage] <- R0*exp(-(Mage[nage-1]*age[nage-1]))/(1-exp(-M[nage]))# Plus group

  N0 <- NA
  N0[1] <- R0
  for(a in 1:(nage-1)){
    N0[a+1] <- N0[a]*exp(-Mage[a])
  }
  N0[nage] <- N0[nage]/(1-Mage[nage])

  SSB.age <- df$Matsel*N0*0.5 # In G
  SSB_0 <- sum(SSB.age)

  SSB_pr<- SSB_0/R0

  SBeq <- 4*h*R0*0.4*SSB_0-SSB_0*(1-h)/(5*h-1)
  #
  #
  # Z <- M+f_in*sel
  # Zage <- Z#cumsum(M)+cumsum(f_in*sel)
  # N1 <- NA
  # N1[1] <- R0
  # N1[2:(nage-1)] <-R0 * exp(-Zage[2:(nage-1)]*age[2:(nage-1)])
  # N1[nage] <- R0*exp(-(Zage[nage-1]*age[nage-1]))/(1-exp(-Z[nage]))# Plus group (ignore recruitment dev's in first year )

  Z <- M+f_in*sel
  Zage <- Z
  N1 <- NA
  N1[1] <- R0
  for(a in 1:(nage-1)){
    N1[a+1] <- N1[a]*exp(-Zage[a])
  }
  # adjust plus group sum of geometric series as a/(1-r)
  N1[nage] <- N1[nage]/(1-Zage[nage])

  SSB_eq <- sum(df$Matsel*N1)*0.5

  SPR <- SSB_eq/SSB_0
  #SPR <- ssb_y/SSB_0
  #print(SPR)
  # ## Calculate the F40 reference point

  getF <- function(par){
    Z <- M+par[1]*sel
    # Zage <- Z
    # N1 <- NA
    # N1[1] <- R0
    # N1[2:(nage-1)] <-R0 * exp(-Zage[2:(nage-1)]*age[2:(nage-1)])
    # N1[nage] <- R0*exp(-(Zage[nage-1]*age[nage-1]))/(1-exp(-M[nage]))# Plus group (ignore recruitment dev's in first year )

    Z <- M+par[1]*sel
    Zage <- Z
    N1 <- NA
    N1[1] <- R0
    for(a in 1:(nage-1)){
      N1[a+1] <- N1[a]*exp(-Zage[a])
    }
    #adjust plus group sum of geometric series as a/(1-r)
    N1[nage] <- N1[nage]/(1-Zage[nage])

    SSB_eq <- sum(df$Matsel*N1)*0.5

    #print(SSB_eq/SSB_0)

    ans <- (SSB_eq/SSB_0-0.4)^2
    return(ans)
  }
  F40 <- optim(par = 0.1, fn = get_f, method = 'Brent', lower = 0, upper = 4)
  Z <- M+f_in*sel
  Zage <- Z
  Neq <- NA
  Neq[1] <- R0
  for(a in 1:(nage-1)){
    Neq[a+1] <- Neq[a]*exp(-Zage[a])
  }
  # adjust plus group sum of geometric series as a/(1-r)
  Neq[nage] <- Neq[nage]/(1-Zage[nage])
  SSB_new <- sum(df$Matsel*Neq)*0.5
  SPR_new <- SSB_new/SSB_0
  Fnew <- F40$par
  V <- sum(n_end*Cw*sel)
  Fx <- 1-exp(-Fnew) # Convert to harvest rate

  #print(Fx/0.18)
  #Fx <- 0.18

  if((ssb_y/SSB_0) < 0.1){
    Cnew <- 0.05*v_real # add a very low catch (fix later)
  }

  if((ssb_y/SSB_0) > 0.4){
    Cnew <- Fx*V
  }

  if(((ssb_y/SSB_0) <= 0.4) & ((ssb_y/SSB_0) >= 0.1)){
    Cnew <- Fx*V*((ssb_y-0.1*SSB_0)*((0.4*SSB_0/ssb_y)/(0.4*SSB_0-0.1*SSB_0)))
  }
  #
  # if (Cnew > 500000){
  #   Cnew <- 500000
  # }
  # Adjust tac by JMC/Utilization
  tac.obs <- read.csv(system.file("extdata/adjusted_tac_fn.csv",
                                  package = "PacifichakeMSE",
                                  mustWork = TRUE))

  if(tac == 1){
    Cexp <- Cnew
  }else if(tac == 2){
    Cexp <- tac.obs$incpt[1]+tac.obs$slp[1]*Cnew
  }else if(tac == 3){
    Cexp <- tac.obs$incpt[2]+tac.obs$slp[2]*Cnew
  }else if(tac == 4){ # Half the treaty specified and with a lower floor
    Cexp <- Cnew*0.5

    if(Cexp < 180000){
      Cexp <- 180000
    }
  }

  # Do a test run
  # print(paste('JTC tac = ', Cnew))
  # print(paste('JMC  tac = ', Cexp))

  if(Cexp > Cnew){ # Never go over the JTC recommendation
    Cexp <- Cnew
  }
  list(Cnew = Cexp, Fnew = F40$par)
}
