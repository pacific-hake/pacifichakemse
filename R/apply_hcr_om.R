#' Calculate the catch value to apply to the current year in the OM loop. TACs can be applied
#' to the catch value also.
#'
#' @param om Operating model list
#' @param yr The year to calculate the reference points for
#' @param yr_ind Index for the year `yr` in the om output
#' @param tac TAC values to apply to the catch before returning the function.
#' @param catch_floor The lowest catch to be allowed
#' @param hcr_lower If `hcr_apply` is `TRUE`, this is the lower limit for the rule
#' @param hcr_upper If `hcr_apply` is `TRUE`, this is the upper limit for the rule
#' @param hcr_fspr If `hcr_apply` is `TRUE`, this is the value to set the Fspr at
#' for catch above `hcr_upper`
#' @param ... Absorb arguments meant for other functions
#'
#' @return Catch for the next year
#' @export
apply_hcr_om <- function(
  om,
  yr = NULL,
  yr_ind = NULL,
  tac = NULL,
  catch_floor = NULL,
  hcr_lower = 0.1,
  hcr_upper = 0.4,
  hcr_fspr = 0.4,
  ...){

  pars <- om$parameters
  r_0 <- exp(pars$log_r_init)
  m_est <- exp(pars$log_m_init)
  h <- exp(pars$log_h)
  p_sel1 <- pars$p_sel_fish %>%
    filter(space == 1)
  p_sel2 <- pars$p_sel_fish %>%
    filter(space == 2)
  f_sel1 <- get_select(om$ages,
                       p_sel1,
                       om$s_min,
                       om$s_max)
  f_sel2 <- get_select(om$ages,
                       p_sel2,
                       om$s_min,
                       om$s_max)
  f_sel <- (f_sel1 + f_sel2) / 2

  c_w <- om$wage_catch_df %>%
    filter(Yr == yr - 1) %>%
    select(-Yr) %>%
    unlist(use.names = FALSE)

  m_age <- rep(m_est, om$n_age)
  n_0 <- r_0
  for(a in 1:(om$n_age - 1)){
    n_0[a + 1] <- n_0[a] * exp(-m_age[a])
  }
  # Adjust plus group sum of geometric series as a/(1-r)
  n_0[om$n_age] <- n_0[om$n_age] / (1 - m_age[om$n_age])

  mat_sel <- om$mat_sel %>% select(-Yr) %>% unlist(use.names = FALSE)
  ssb_age <- mat_sel * n_0 * 0.5
  ssb_0 <- sum(ssb_age)

  # Assume the year's F is mean of F of the four seasons of last year
  # Maybe these should be weighted by om$catch_props_space_season
  f_in1 <- mean(om$f_out_save[yr_ind - 1, , 1])
  f_in2 <- mean(om$f_out_save[yr_ind - 1, , 2])

  z_age1 <- m_age + f_in1 * f_sel1
  z_age2 <- m_age + f_in2 * f_sel2
  z_age <- (z_age1 + z_age2) / 2

  # Portion the R0 value into the areas based on apportionment
  n_1 <- r_0
  for(a in 1:(om$n_age - 1)){
    n_1[a + 1] <- n_1[a] * exp(-z_age[a])
  }
  # Adjust plus group sum of geometric series as a/(1-r)
  n_1[om$n_age] <- n_1[om$n_age] / (1 - z_age[om$n_age])
  n_1[1] <- 0

  ssb_eq <- sum(mat_sel * n_1) * 0.5
  #spr <- ssb_eq / ssb_0

  # Calculate the F_SPR40% (hcr_fspr) reference point
  get_f <- function(par, n_1, ssb_eq, f_sel, r_0, ssb_0){
    z_age <- m_age + par[1] * f_sel
    n_1 <- NULL
    n_1[1] <- r_0
    for(a in 1:(om$n_age - 1)){
      n_1[a + 1] <- n_1[a] * exp(-z_age[a])
    }
    # Adjust plus group sum of geometric series as a/(1-r)
    n_1[om$n_age] <- n_1[om$n_age] / (1 - z_age[om$n_age])
    ssb_eq <- sum(mat_sel * n_1) * 0.5
    (ssb_eq / ssb_0 - hcr_fspr) ^ 2
  }
  f_xx <- optim(par = 0.1,
                fn = get_f,
                method = "Brent",
                lower = 0,
                upper = 4,
                n_1 = n_1,
                ssb_eq = ssb_eq,
                f_sel = f_sel,
                r_0 = r_0,
                ssb_0 = ssb_0)

  f_new <- f_xx$par

  # Extract a vector for the numbers-at-age at the end of last year
  n_end <- om$n_save_age[, yr_ind - 1, , om$n_season] %>% rowSums

  # Replace age 0's with zero because there aren't any age zeros at the end of the year
  n_end[1] <- 0.0
  v <- sum(n_end * c_w * f_sel)

  # Convert to harvest rate
  f_x <- 1 - exp(-f_new)

  ssb_y <- sum(om$ssb_all[yr_ind - 1, , 4])
  depl <- ssb_y / ssb_0

  if(depl < hcr_lower){
    c_new <- 0
    #c_new <- 0.05 * v
  }else if(depl > hcr_upper){
    c_new <- f_x * v
  }else{
    # 40:10 adjustment (actually hcr_upper:hcr_lower)
    fct <- ((ssb_y - hcr_lower * ssb_0) / (hcr_upper * ssb_0 - hcr_lower * ssb_0))
    # Nis' code with extra term:
    #fct <- ((ssb_y - hcr_lower * ssb_0) * ((hcr_upper * ssb_0 / ssb_y) / (hcr_upper * ssb_0 - hcr_lower * ssb_0)))
    c_new <- fct * f_x * v
  }

  # Adjust TAC by JMC/Utilization
  if(length(tac) == 1){
    # Floor 50%
    c_exp <- c_new * 0.5
    c_exp <- ifelse(c_exp < catch_floor, catch_floor, c_exp)
  }else{
    c_exp <- tac[1] + tac[2] * c_new
  }

  # Never go over the JTC recommendation
  ifelse(c_exp > c_new, c_new, c_exp)
}
