#' Calculate the catch value to apply to the current year in the OM loop. TACs can be applied
#' to the catch value also.
#'
#' @param om Operating model list
#' @param yr The year to calculate the reference points for
#' @param yr_ind Index for the year `yr` in the om output
#' @param tac TAC values to apply to the catch before returning the function.
#' @param catch_floor The lowest catch to be allowed
#' @param hcr_apply Logical. If `TRUE` apply the Harvest control rule
#' @param hcr_lower If `hcr_apply` is `TRUE`, this is the lower limit for the rule
#' @param hcr_upper If `hcr_apply` is `TRUE`, this is the upper limit for the rule
#' @param hcr_fspr If `hcr_apply` is `TRUE`, this is the value to set the Fspr at for catch above `hcr_upper`
#' @param ... Absorb arguments meant for other functions
#'
#' @return Catch for the next year
#' @export
apply_hcr_om <- function(
  om,
  yr = NULL,
  yr_ind = NULL,
  season,
  space,
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

  c_w <- om$wage_catch_df %>%
    filter(Yr == yr - 1) %>%
    select(-Yr) %>%
    unlist(use.names = FALSE)

  m_age <- rep(m_est, om$n_age)
  n_0 <- NULL
  n_0[1] <- r_0
  for(a in 1:(om$n_age - 1)){
    n_0[a + 1] <- n_0[a] * exp(-m_age[a])
  }
  # Adjust plus group sum of geometric series as a/(1-r)
  n_0[om$n_age] <- n_0[om$n_age] / (1 - m_age[om$n_age])

  mat_sel <- om$mat_sel %>% select(-Yr) %>% unlist(use.names = FALSE)
  ssb_age <- mat_sel * n_0 * 0.5
  ssb_0 <- sum(ssb_age)
  #ssb_0 <- om$ssb_0 %>% sum

  # Assume the year's F is mean of F of the four seasons of last year
  f_in1 <- om$f_out_save[, , 1] %>%
    as_tibble(rownames = "yrs") %>%
    filter(yrs == yr - 1) %>%
    select(-yrs) %>%
    unlist() %>%
    mean()
  f_in2 <- om$f_out_save[, , 2] %>%
    as_tibble(rownames = "yrs") %>%
    filter(yrs == yr - 1) %>%
    select(-yrs) %>%
    unlist() %>%
    mean()

  z_age1 <- m_age + f_in1 * f_sel1
  z_age2 <- m_age + f_in2 * f_sel2
  n1_1 <- n2_1 <- NULL
  n1_1[1] <- n2_1[1] <- r_0
  for(a in 1:(om$n_age - 1)){
    n1_1[a + 1] <- n1_1[a] * exp(-z_age1[a])
    n2_1[a + 1] <- n2_1[a] * exp(-z_age2[a])
  }
  # Adjust plus group sum of geometric series as a/(1-r)
  n1_1[om$n_age] <- n1_1[om$n_age] / (1 - z_age1[om$n_age])
  n2_1[om$n_age] <- n2_1[om$n_age] / (1 - z_age2[om$n_age])

  ssb_eq1 <- sum(mat_sel * n1_1) * 0.5
  ssb_eq2 <- sum(mat_sel * n2_1) * 0.5
  #spr <- ssb_eq / ssb_0

  # Calculate the F_SPR40% (hcr_fspr) reference point
  get_f <- function(par, z_age, n_1, ssb_eq, f_sel){
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
  f_xx1 <- optim(par = 0.1,
                 fn = get_f,
                 method = "Brent",
                 lower = 0,
                 upper = 4,
                 z_age = z_age1,
                 n_1 = n1_1,
                 ssb_eq = ssb_eq1,
                 f_sel = f_sel1)
  f_xx2 <- optim(par = 0.1,
                 fn = get_f,
                 method = "Brent",
                 lower = 0,
                 upper = 4,
                 z_age = z_age2,
                 n_1 = n2_1,
                 ssb_eq = ssb_eq2,
                 f_sel = f_sel2)

  f_new1 <- f_xx1$par
  f_new2 <- f_xx2$par

  # Extract a vector for the numbers-at-age at the end of last year
  n_end1 <- om$n_save_age[, yr_ind - 1, 1, om$n_season]
  n_end2 <- om$n_save_age[, yr_ind - 1, 2, om$n_season]
  # Replace age 0's with zero because there aren't any age zeros at the end of the year
  n_end1[1] <- 0.0
  n_end2[1] <- 0.0
  v1 <- sum(n_end1 * c_w * f_sel1)
  v2 <- sum(n_end1 * c_w * f_sel2)
  # Convert to harvest rate
  f_x1 <- 1 - exp(-f_new1)
  f_x2 <- 1 - exp(-f_new2)

  ssb_y <- om$ssb_all[yr_ind - 1, , 4] %>% sum
  depl <- ssb_y / ssb_0

  if(depl < hcr_lower){
    c_new <- 0.05 * (v1 + v2)
  }else if(depl > hcr_upper){
    c_new <- f_x1 * v1 + f_x2 * v2
  }else{
    # 40:10 adjustment (actually hcr_upper:hcr_lower)
    fct <- (ssb_y - hcr_lower * ssb_0) / (hcr_upper * ssb_0 - hcr_lower * ssb_0)
    c_new <- fct * (f_x1 * v1 + f_x2 * v2)
  }

  # Adjust TAC by JMC/Utilization
  if(length(tac) == 1){
    # Floor 50%
    c_exp <- c_new * 0.5
    c_exp <- ifelse(c_exp < catch_floor, catch_floor, c_exp)
  }else{
    c_exp <- tac[1] + tac[2] * c_new
  }
  #if(yr >= 2020 & season == 4 & space == 2) browser()
  # Never go over the JTC recommendation
  ifelse(c_exp > c_new, c_new, c_exp)
}
