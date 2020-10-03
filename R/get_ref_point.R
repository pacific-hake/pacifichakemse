#' Get reference points (TODO: improve docs on this function)
#'
#' @param pars estimated parameters
#' @param df data frame of non-estimated parameters
#' @param ssb_y Spawning biomass
#' @param f_in Fishing mortality
#' @param n_end Numbers at age
#' @param tac which tac function to use?
#' @param v_real Vulnerable biomass in OM
#' @param space The space (area) to use the selectivity values for. Defaults to USA (2)
#' @param catch_floor The lowest catch to be allowed
#' @param ... Absorb arguments meant for other functions
#'
#' @return A list of reference points
#' @export
get_ref_point <- function(pars,
                          df,
                          ssb_y,
                          f_in = NA,
                          n_end,
                          tac = NULL,
                          v_real = NA,
                          space = 2,
                          catch_floor = NULL,
                          ...){

  r_0 <- exp(pars$log_r_init)
  m_est <- exp(pars$log_m_init)
  h <- exp(pars$log_h)
  p_sel <- df$parameters$p_sel_fish %>%
    filter(!!space == space)
  p_sel$value <- c(0, pars$p_sel_fish)
  f_sel <- get_select(df$ages,
                      p_sel,
                      df$s_min,
                      df$s_max)

  c_w <- df$wage_catch[nrow(df$wage_catch),] %>% select(-Yr) %>% unlist(use.names = FALSE)
  m_age <- rep(m_est, df$n_age)
  n_0 <- NULL
  n_0[1] <- r_0
  for(a in 1:(df$n_age - 1)){
    n_0[a + 1] <- n_0[a] * exp(-m_age[a])
  }
  # Adjust plus group sum of geometric series as a/(1-r)
  n_0[df$n_age] <- n_0[df$n_age] / (1 - m_age[df$n_age])

  mat_sel <- df$mat_sel %>% select(-Yr) %>% unlist(use.names = FALSE)
  ssb_age <- mat_sel * n_0 * 0.5
  ssb_0 <- sum(ssb_age)
  #ssb_pr <- ssb_0 / r_0
  #sb_eq <- 4 * h * r_0 * 0.4 * ssb_0 - ssb_0 * (1 - h) / (5 * h - 1)

  z_age <- m_age + f_in * f_sel
  n_1 <- NULL
  n_1[1] <- r_0
  for(a in 1:(df$n_age - 1)){
    n_1[a + 1] <- n_1[a] * exp(-z_age[a])
  }
  # Adjust plus group sum of geometric series as a/(1-r)
  n_1[df$n_age] <- n_1[df$n_age] / (1 - z_age[df$n_age])

  ssb_eq <- sum(mat_sel * n_1) * 0.5
  spr <- ssb_eq / ssb_0

  # Calculate the F40 reference point
  get_f <- function(par){
    z_age <- m_age + par[1] * f_sel
    n_1 <- NULL
    n_1[1] <- r_0
    for(a in 1:(df$n_age - 1)){
      n_1[a + 1] <- n_1[a] * exp(-z_age[a])
    }
    #adjust plus group sum of geometric series as a/(1-r)
    n_1[df$n_age] <- n_1[df$n_age] / (1 - z_age[df$n_age])
    ssb_eq <- sum(mat_sel * n_1) * 0.5
    (ssb_eq / ssb_0 - 0.4) ^ 2
  }
  f_40 <- optim(par = 0.1,
                fn = get_f,
                method = "Brent",
                lower = 0,
                upper = 4)

  z_age <- m_age + f_in * f_sel
  n_eq <- NULL
  n_eq[1] <- r_0
  for(a in 1:(df$n_age - 1)){
    n_eq[a + 1] <- n_eq[a] * exp(-z_age[a])
  }
  # adjust plus group sum of geometric series as a/(1-r)
  n_eq[df$n_age] <- n_eq[df$n_age] / (1 - z_age[df$n_age])

  ssb_new <- sum(mat_sel * n_eq) * 0.5
  spr_new <- ssb_new / ssb_0
  f_new <- f_40$par
  v <- sum(n_end * c_w * f_sel)
  # Convert to harvest rate
  f_x <- 1 - exp(-f_new)

  if((ssb_y / ssb_0) < 0.1){
    # TODO: fix later (add a very low catch)
    c_new <- 0.05 * v_real
  }

  if((ssb_y / ssb_0) > 0.4){
    c_new <- f_x * v
  }

  if(((ssb_y / ssb_0) <= 0.4) & ((ssb_y / ssb_0) >= 0.1)){
    c_new <- f_x * v *((ssb_y - 0.1 * ssb_0) *
                         ((0.4 * ssb_0 / ssb_y) /
                            (0.4 * ssb_0 - 0.1 * ssb_0)))
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
  c_exp <- ifelse(c_exp > c_new, c_new, c_exp)

  list(c_new = c_exp, f_new = f_40$par)
}
