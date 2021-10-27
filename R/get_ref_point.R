#' Calculate the catch and F40 value to apply to the next year in the MSE loop. TACs will be applied
#' to the catch value.
#'
#' @param pars Estimated parameters
#' @param df Data frame of non-estimated parameters
#' @param yr The year to calculate the reference points for
#' @param ssb_y Spawning biomass
#' @param f_in Fishing mortality
#' @param n_end Numbers at age
#' @param tac TAC values to apply to the catch before returning the function.
#' @param v_real Vulnerable biomass in OM
#' @param space The space (area) to use the selectivity values for. Defaults to USA (2)
#' @param catch_floor The lowest catch to be allowed
#' @param ... Absorb arguments meant for other functions
#'
#' @return A list of two, catch and F40 for the next year
#' @export
get_ref_point <- function(pars,
                          df,
                          yr = NULL,
                          ssb_y,
                          f_in = NA,
                          n_end,
                          tac = NULL,
                          v_real = NA,
                          space = 2,
                          catch_floor = NULL,
                          hcr_lower = 0.1,
                          hcr_upper = 0.4,
                          hcr_fspr = 0.4,
                          ...){

  r_0 <- exp(pars$log_r_init)
  m_est <- exp(pars$log_m_init)
  h <- exp(pars$log_h)
  p_sel <- df$parameters$p_sel_fish[df$parameters$p_sel_fish[, "space"] == space, ]
  p_sel[, "value"] <- pars$p_sel_fish
  f_sel <- get_select(df$ages,
                      p_sel,
                      df$s_min,
                      df$s_max)

  c_w <- df$wage_catch[df$wage_catch[, "Yr"] == yr, -1]

  m_age <- rep(m_est, df$n_age)
  n_0 <- NULL
  n_0[1] <- r_0
  for(a in 1:(df$n_age - 1)){
    n_0[a + 1] <- n_0[a] * exp(-m_age[a])
  }
  # Adjust plus group sum of geometric series as a/(1-r)
  n_0[df$n_age] <- n_0[df$n_age] / (1 - m_age[df$n_age])

  mat_sel <- df$mat_sel[-1]
  ssb_age <- mat_sel * n_0 * 0.5
  ssb_0 <- sum(ssb_age)

  #ssb_pr <- ssb_0 / r_0
  #sb_eq <- 4 * h * r_0 * upper_ref * ssb_0 - ssb_0 * (1 - h) / (5 * h - 1)

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

  # Calculate the F40 (actually f_ref) reference point
  get_f <- function(par){
    z_age <- m_age + par[1] * f_sel
    n_1 <- NULL
    n_1[1] <- r_0
    for(a in 1:(df$n_age - 1)){
      n_1[a + 1] <- n_1[a] * exp(-z_age[a])
    }
    # Adjust plus group sum of geometric series as a/(1-r)
    n_1[df$n_age] <- n_1[df$n_age] / (1 - z_age[df$n_age])
    ssb_eq <- sum(mat_sel * n_1) * 0.5
    (ssb_eq / ssb_0 - hcr_fspr) ^ 2
  }
  f_xx <- optim(par = 0.1,
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

  f_new <- f_xx$par
  v <- sum(n_end * c_w * f_sel)
  # Convert to harvest rate
  f_x <- 1 - exp(-f_new)
  depl <- ssb_y / ssb_0

  if(depl < hcr_lower){
    c_new <- 0.0001
    #c_new <- 0.05 * v_real
  }else if(depl > hcr_upper){
    c_new <- f_x * v
  }else{
    # c_new <- f_x * v *((ssb_y - hcr_lower * ssb_0) *
    #                      ((hcr_upper * ssb_0 / ssb_y) /
    #                         (hcr_upper * ssb_0 - hcr_lower * ssb_0)))
    # 40:10 adjustment (actually hcr_upper:hcr_lower)
    fct <- ((ssb_y - hcr_lower * ssb_0) * ((hcr_upper * ssb_0 / ssb_y) / (hcr_upper * ssb_0 - hcr_lower * ssb_0)))
    c_new <- fct * f_x * v
  }

  # Adjust TAC by JMC/Utilization
  if(length(tac) == 1){
    # Floor 50%
    c_exp <- c_new * 0.5
    if(c_exp < catch_floor){
      c_exp <- catch_floor
    }
  }else{
    c_exp <- tac[1] + tac[2] * c_new
  }

  # Never go over the JTC recommendation
  if(c_exp > c_new){
    c_exp <- c_new
  }

  list(c_new = c_exp, f_new = f_new)
}
