#' Run the outer year loop of the OM
#'
#' @param df See [run_agebased_true_catch()]
#' @param lst See [run_agebased_true_catch()]
#'
#' @return A list
#' @export
run_year_loop_om <- function(df = NULL,
                             lst = NULL){
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
    lst$n_save_age[1, yr_ind, , 1] <- rec    lst$r_save[yr_ind, ] <- rec
    # -------------------------------------------------------------------------

    browser()


  })
}
