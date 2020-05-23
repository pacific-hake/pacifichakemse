#' Run the Operating model for all seasons and spaces in one year
#'
#' @param df See [run_agebased_true_catch()]
#' @param lst See [run_agebased_true_catch()]
#' @param yr The year to run the operating model for
#' @param yr_ind The index of `yr` in the `df$yrs` vector
#' @param m_season A vector of natural mortality-at-age
#' @param pope_mul Multiplier used in Pope's method
#' @param ... Absorbs additional arguments meant for other functions
#'
#' @return A modified version of `lst` with the current data for `yr` populated
#' in all it's arrays and other objects
#' @importFrom crayon red yellow
#' @export
run_season_loop_om <- function(df,
                               lst,
                               yr,
                               yr_ind,
                               m_season,
                               pope_mul = 0.5,
                               verbose = TRUE,
                               ...){
  map(seq_len(df$n_season), function(season = .x){
    if(verbose){
      cat(red(paste0("Season: ", season, "\n")))
    }
    map(seq_len(df$n_space), function(space = .x){
      if(verbose){
        cat(yellow(paste0("      Space: ", space, "\n")))
      }
      p_sel <- df$p_sel %>% filter(!!space == space)
      p_sel_tmp <- p_sel
      if(df$flag_sel[yr_ind] == 1){
        p_sel_tmp$value <- df$parms_init$p_sel[, yr_ind - df$sel_idx + 1] * df$sigma_p_sel
      }
      if(df$yrs[yr_ind] > df$m_yr){
        if(df$selectivity_change == 1){
          if(space != 1){
            p_sel_tmp$value <- c(rep(0.05, df$s_min_survey),
                                 rep(0, df$s_max_survey - 2 * df$s_min_survey + 1))
          }
        }else if(df$selectivity_change == 2){
          p_sel_tmp$value <- df$parms_init$p_sel[,ncol(df$parms_init$p_sel)] * df$sigma_p_sel
        }
      }

      # Constant over space
      f_sel <- get_select(df$ages,
                          p_sel_tmp,
                          df$s_min,
                          df$s_max)
      lst$f_sel_save[, yr_ind, space] <<- f_sel
      if(df$n_space > 1){
        if(df$yrs[yr_ind] <= df$m_yr){
          catch_space <- df$catch_country %>%
            filter(year == yr) %>%
            select(contains(paste0("space", space))) %>% pull()
        }else{
          catch_space <- df$catch[yr_ind] * df$f_space[space]
        }
      }else{
        catch_space <- df$catch[yr_ind]
      }
      # Catch distribution in the yrs
      e_tmp <- catch_space * df$catch_props_space_season[space, season]
      n_tmp <- lst$n_save_age[, yr_ind, space, season]
      # Get biomass from previous yrs
      wage_catch <- df$wage_catch %>% get_age_dat(yr) %>% unlist()
      b_tmp <- sum(n_tmp * exp(-m_season * pope_mul) * wage_catch * f_sel)
      lst$v_save[yr_ind, space, season] <<- b_tmp
      lst$catch_quota[yr_ind, space, season] <<- e_tmp

      if(e_tmp / b_tmp >= 0.9){
        if(df$yrs[yr_ind] < df$m_yr){
          # Stop if in the past
          stop("Catch exceeds available biomass in yrs: ",
               df$yrs[yr_ind], " and season ",
               season, " , space ", space,
          call. = FALSE)
        }
        e_tmp <- 0.75 * b_tmp
        lst$catch_quota_n[yr_ind, space, season] <<- 1
      }
      f_out <- get_f(e_tmp = e_tmp,
                     b_tmp = b_tmp,
                     m_season = m_season,
                     f_sel = f_sel,
                     n_tmp = n_tmp,
                     wage_catch = wage_catch,
                     method = "Hybrid")
      if(e_tmp > 0){
        f_new <- f_out
        f_season <- f_new * f_sel
      }else{
        f_season <- 0
      }
      # Terminal fishing mortality
      lst$f_out_save[yr_ind, season, space] <<- f_out
      lst$f_season_save[, yr_ind, space, season] <<- f_season
      z <- m_season + f_season
      lst$z_save[, yr_ind, space, season] <<- z
      # This is used to deal with movement from one spce to another.
      # space is the area fish are in and space_idx is the area the fish
      # are coming in from. These indexes are used in the numbers-at-age
      # calculations which include movement
      if(space - 1 == 0){
        space_idx <- 2
      }
      if(space == df$n_space){
        space_idx <- df$n_space - 1
      }
      if(space > 1 & space < df$n_space){
        space_idx <- c(space - 1, space + 1)
      }
      if(!df$move){
        space_idx <- 1
      }

      if(season < df$n_season){
        lst$n_save_age[, yr_ind, space, season + 1] <<- lst$n_save_age[, yr_ind, space, season] * exp(-z) -
          # Remove the ones that leave
          lst$n_save_age[, yr_ind, space, season] * exp(-z) * (df$move_mat[space, , season, yr_ind]) +
          # Add the ones come from the surrounding areas
          lst$n_save_age[, yr_ind, space_idx, season] * exp(-z) * (df$move_mat[space_idx, , season, yr_ind])
      }else{
        lst$n_save_age[2:(df$n_age - 1), yr_ind + 1, space, 1] <<- lst$n_save_age[1:(df$n_age - 2), yr_ind, space, season] *
          exp(-z[1:(df$n_age - 2)]) - lst$n_save_age[1:(df$n_age - 2), yr_ind, space, season] *
          # Remove the ones that leave
          exp(-z[1:(df$n_age - 2)]) * (df$move_mat[space, 1:(df$n_age - 2), season, yr_ind]) +
          # Add the ones come to the surrounding areas
          lst$n_save_age[1:(df$n_age - 2), yr_ind, space_idx, season] *
          exp(-z[1:(df$n_age - 2)]) * (df$move_mat[space_idx, 1:(df$n_age - 2), season, yr_ind])

        # Plus group
        n_survive_plus <- (lst$n_save_age[df$n_age - 1, yr_ind, space, df$n_season] * exp(-z[df$n_age - 1]) +
                             lst$n_save_age[df$n_age, yr_ind, space, df$n_season] * exp(-z[df$n_age]))
        # Leaving
        n_out_plus <- n_survive_plus * (df$move_mat[space, df$n_age, season, yr_ind])
        # Incoming
        n_in_plus <- (lst$n_save_age[df$n_age-1, yr_ind, space_idx, df$n_season] * exp(-z[df$n_age - 1]) +
                       lst$n_save_age[df$n_age, yr_ind, space_idx, df$n_season] * exp(-z[df$n_age])) *
          df$move_mat[space_idx, df$n_age, season, yr_ind]
        lst$n_save_age[df$n_age, yr_ind + 1, space, 1] <<- n_survive_plus - n_out_plus + n_in_plus
      }
      lst$age_comps_om[, yr_ind, space, season] <<- lst$n_save_age[, yr_ind, space, season] / sum(lst$n_save_age[, yr_ind, space, season])
      lst$ssb_all[yr_ind, space, season] <<- sum(lst$n_save_age[, yr_ind, space, season] * lst$mat_sel, na.rm = TRUE)
      lst$catch_save_age[, yr_ind, space, season] <<- (f_season / z) * (1 - exp(-z)) * lst$n_save_age[, yr_ind, space, season] * wage_catch
      lst$catch_n_save_age[, yr_ind, space, season] <<- (f_season / z) * (1 - exp(-z)) * lst$n_save_age[, yr_ind, space, season]
      if(lst$catch_quota[yr_ind, space, season] > 0){
        if((sum(lst$catch_save_age[, yr_ind, space, season]) / lst$catch_quota[yr_ind, space, season]) > 1.1){
          stop("F estimation overshoots more than 10% in year ", yr, ", season ", season,
               call. = FALSE)
        }
      }
    })
  })
  #if(yr == 1995) browser()
  lst
}
