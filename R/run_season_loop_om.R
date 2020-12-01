#' Run the Operating model for all seasons and spaces in one year
#'
#' @param om See [run_om()]
#' @param yr The year to run the operating model for
#' @param yr_ind The index of `yr` in the `om$yrs` vector
#' @param m_season A vector of natural mortality-at-age
#' @param attain The attainment vector of length 2, in the order Canada, US. These are proportions
#' of the catch to take.
#' @param ages_no_move Ages that don't move in the movement model
#' @param pope_mul Multiplier used in Pope's method
#' @param verbose Print the loop information to the console
#' @param testing Logical. If TRUE, write out testing files
#' @param ... Absorbs additional arguments meant for other functions
#'
#' @return A modified version of `om` with the current data for `yr` populated
#' in all it's arrays and other objects
#' @importFrom crayon red yellow
#' @importFrom tidyselect contains
#' @export
run_season_loop_om <- function(om,
                               yr,
                               yr_ind,
                               m_season,
                               attain = c(1, 1),
                               ages_no_move = c(0, 1),
                               pope_mul = 0.5,
                               verbose = TRUE,
                               testing = FALSE,
                               ...){

  verify_argument(om, "list")

  mat_sel <- om$mat_sel %>% select(-Yr)
  # Begin season loop ---------------------------------------------------------
  map(seq_len(om$n_season), function(season = .x){
    if(verbose){
      cat(red("Season:", season, "\n"))
    }
    # Begin space loop --------------------------------------------------------
    map(seq_len(om$n_space), function(space = .x){
      if(verbose){
        cat(yellow("      Space:", space, "\n"))
      }
      # Calculate selectivity -------------------------------------------------
      p_sel <- om$parameters$p_sel_fish[om$parameters$p_sel_fish$space == space,]
      p_sel_yrs <- om$sel_by_yrs
      if(om$flag_sel[yr_ind]){
        p_sel$value <- p_sel$value +
          p_sel_yrs[, yr_ind - om$sel_idx + 1] * om$sigma_p_sel
      }
      if(om$yrs[yr_ind] > om$m_yr){
        if(om$selectivity_change == 1){
          if(space != 1){
            p_sel$value <- c(rep(0.05, om$s_min_survey),
                             rep(0, om$s_max_survey - 2 *
                                   om$s_min_survey + 1))
          }
        }else if(om$selectivity_change == 2){
          p_sel <- om$parameters$p_sel_fish[om$parameters$p_sel_fish$space == 2,]
          p_sel$value <- p_sel$value +
            p_sel_yrs[,ncol(p_sel_yrs)] *
            om$sigma_p_sel
        }
      }

      # Constant over space
      f_sel <- get_select(om$ages,
                          p_sel,
                          om$s_min,
                          om$s_max)

      # Write selectivity testing file ----------------------------------------
      if(testing){
        if(yr == 1966 && season == 1 && space == 1){
          header <- c("Year", "Season", "Space", "Fsel", paste0("fsel", 1:(length(f_sel) - 1)))
          write(paste0(header, collapse = ","), "fselvals.csv")
        }
        write(paste(yr, season, space, paste(f_sel, sep = ",", collapse = ","), sep = ","), "fselvals.csv", append = TRUE)
      }

      om$f_sel_save[, yr_ind, space] <<- f_sel

      # Calculate catch space -------------------------------------------------
      if(om$n_space > 1){
        if(om$yrs[yr_ind] <= om$m_yr){
          catch_space <- om$catch_country %>%
            filter(year == yr) %>%
            select(contains(paste0("space", space))) %>% pull()
        }else{
          if("tbl_df" %in% class(om$catch_obs)){
            catch_space <- om$catch_obs[yr_ind, ]$value * om$f_space[space]
          }else{
            catch_space <- om$catch_obs[yr_ind, ] * om$f_space[space]
          }
        }
      }else{
        if("tbl_df" %in% class(om$catch_obs)){
          catch_space <- om$catch_obs[yr_ind, ]$value * om$f_space[space]
        }else{
          catch_space <- om$catch_obs[yr_ind, ] * om$f_space[space]
        }
      }

      # Calculate catch distribution ------------------------------------------
      if(attain[space] == 0){
        e_tmp <- 3000
      }else{
        e_tmp <- catch_space * ifelse(yr > om$m_yr, attain[space], 1) * om$catch_props_space_season[space, season] %>% pull
      }
      # Save the catch actually applied to each country so the EM can access it
      if(yr > om$m_yr){
        tmp_space_catch <- om$catch_country[[grep(space, names(om$catch_country))]][yr_ind]
        if(season == 1){
          tmp_space_catch <- 0
        }
        om$catch_country[[grep(space, names(om$catch_country))]][yr_ind] <<- tmp_space_catch + e_tmp
      }

      n_tmp <- om$n_save_age[, yr_ind, space, season]
      # Get biomass from previous yrs
      wage_catch <- om$wage_catch_df %>% get_age_dat(yr) %>% unlist()
      b_tmp <- sum(n_tmp * exp(-m_season * pope_mul) * wage_catch * f_sel, na.rm = TRUE)
      om$v_save[yr_ind, space, season] <<- b_tmp
      om$catch_quota[yr_ind, space, season] <<- e_tmp

      tryCatch({
        tmp <- e_tmp / b_tmp
      }, error = function(e){
        stop("Error in the Operating model. If running a standalone OM outside the MSE, ",
             "did you set `n_sim_yrs` instead of `yr_future`?",
             call. = FALSE)
      })

      if(e_tmp / b_tmp >= 0.9){
        if(om$yrs[yr_ind] < om$m_yr){
          # Stop if in the past
          message("Catch exceeds available biomass in yrs: ",
                  om$yrs[yr_ind], " and season ",
                  season, " , space ", space)
        }
        #e_tmp <- 0.75 * b_tmp
        #om$catch_quota_n[yr_ind, space, season] <<- 1
      }

      # Calculate F based on catch distribution -------------------------------
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
      om$f_out_save[yr_ind, season, space] <<- f_out
      om$f_season_save[, yr_ind, space, season] <<- f_season
      z <- m_season + f_season
      om$z_save[, yr_ind, space, season] <<- z

      # Calculate numbers-at-age with movement --------------------------------
      # This is used to deal with movement from one space to another.
      # space is the area fish are in and space_idx is the area the fish
      # are coming in from. These indexes are used in the numbers-at-age
      # calculations which include movement
      if(space == 1){
        space_idx <- 2
      }
      if(space == om$n_space){
        space_idx <- om$n_space - 1
      }
      if(space > 1 & space < om$n_space){
        space_idx <- c(space - 1, space + 1)
      }
      if(!om$move){
        space_idx <- 1
      }
      if(season < om$n_season){
        for(k in 1:length(space_idx)){
          # Add the ones come to the surrounding areas
          n_in_tmp <- om$n_save_age[, yr_ind, space_idx, season] * exp(-z) * (om$move_mat[space_idx[k], , season, yr_ind])
          if(k == 1){
            n_in <- n_in_tmp
          }else{
            n_in <- n_in + n_in_tmp
          }
        }
        om$n_save_age[, yr_ind, space, season + 1] <<- om$n_save_age[, yr_ind, space, season] * exp(-z) -
          # Remove the ones that leave
          om$n_save_age[, yr_ind, space, season] * exp(-z) * (om$move_mat[space, , season, yr_ind]) +
          # Add the ones come from the surrounding areas
          n_in
      }else{
        for(k in 1:length(space_idx)){
          # Add the ones come to the surrounding areas
          n_in_tmp <- om$n_save_age[1:(om$n_age - 2), yr_ind, space_idx, season] *
            exp(-z[1:(om$n_age - 2)]) * (om$move_mat[space_idx, 1:(om$n_age - 2), season, yr_ind])
          n_in_plus_tmp <- (om$n_save_age[om$n_age - 1, yr_ind, space_idx[k], om$n_season] * exp(-z[om$n_age - 1]) +
                              om$n_save_age[om$n_age, yr_ind, space_idx[k], om$n_season] * exp(-z[om$n_age])) *
            om$move_mat[space_idx[k], om$n_age, season, yr_ind]
          if(k == 1){
            n_in <- n_in_tmp
            n_in_plus <- n_in_plus_tmp
          }else{
            n_in <- n_in + n_in_tmp
            n_in_plus <- n_in_plus + n_in_plus_tmp
          }
        }
        om$n_save_age[2:(om$n_age - 1), yr_ind + 1, space, 1] <<- om$n_save_age[1:(om$n_age - 2), yr_ind, space, season] *
          exp(-z[1:(om$n_age - 2)]) - om$n_save_age[1:(om$n_age - 2), yr_ind, space, season] *
          # Remove the ones that leave
          exp(-z[1:(om$n_age - 2)]) * (om$move_mat[space, 1:(om$n_age - 2), season, yr_ind]) +
          # Add the ones come to the surrounding areas
          om$n_save_age[1:(om$n_age - 2), yr_ind, space_idx, season] *
          exp(-z[1:(om$n_age - 2)]) * (om$move_mat[space_idx, 1:(om$n_age - 2), season, yr_ind])

        # Plus group
        n_survive_plus <- (om$n_save_age[om$n_age - 1, yr_ind, space, om$n_season] * exp(-z[om$n_age - 1]) +
                             om$n_save_age[om$n_age, yr_ind, space, om$n_season] * exp(-z[om$n_age]))
        # Leaving
        n_out_plus <- n_survive_plus * (om$move_mat[space, om$n_age, season, yr_ind])

        om$n_save_age[om$n_age, yr_ind + 1, space, 1] <<- n_survive_plus - n_out_plus + n_in_plus
      }

      # Calculate age-comps ---------------------------------------------------
      om$age_comps_om[, yr_ind, space, season] <<- om$n_save_age[, yr_ind, space, season] /
        sum(om$n_save_age[, yr_ind, space, season])
      if(yr_ind == 1 && season == 1){
        om$ssb_all[1, space, season] <<- om$init_ssb_all[space]
      }else{
        om$ssb_all[yr_ind, space, season] <<- sum(om$n_save_age[, yr_ind, space, season] * mat_sel, na.rm = TRUE)
      }
      om$catch_save_age[, yr_ind, space, season] <<- (f_season / z) * (1 - exp(-z)) * om$n_save_age[, yr_ind, space, season] * wage_catch
      om$catch_n_save_age[, yr_ind, space, season] <<- (f_season / z) * (1 - exp(-z)) * om$n_save_age[, yr_ind, space, season]

      # Calculate catch quota -------------------------------------------------
      # if(om$catch_quota[yr_ind, space, season] > 0){
      #   if(sum(om$catch_save_age[, yr_ind, space, season]) / om$catch_quota[yr_ind, space, season] > 1.1){
      #     stop("F estimation overshoots more than 10% in year ", yr, ", season ", season, "space ", space,
      #          call. = FALSE)
      #   }
      # }
    })
    # End space loop ----------------------------------------------------------
  })
  # End season loop -----------------------------------------------------------

  om
}
