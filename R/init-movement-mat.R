#' Initialize the movement model matrix. An alternative function should be
#' written if changes are required to the initialization assumptions
#'
#' @param n_space See [load_data_om()]
#' @param space_names See [load_data_om()]
#' @param n_season See [load_data_om()]
#' @param season_names See [load_data_om()]
#' @param m_yr The last non-future year. Used to de-populate future values
#' if `populate_future` is FALSE
#' @param n_yr The number of years in the array dimension
#' @param yrs A vector of names for the years. Length must equal `n_yr`
#' @param move_max A vector of the maximum movement rate, one for each
#' of `n_seasons`
#' @param move_slope  See [load_data_om()]
#' @param move_fifty Age at 50 percent movement rate
#' @param move_south  See [load_data_om()]
#' @param move_out  See [load_data_om()]
#' @param move_init  See [load_data_om()]
#' @param ages_no_move See [load_data_om()]
#' @param ages See [load_data_om()]
#' @param age_names See [load_data_om()]
#' @param f_space See [load_data_om()]
#'
#' @return A list of 3 elements: The `move_mat` matrix for movement,
#' the `move_init` vector of length `n_space` and the `f_space` vector
#' of length `n_space`
#' @export
init_movement_mat <- function(n_space = NULL,
                              space_names = NULL,
                              n_season = NULL,
                              season_names = NULL,
                              m_yr = NULL,
                              n_yr = NULL,
                              yrs = NULL,
                              move_max = NULL,
                              move_slope = NULL,
                              move_fifty = NULL,
                              move_south = NULL,
                              move_out = NULL,
                              move_init = NULL,
                              ages_no_move = NULL,
                              ages = NULL,
                              age_names = NULL,
                              f_space = NULL){

  n_age <- length(ages)
  move_mat <- array(0,
                    dim = c(n_space, n_age, n_season, n_yr),
                    dimnames = list(space_names,
                                    age_names,
                                    season_names,
                                    yrs))
  for(i in 1:n_space){
    for(j in 1:n_season){
      move_mat[i, , j, ] <- move_max[j] / (1 + exp(-move_slope * (ages - move_fifty)))
    }
  }

  # Some ages don't move
  move_mat[, which(ages_no_move %in% ages), , ] <- 0
  if(n_season == 4){
    # Don't move south during the year
    move_mat[1, 3:n_age, 2:3,] <- move_south
    # continuing south movement at spawning time
    move_mat[1, 3:n_age, 1,] <- move_south
    # The following two lines are co-dependent. The fish that move_out of Canada
    # move into the US
    move_mat[1, 3:n_age, 4,] <- move_out
    move_mat[2, 3:n_age, 4,] <- move_south

  }else{
    move_init <- 1
    # All F occurs in US
    f_space <- c(0, 1)
  }
  # if(!populate_future){
  #   m_ind <- which(yrs == m_yr)
  #   move_mat[, , , (m_ind + 1):n_yr] <- NA
  # }
  list(move_mat = move_mat,
       move_init = move_init,
       f_space = f_space)
}
