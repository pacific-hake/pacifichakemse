#' Calculate mean age in the population
#'
#' @param agemat matrix of ages
#' @param maxage maximum age to include in the calculation
#'
#' @return A vector of the mean ages by year, with names attribute as year names (character)
#' @export
calc_mean_age <- function(agemat = NULL,
                          maxage = NULL){

  verify_argument(agemat, "matrix")
  verify_argument(maxage, c("numeric", "integer"), 1)

  n_ages_agemat <- nrow(agemat)
  stopifnot(n_ages_agemat >= maxage)
  age <- 1:maxage

  if(n_ages_agemat > maxage){
    # Calculate plus group
    agemat[maxage, ] <- colSums(agemat[maxage:n_ages_agemat, ], na.rm = TRUE)
  }
  agemat <- agemat[1:maxage, ]
  agemat <- apply(agemat, 2, function(x){x / sum(x)})
  agemat <- agemat %>% as_tibble()

  if(all(colSums(agemat) != 1)){
    agemat <- map_df(agemat, ~{
      .x / sum(.x)
    })
  }

  # Return vector of mean ages by year
  map_dbl(agemat, ~{
    sum(age * .x)
  })
}
