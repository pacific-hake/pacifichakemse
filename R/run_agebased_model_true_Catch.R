#' Run an agebased model
#'
#' @param df data frame of parameters and life history values
#' @param seed seed for survey error and recruitment deviations
#'
#' @return A list of model outputs (TODO)
#' @importFrom purrr map_dbl
#' @export
#'
#' @examples
#' \dontrun{
#' run_agebased_true_catch(df)
#' }
run_agebased_true_catch <- function(df = NULL,
                                    seed = 100,
                                    ...){
  verify_argument(df, "list")
  verify_argument(seed, "numeric", 1)

  set.seed(seed)

  lst <- setup_blank_om_objects(yrs = df$yrs,
                                ages = df$ages,
                                max_surv_age = df$age_max_age,
                                n_space = df$n_space,
                                n_season = df$n_season)

  lst <- init_agebased_model(df, lst)

  run_year_loop_om(df, lst, ...)
    #Catch.age[,idx]  <- (Fyrs/(Fyrs+m_yrs))*(1-exp(-(Fyrs+m_yrs)))*rowSums(N.save.age[,idx,,1])*wage$catch # Calculate the catch in kg

}
