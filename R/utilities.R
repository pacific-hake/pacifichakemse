#' Calculate and insert columns containing arbitrary quantiles for a particular column
#'
#' @description Calculate and insert columns containing arbitrary quantiles for a particular column
#'
#' @param df A [data.frame]
#' @param col A column name on which to perform the calculations. Must be in `df` or an error
#' will be thrown
#' @param probs A vector of quantile probabilities to pass to [stats::quantile()]
#' @param include_mean If TRUE, include the mean in the output
#'
#' @return A [data.frame] with a new column for each value in the `probs` vector
#' @importFrom purrr set_names
#' @export
#' @examples
#' library(tibble)
#' library(dplyr)
#' library(purrr)
#' pq <- tribble(
#'   ~year, ~grp, ~val,
#'   2000,    1,  2.1,
#'   2001,    1,  3.4,
#'   2002,    1,  4.5,
#'   2003,    1,  5.6,
#'   2004,    1,  6.7,
#'   2000,    2,  3.1,
#'   2001,    2,  4.4,
#'   2002,    2,  5.5,
#'   2003,    2,  6.6,
#'   2004,    2,  8.7,
#'   2000,    3, 13.1,
#'   2001,    3, 14.4,
#'   2002,    3, 15.5,
#'   2003,    3, 16.6,
#'   2004,    3, 18.7)
#'
#' probs <- c(0.05, 0.25, 0.5, 0.75, 0.95)
#'
#' yrs <- sort(unique(pq$year))
#' df <- pq %>%
#'   group_by(year) %>%
#'   group_map(~ calc_quantiles(.x, col = "val", probs = probs)) %>%
#'   map_df(~{.x}) %>%
#'   mutate(year = yrs) %>%
#'   select(year, everything())
calc_quantiles <- function(df = NULL,
                           col = NULL,
                           probs = c(0.05, 0.25, 0.5, 0.75, 0.95),
                           include_mean = TRUE){

  verify_argument(df, c("data.frame", "tbl_df"))
  verify_argument(col, "character", 1)
  verify_argument(probs, "numeric")
  verify_argument(include_mean, "logical", 1)

  stopifnot(col %in% names(df))
  stopifnot(class(df[[col]]) == "numeric")
  col_sym <- sym(col)
  out <- summarize_at(df,
               vars(!!col_sym),
               map(probs,
                   ~partial(quantile, probs = .x, na.rm = TRUE)) %>%
                 set_names(probs))

  if(include_mean){
    out <- out %>%
      mutate(avg = mean(df[[col]]))
  }
  out
}

#' Calculate quantiles across groups for a given column
#'
#' @description Calculate quantiles across groups for a given column
#'
#' @rdname calc_quantiles
#'
#' @param df A [data.frame] with columns with names given by `grp_col` and `col`
#' @param grp_col The column name to use for grouping the data
#' @param col The column name to use as values to calculate quantiles for
#' @param probs A vector of quantiles to pass to [stats::quantile()]
#' @param include_mean If TRUE, include the mean in the output
#' @param grp_names The column name to use for labeling the grouped column. By default it is the same as the
#' grouping column (`grp_col`).
#'
#' @return A [data.frame] containing the quantile values with one row per group represented by `grp_col`
#' @importFrom rlang sym
#' @export
#'
#' @examples
#' library(tibble)
#' library(dplyr)
#' library(purrr)
#' pq <- tribble(
#'   ~year, ~grp, ~val,
#'   2000,    1,  2.1,
#'   2001,    1,  3.4,
#'   2002,    1,  4.5,
#'   2003,    1,  5.6,
#'   2004,    1,  6.7,
#'   2000,    2,  3.1,
#'   2001,    2,  4.4,
#'   2002,    2,  5.5,
#'   2003,    2,  6.6,
#'   2004,    2,  8.7,
#'   2000,    3, 13.1,
#'   2001,    3, 14.4,
#'   2002,    3, 15.5,
#'   2003,    3, 16.6,
#'   2004,    3, 18.7)
#'
#' probs <- c(0.05, 0.25, 0.5, 0.75, 0.95)
#'
#' j <- calc_quantiles_by_group(pq,
#'                              grp_col = "year",
#'                              col = "val",
#'                              probs = probs)
calc_quantiles_by_group <- function(df = NULL,
                                    grp_col = NULL,
                                    col = NULL,
                                    grp_names = grp_col,
                                    probs = c(0.05, 0.25, 0.5, 0.75, 0.95),
                                    include_mean = TRUE){

  verify_argument(df, c("data.frame", "tbl_df"))
  verify_argument(grp_col, "character", 1)
  verify_argument(col, "character", 1)
  verify_argument(grp_names, "character", 1)
  verify_argument(probs, "numeric")
  verify_argument(include_mean, "logical", 1)

  stopifnot(grp_col %in% names(df))
  stopifnot(col %in% names(df))

  grp_col_sym <- sym(grp_col)
  grp_names_sym <- sym(grp_names)
  col_sym <- sym(col)
  grp_vals <- unique(df[[grp_names]])

  df %>%
    group_by(!!grp_col_sym) %>%
    group_map(~ calc_quantiles(.x, col = col, probs = probs, include_mean = include_mean)) %>%
    map_df(~{.x}) %>%
    mutate(!!grp_names_sym := grp_vals) %>%
    select(!!grp_names_sym, everything()) %>%
    ungroup()
}

#' Calculate quantiles for the mean values (across years) of each value in
#' the group given by `grp_col` for a term of years
#'
#' @description Calculate quantiles for the mean values (across years) of each value in
#' the group given by `grp_col` for a term of years
#'
#' @rdname calc_quantiles
#'
#' @param df A [data.frame] with columns `year`, the string assigned to `grp_col`, and
#' the string assigned to `col`
#' @param grp_col The column name to use for grouping the data
#' @param min_yr The minimum year in the term. If `NA`, the minimum value in the [data.frame]
#' will be used
#' @param max_yr The maximum year in the term. If `NA`, the maximum value in the [data.frame]
#' will be used
#' @param probs A vector of quantiles to pass to [stats::quantile()]
#' @param mean_multiplier This value will be multiplied by the mean of the values
#' before the quantiles are calculated
#'
#' @return A single-row [data.frame] containing the quantile values of the run means
#' @importFrom rlang sym
#' @export
#'
#' @examples
#' library(tibble)
#' library(dplyr)
#' library(purrr)
#' pq <- tribble(
#'   ~year, ~grp, ~val,
#'   2000,    1,  2.1,
#'   2001,    1,  3.4,
#'   2002,    1,  4.5,
#'   2003,    1,  5.6,
#'   2004,    1,  6.7,
#'   2000,    2,  3.1,
#'   2001,    2,  4.4,
#'   2002,    2,  5.5,
#'   2003,    2,  6.6,
#'   2004,    2,  8.7,
#'   2000,    3, 13.1,
#'   2001,    3, 14.4,
#'   2002,    3, 15.5,
#'   2003,    3, 16.6,
#'   2004,    3, 18.7)
#'
#' probs <- c(0.05, 0.25, 0.5, 0.75, 0.95)
#'
#' j <- calc_term_quantiles(pq,
#'                          grp_col = "grp",
#'                          col = "val",
#'                          probs = probs)
calc_term_quantiles <- function(df = NULL,
                                grp_col = NULL,
                                col = NULL,
                                min_yr = NA_real_,
                                max_yr = NA_real_,
                                probs = c(0.05, 0.25, 0.5, 0.75, 0.95),
                                mean_multiplier = 1){

  verify_argument(df, c("data.frame", "tbl_df"))
  verify_argument(grp_col, "character", 1)
  verify_argument(col, "character", 1)
  verify_argument(min_yr, c("integer", "numeric"), 1)
  verify_argument(max_yr, c("integer", "numeric"), 1)
  verify_argument(probs, "numeric")
  verify_argument(mean_multiplier, c("integer", "numeric"), 1)

  stopifnot(grp_col %in% names(df))
  stopifnot(col %in% names(df))
  stopifnot(class(df[[col]]) == "numeric")
  grp_col_sym <- sym(grp_col)
  col_sym <- sym(col)
  if(is.na(min_yr)){
    min_yr <- min(df$year)
  }
  if(is.na(max_yr)){
    max_yr <- max(df$year)
  }
  if(max_yr < min_yr){
    warning("max_yr (", max_yr, ") is less than min_yr (", min_yr, "). Returning NULL")
    return(NULL)
  }
  if(!df %>% filter(df$year %in% min_yr:max_yr) %>% nrow){
    warning("There are no rows in the data frame within the years specified. Returning NULL")
    return(NULL)
  }

  df %>%
    filter(year >= min_yr & year <= max_yr) %>%
    group_by(!!grp_col_sym) %>%
    summarize(avg = mean(!!col_sym) * mean_multiplier) %>%
    group_map(~ calc_quantiles(.x, col = "avg", probs = probs, include_mean = FALSE)) %>%
    map_df(~{.x}) %>%
    ungroup()
}

#' Read in and convert data objects from the
#' old code to make sure new code produced the same likelihoods
#'
#' @param yr The year for file read (file will be d2018.rds for example)
#'
#' @return The data with converted names
#' @export
conv_d <- function(yr){
  d <- readRDS(paste0("d", yr, ".rds"))
  nm <- names(d)
  nm[nm == "year_sel"] <- "yr_sel"
  nm[nm == "Msel"] <- "m_sel"
  nm[nm == "Matsel"] <- "mat_sel"
  nm[nm == "nage"] <- "n_age"
  nm[nm == "selYear"] <- "sel_change_yr"
  nm[nm == "years"] <- "yrs"
  nm[nm == "tEnd"] <- "t_end"
  nm[nm == "logQ"] <- "log_q"
  nm[nm == "Smin"] <- "s_min"
  nm[nm == "Smax"] <- "s_max"
  nm[nm == "Smin_survey"] <- "s_min_survey"
  nm[nm == "Smax_survey"] <- "s_max_survey"
  nm[nm == "age_maxage"] <- "age_max_age"
  nm[nm == "Catchobs"] <- "catch_obs"
  nm[nm == "logSDcatch"] <- "log_sd_catch"
  nm[nm == "logSDR"] <- "rdev_sd"
  nm[nm == "logphi_survey"] <- "log_phi_survey"
  nm[nm == "sigma_psel"] <- "sigma_p_sel"
  nm[nm == "smul"] <- "s_mul"
  nm[nm == "Bprior"] <- "b_prior"
  nm[nm == "Aprior"] <- "a_prior"
  nm[nm == "age"] <- "ages"
  names(d) <- nm
  d
}

#' Read in and convert parameter objects from the
#' old code to make sure new code produced the same likelihoods
#'
#' @param yr The year for file read (file will be p2018.rds for example)
#'
#' @return The parameters with converted names
#' @export
conv_p <- function(yr){
  p <- readRDS(paste0("p", yr, ".rds"))
  nm <- names(p)
  nm[nm == "logRinit"] <- "log_r_init"
  nm[nm == "logh"] <- "log_h"
  nm[nm == "logMinit"] <- "log_m_init"
  nm[nm == "logSDsurv"] <- "log_sd_surv"
  nm[nm == "logphi_catch"] <- "log_phi_catch"
  nm[nm == "psel_fish"] <- "p_sel_fish"
  nm[nm == "psel_surv"] <- "p_sel_surv"
  nm[nm == "initN"] <- "init_n"
  nm[nm == "Rin"] <- "r_in"
  nm[nm == "PSEL"] <- "p_sel"
  nm[nm == "F0"] <- "f_0"
  names(p) <- nm
  p
}

#' Convert a vector of aggregated values into a [data.frame] containing
#' the year, run, value, and possibly country as columns
#'
#' @param vec The aggregated vector of data. This must be the same length as `run_vec`
#' and `yr_vec`
#' @param col The name of the column to create in the new data frame
#' @param yr_vec The vector of years to place in the [data.frame]. This must be the
#' same length as `vec`
#' @param country The name for the country. If `NULL`, no country column will be added
#' @param probs A vector of quantiles to pass to [stats::quantile()]
#' @param inc_mean Logical. Include the mean (as column `avg`)
#'
#' @return A [data.frame] containing the year, run, value, and possibly country as columns
#' @importFrom rlang :=
#' @export
conv_vec_to_mse_df <- function(vec = NULL,
                               col = NULL,
                               yr_vec = NULL,
                               country = NULL,
                               probs = c(0.05, 0.25, 0.5, 0.75, 0.95),
                               inc_mean = TRUE){

  stopifnot(!is.null(vec))
  stopifnot(!is.null(col))
  stopifnot(!is.null(yr_vec))
  stopifnot(!is.null(probs))
  stopifnot(length(vec) == length(yr_vec))

  col_sym <- sym(col)
  df <- vec %>%
    as.data.frame %>%
    rename(!!col_sym := 1) %>%
    mutate(year = yr_vec) %>%
    group_by(year) %>%
    group_map(~ calc_quantiles(.x, col = col, probs = probs, include_mean = inc_mean)) %>%
    map_df(~{.x}) %>%
    mutate(year = unique(yr_vec))
  if(!is.null(country)){
    df <- df %>%
      mutate(country = country) %>%
      select(country, year, everything())
  }else{
    df <- df %>%
      select(year, everything())
  }
  df
}

#' TMB helper function. Extract parameter from `obj` to use as initial values in optimization
#'
#' @details The code was copied from a non-exported function in the `TMBhelper` package called
#' `extract_fixed()`
#'
#' @param obj Object to be used as input to [TMB::MakeADFun()]
#'
#' @return A vector of initial values of parameters
#' @export
extract_initial_values <- function(obj){
  if(!length(obj$env$random)) {
    obj$env$last.par.best
  }
  else {
    obj$env$last.par.best[-c(obj$env$random)]
  }
}

#' Get a vector of the years from a list of MSEs
#'
#' @param lst A list of MSE results, constructed from code such as:
#' ls_plots <- map(fls,~{readRDS(.x)}) where fls is a list of RDS filenames
#' which have been written by [run_mses()]. See [setup_mse_plot_objects()]
#' for a closer look at how this list is constructed
#'
#' @return A vector of the years from a list of MSEs
#' @importFrom purrr map_lgl
#' @export
get_yrs_mse_list <- function(lst){

  ls_all <- map(lst, ~{
    as.numeric(attributes(.x$Catch)$dimnames$year)
  })
  first_elem <- ls_all[[1]]
  are_identical <- map_lgl(ls_all, ~{
    identical(.x, first_elem)
  })
  if(!all(are_identical)){
    stop("The MSE runs in `lst` have different years: eg - attributes(lst[[1]]$Catch)$dimnames$year",
         call. = FALSE)
  }
  ls_all[[1]]
}

#' Construct blank objects for Operating model outputs
#'
#' @param yrs A vector of years
#' @param ages A vector of ages
#' @param age_plus_grp Age for age plus group
#' @param max_surv_age The maximum age in the survey age comp data
#' @param n_space The number of spaces in the movement model
#' @param n_season The number of seasons in the movement model
#'
#' @return A list of the blank OM outputs
#' @export
setup_blank_om_objects <- function(yrs = NULL,
                                   ages = NULL,
                                   age_plus_grp = NULL,
                                   max_surv_age = NULL,
                                   n_space = NULL,
                                   n_season = NULL){

  n_yr <- length(yrs)
  n_age <- length(ages)

  lst <- NULL

  lst$ssb <- matrix(NA,
                    n_yr,
                    n_space,
                    dimnames = list(yrs = yrs,
                                    space = seq_len(n_space)))
  lst$ssb_all <- array(NA,
                       dim = c(n_yr, n_space, n_season),
                       dimnames = list(yrs = yrs,
                                       space = seq_len(n_space),
                                       season = seq_len(n_season)))
  lst$ssb_weight <- matrix(NA,
                           n_yr,
                           n_space,
                           dimnames = list(yrs = yrs,
                                           space = seq_len(n_space)))
  lst$biomass_save <- matrix(NA,
                             n_yr,
                             n_space,
                             dimnames = list(yrs = yrs,
                                             space = seq_len(n_space)))
  lst$catch <- matrix(NA,
                      n_yr,
                      dimnames = list(yrs = yrs))

  lst$catch_age <- matrix(NA,
                          n_age,
                          n_yr,
                          dimnames = list(ages = ages,
                                          yrs = yrs))
  lst$catch_n <- matrix(NA,
                        n_yr,
                        dimnames = list(yrs = yrs))
  lst$catch_n_age <- matrix(NA,
                            n_age,
                            n_yr,
                            dimnames = list(ages = ages,
                                            yrs = yrs))
  lst$f_sel_save <- array(NA,
                          dim = c(n_age, n_yr, n_space),
                          dimnames = list(ages = ages,
                                          yrs = yrs,
                                          space = seq_len(n_space)))
  lst$f_season_save <- array(NA,
                             dim = c(n_age, n_yr, n_space, n_season),
                             dimnames = list(ages = ages,
                                             yrs = yrs,
                                             space = seq_len(n_space),
                                             season = seq_len(n_season)))
  lst$f_out_save <- array(NA,
                          dim = c(n_yr, n_season, n_space),
                          dimnames = list(yrs = yrs,
                                          season = seq_len(n_season),
                                          space = seq_len(n_space)))
  lst$n_save_age <- array(NA,
                          dim = c(n_age, n_yr + 1, n_space, n_season),
                          dimnames = list(ages = ages,
                                          yrs = c(yrs, max(yrs) + 1),
                                          space = seq_len(n_space),
                                          season = seq_len(n_season)))
  lst$n_save_age_mid <- array(NA,
                              dim = c(n_age, n_yr + 1, n_space, n_season),
                              dimnames = list(ages = ages,
                                              yrs = c(yrs, max(yrs) + 1),
                                              space = seq_len(n_space),
                                              season = seq_len(n_season)))
  lst$r_save <- matrix(NA,
                       n_yr,
                       n_space)
  lst$v_save <- array(NA,
                      dim = c(n_yr, n_space, n_season),
                      dimnames = list(yrs = yrs,
                                      space = seq_len(n_space),
                                      season = seq_len(n_season)))
  lst$catch_save_age <- array(NA,
                              dim = c(n_age, n_yr, n_space, n_season),
                              dimnames = list(ages = ages,
                                              yrs = yrs,
                                              space = seq_len(n_space),
                                              season = seq_len(n_season)))

  lst$catch_n_save_age <- array(NA,
                                dim = c(n_age, n_yr, n_space, n_season),
                                dimnames = list(ages = ages,
                                                yrs = yrs,
                                                space = seq_len(n_space),
                                                season = seq_len(n_season)))
  lst$catch_quota <- array(NA,
                           dim = c(n_yr, n_space, n_season),
                           dimnames = list(yrs = yrs,
                                           space = seq_len(n_space),
                                           season = seq_len(n_season)))
  lst$catch_quota_n <- array(0,
                             dim = c(n_yr, n_space, n_season),
                             dimnames = list(yrs = yrs,
                                             space = seq_len(n_space),
                                             season = seq_len(n_season)))
  # lst$survey <- array(NA,
  #                 dim = c(n_yr),
  #                 dimnames = list(yrs = yrs))
  lst$survey_true <- array(NA,
                           dim = c(n_space, n_yr),
                           dimnames = list(space = seq_len(n_space),
                                           yrs = yrs))
  lst$surv_tot <- matrix(NA,
                         n_yr,
                         n_space,
                         dimnames = list(yrs = yrs,
                                         space = seq_len(n_space)))

  lst$age_comps_surv <- array(NA,
                              dim = c(max_surv_age, n_yr),
                              dimnames = list(ages = seq_len(max_surv_age),
                                              yrs = yrs))
  lst$age_comps_surv_space <- array(NA,
                                    dim = c(max(ages), n_yr, n_space),
                                    dimnames = list(ages = seq_len(max(ages)),
                                                    yrs = yrs))
  lst$age_comps_catch <- array(NA,
                               dim = c(max_surv_age, n_yr),
                               dimnames = list(ages = seq_len(max_surv_age),
                                               yrs = yrs))

  lst$age_comps_catch_space <- array(NA,
                                     dim = c(age_plus_grp, n_yr, n_space),
                                     dimnames = list(ages = seq_len(age_plus_grp),
                                                     yrs = yrs,
                                                     space = seq_len(n_space)))
  lst$age_comps_om <- array(NA,
                            dim = c(n_age, n_yr, n_space,n_season),
                            dimnames = list(ages = ages,
                                            yrs= yrs,
                                            space = seq_len(n_space),
                                            season = seq_len(n_season)))
  lst$z_save <- array(NA,
                      dim = c(n_age, n_yr, n_space, n_season),
                      dimnames = list(ages = ages,
                                      yrs = yrs,
                                      space = seq_len(n_space),
                                      season = seq_len(n_season)))

  lst
}

#' Extract row(s) of age data from a [data.frame]
#'
#' @param d A [data.frame] in the format of the `wage_*` data frames as
#' output by [load_data_om()]
#' @param yr A vector of years to extract row of data for
#'
#' @return A data frame with the rows requested
#' @export
get_age_dat <- function(d = NULL,
                        yr = NULL){

  verify_argument(d, "data.frame")
  verify_argument(yr, c("integer", "numeric"))
  stopifnot("Yr" %in% names(d))

  # Using the pipes here significantly slows down the code, so use base code
  #d %>% filter(Yr %in% yr) %>% select(-c(Yr))
  d[d$Yr %in% yr, -1]
}

#' Modify the `yr` row in the `wage` [data.frame], copying data from the `yr_copy` row
#'
#' @param wage A weight-at-age [data.frame] as created by [load_ss_model_data()]
#' @param yr The year to modify the row data for in the [data.frame]
#' @param yr_copy The year to use as source data row for the copy. If `NULL`, the first row will be used
#'
#' @return The same `wage` [data.frame] with the year row modified
#' @export
modify_wage_df <- function(wage = NULL, yr = NULL, yr_copy = NULL){

  verify_argument(wage, "data.frame")
  verify_argument(yr, c("integer", "numeric"), 1)
  stopifnot("Yr" %in% names(wage))

  line <- which(wage$Yr == yr)
  if(is.null(yr_copy)){
    line_copy <- 1
  }else{
    line_copy <- which(wage$Yr == yr_copy)
  }

  if(!nrow(wage)){
    warning("The wage data frame provided is empty")
    return(wage)
  }
  if(!length(line)){
    warning("The year ", yr, " was not found in the wage data frame provided")
    return(wage)
  }
  if(!length(line_copy)){
    warning("The yr_copy year ", yr, " was not found in the wage data frame provided")
    return(wage)
  }
  if(length(line) > 1){
    warning("The year ", yr, " occurs multiple times in the wage data frame provided")
    return(wage)
  }
  if(length(line_copy) > 1){
    warning("The yr_copy year ", yr, " occurs multiple times in the wage data frame provided")
    return(wage)
  }
  if(line == line_copy){
    warning("The year to copy from is the same as the year to copy to")
    return(wage)
  }

  wage[line, -1] <- wage[line_copy, -1]

  wage
}

#' Get the arguments of the calling function
#'
#' @return a list of the arguments
#' @export
get_args <- function(){
  cl <- sys.call(-1)
  f <- get(as.character(cl[[1]]), mode = "function", sys.frame(-2))
  cl <- match.call(definition = f, call = cl)
  as.list(cl)[-1]
}

#' Extract the vectors from a list into a [tibble::tibble()].
#' Used in [fetch_extra_mcmc()]
#'
#' @param reps_lst A list of vectors, all the same length and structure,
#' typically extracted as a portion of a Report.sso file
#' @param header A vector of column names for the new table
#'
#' @return A [tibble::tibble()] representing one row for each of the list
#'  elements found in `reps_lst`. A new column called `Iter` is prepended and
#'  represents the list element number that the data for each row came from.
#'  List elements that are NA will not be included in the table.
extract_rep_table <- function(reps_lst, header){
  lst <- map2(reps_lst, 1:length(reps_lst), ~{
    if(is.na(.x[1])){
      return(NULL)
    }
    vecs <- str_split(.x, " +")
    vec_lengths <- map_int(vecs, ~{length(.x)})
    vec_maxlength <- max(vec_lengths)
    vecs <- map(vecs, ~{
      length(.x) <- vec_maxlength
      .x
    })
    tab <- do.call(rbind, vecs) %>%
      as_tibble()
    names(tab) <- header
    tab %>%
      add_column(Iter = .y, .before = 1)
  })
  do.call(rbind, lst) %>%
    as_tibble(.name_repair = "minimal")
}

#' Append objects to list
#'
#' @param lst The list to append to
#' @param ... The objects to append
#'
#' @importFrom purrr map_chr
#' @return The modified list
append_objs_to_list <- function(lst = NULL,
                           ...){
  verify_argument(lst, "list")

  ellipsis <- list(...)
  arg_names <- get_args()[-1]
  nms <- map_chr(arg_names, ~{
    nm <- .x %>% as.character() %>% tail(1)
  })
  names(ellipsis) <- nms
  lst <- c(lst, ellipsis)
  new_lst_names <- names(lst)[names(lst) != ""]
  if(length(unique(new_lst_names)) != length(new_lst_names)){
    stop("List contains multiple elements with the same name. The duplicated names are:\n",
         paste(new_lst_names[duplicated(new_lst_names)], collapse = " "),
         call. = FALSE)
  }
  lst
}

#' Extract the weight-at-age data for the given Fleet
#'
#' @param df A weight-at-age [data.frame]
#' @param fleet The fleet number to extract
#'
#' @return A [data.frame]
format_wage_df <- function(df = NULL,
                           fleet = NULL){

  verify_argument(df, "data.frame")
  verify_argument(fleet, c("integer", "numeric"), 1)

  df %>%
    filter(Fleet == fleet) %>%
    select(-Fleet)
}

#' Convert the output of [format_wage_df()] into a matrix of correct dimensions
#' for the TMB input
#'
#' @param df  A weight-at-age [data.frame] as output from [format_wage_df()]
#'
#' @return A [matrix]
format_wage_matrix <- function(df = NULL){
  df %>%
    select(-Yr) %>%
    as.matrix() %>%
    t()
}

#' Extract parameter estimates from the linear par vector returned by TMB
#' to a list with parameter names and values
#'
#' @param obj The output from the TMB model
#'
#' @importFrom utils tail
#' @return A named [list] of parameter estimates extracted from `obj$par`
extract_params_tmb <- function(obj){
  par <- obj$par
  nms <- unique(names(par))
  map(nms, ~{
    par[names(par) == .x] %>%
      unname()
  }) %>%
    set_names(nms)
}

df_identical <- function(wa1, wa2, nm_wa1, nm_wa2, diff_tol = 1e-20){
  if(class(wa1) != "matrix" || class(wa2) != "matrix"){
    stop("Both ", nm_wa1, " and ", nm_wa2, " must be class matrix",
         call. = FALSE)
  }
  if(!identical(dim(wa1), dim(wa2))){
    stop(nm_wa1, " not identical to ", nm_wa2, ". They have different dimensions",
         call. = FALSE)
  }
  #if(identical(wa1, wa2)){
  #  message(nm_wa1, " is identical to ", nm_wa2, "\n")
  #}
  wa1 <- as.data.frame(wa1)
  wa2 <- as.data.frame(wa2)
  wa_cols_ident <- map_lgl(seq_len(ncol(wa1)), ~{
    if(all(is.na(wa1[,.x]))){
      if(all(is.na(wa2[,.x]))){
        return(TRUE)
      }
      return(FALSE)
    }
    diff <- wa1[,.x] %>%
      as_tibble() %>%
      add_column(wa2[, .x]) %>%
      mutate(diff = .[[1]] - .[[2]]) %>%
      mutate(in_tol = diff < diff_tol)
    if(all(diff$in_tol)){
      return(TRUE)
    }
    FALSE
  })
  if(!all(wa_cols_ident)){
    stop(nm_wa1, " not identical to ", nm_wa2, ". These columns are not identical: ",
         paste(which(!wa_cols_ident), collapse = " "),
         call. = FALSE)
  }
}


#' Get likelihood values for a model object the begin with 'ans'
#'
#' @param report The object returned by the `report()` function of the returned object of the minimizer.
#'
#' @return A list of the likelihoods whose names begin with 'ans'. Add these in pacifichakemse.cpp at the end as a REPORT
#' @export
#'
#' @examples
#' \dontrun{
#' obj <- MakeADFun(d, p, DLL = "pacifichakemse", silent = FALSE)
#' report <- obj$report()
#' get_likelihoods(report)
#' }
get_likelihoods <- function(report){
  map2(names(report), report, ~{if(length(grep("ans", .x))){ret <- .y;names(ret) <- .x;ret}}) %>%
    unlist() %>%
    `[`(!is.na(names(.)))
}

#' Compare two data object inputs for input into `pacifichakemse.cpp`
#'
#' @description Used while developing the code to compare old and new data being input.
#' They need to be exactly the same
#'
#' @param d1 Data object 1
#' @param d2 Data object 2
#' @param p1 Parameter object 1
#' @param p2 Parameter object 2
#'
#' @export
compare_tmb_data <- function(d1, d2, p1, p2){
  d1 <- d1[order(names(d1))]
  d2 <- d2[order(names(d2))]

  d <- map2(d1, d2, ~{
    identical(.x, .y)
  }) %>%
    map_df(~{.x}) %>%
    t() %>%
    as_tibble(rownames = "name", .name_repair = "minimal") %>%
    rename(is_identical = 2) %>%
    mutate(type = "data")

  p1 <- p1[order(names(p1))]
  p2 <- p2[order(names(p2))]
  p <- map2(p1, p2, ~{
    identical(.x, .y)
  }) %>%
    map_df(~{.x}) %>%
    t() %>%
    as_tibble(rownames = "name", .name_repair = "minimal") %>%
    rename(is_identical = 2) %>%
    mutate(type = "parameter")

  bind_rows(d, p)
}

#' Color the backgrounds of the facet labels in a ggplot object
#'
#' @param g The ggplot object
#' @param facet_back_cols A vector of the colors to apply to the facet backgrounds
#' @param facet_back_alpha transparency between 0 and 99
#'
#' @return The modified ggplot object
#' @importFrom ggplot2 ggplot_gtable ggplot_build
#' @importFrom grid grid.draw
#' @export
color_facet_backgrounds <- function(g = NULL,
                                    facet_back_cols = NULL,
                                    facet_back_alpha = 99){

  verify_argument(g, "ggplot")
  verify_argument(facet_back_cols, "character")
  verify_argument(facet_back_alpha, c("integer", "numeric"), 1)

  if(facet_back_alpha < 0){
    facet_back_alpha <- 0
  }
  if(facet_back_alpha > 99){
    facet_back_alpha <- 99
  }
  if(facet_back_alpha < 10){
    # Needs to be two digits for the alpha string
    facet_back_alpha <- paste0("0", facet_back_alpha)
  }

  # Add scenario colors to the strip backgrounds
  gt <- ggplot_gtable(ggplot_build(g))
  strip <- which(grepl("strip-t", gt$layout$name))
  if(!length(strip)){
    warning("The ggplot object does not contain facets, returning the original object")
    return(g)
  }
  if(length(facet_back_cols) < length(strip)){
    warning("The facet_back_cols vector is shorter than the number of facet backgrounds. Recycling the colors")
    facet_back_cols <- rep(facet_back_cols, length(strip) %/% length(facet_back_cols) + 1)
  }
  # Need to re-order gt$layout$name[strip] as they are out of order and have the format strip-t-1-2 etc
  # This is accomplished by reversing the numbers in the ordered strip text eg. strip-t-1-2 becomes strip-t-2-1
  # If this swapping leads to strip text which doesn't exist in the grob layout, use the original text instead.
  swap_rows_cols <- function(vec){
    out <- NULL
    for(i in seq_along(vec)){
      out[i] <- gsub("strip-t-([1-9]+)-([1-9]+)", "strip-t-\\2-\\1", vec[i])
    }
    out
  }
  tmp_layout_name <- sort(gt$layout$name[strip])
  strip_tmp <- match(tmp_layout_name, gt$layout$name)
  tmp_layout_name <- swap_rows_cols(tmp_layout_name)
  strip <- match(tmp_layout_name, gt$layout$name)
  if(any(is.na(strip))){
    strip <- strip_tmp
  }
  facet_back_cols <- facet_back_cols[seq_along(strip)]

  k <- 1
  for(i in strip){
    j <- which(grepl('rect', gt$grobs[[i]]$grobs[[1]]$childrenOrder))
    gt$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- paste0(col2hex(facet_back_cols[k]), facet_back_alpha)
    k <- k + 1
  }
  grid.draw(gt)
}

#' Convert color name to hex string. Code borrowed from [gplots::col2hex()]
#'
#' @param cname Color name as a common name. If a hex string is supplied, it will be returned unchanged.
#'
#' @return The HEX string representing the color
#' @importFrom grDevices rgb col2rgb
#' @export
col2hex <- function(cname){

  verify_argument(cname, "character", 1)

  col_mat <- col2rgb(cname)
  rgb(red = col_mat[1, ] / 255, green = col_mat[2, ] / 255, blue = col_mat[3, ] / 255)
}