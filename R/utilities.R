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

  stopifnot(!is.null(df))
  stopifnot(!is.null(col))
  stopifnot(!is.null(probs))
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
                                min_yr = NA,
                                max_yr = NA,
                                probs = c(0.05, 0.25, 0.5, 0.75, 0.95),
                                mean_multiplier = 1){
  stopifnot(!is.null(df))
  stopifnot(!is.null(grp_col))
  stopifnot(!is.null(col))
  stopifnot(grp_col %in% names(df))
  stopifnot(col %in% names(df))
  stopifnot(class(df[[col]]) == "numeric")
  grp_col_sym <- sym(grp_col)
  col_sym <- sym(col)
  stopifnot(!is.null(min_yr))
  stopifnot(!is.null(max_yr))
  stopifnot(!is.null(probs))
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


#' Convert a vector of aggregated values into a [data.frame] containing
#' the year, run, value, and possibly country as columns
#'
#' @param vec The aggregated vector of data. This must be the same length as `run_vec`
#' and `yr_vec`
#' @param col The name of the column to create in teh new data frame
#' @param yr_vec The vector of years to place in the [data.frame]. This must be the
#' same length as `vec`
#' @param country The name for the country. If `NULL`, no country column will be added
#' @param probs A vector of quantiles to pass to [stats::quantile()]
#' @param inc_mean Logical. Include the mean (as column `avg`)
#'
#' @return A [data.frame] containing the year, run, value, and possibly country as columns
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

#' Load data from CSV files
#'
#' @param sel_hist Logical. Load the PSEL file?
#'
#' @return A list of the loaded data.frames
#' @importFrom readr read_csv cols
#' @export
csv_data <- function(sel_hist = TRUE){
  load_from_csv <- function(file){
    read_csv(system.file(file.path("extdata", file),
                         package = "PacifichakeMSE",
                         mustWork = TRUE),
             col_types = cols())
  }

  wage_ss <- load_from_csv("wage_ss.csv")
  wage_unfished <- load_from_csv("unfished_waa.csv")
  catch <- load_from_csv("hake_totcatch.csv")
  # Survey abundance
  df_survey <- load_from_csv("acoustic_survey.csv")
  age_survey_df <- load_from_csv("agecomps_survey.csv")
  age_catch_df <- load_from_csv("agecomps_fishery.csv")
  survey <- load_from_csv("survey.csv")
  # Load the age comps
  age_survey_tmp <- load_from_csv("age_survey_ss.csv")
  age_catch_tmp <- load_from_csv("age_catch_ss.csv")
  ac_data <- load_from_csv("ac_data.csv")
  # Load parameters from the assessment
  # Not used
  # initN <- rev(load_from_csv("Ninit_MLE.csv")[,1])
  # Not used
  # Rdev <- load_from_csv("Rdev_MLE.csv")[,1]
  # Not used
  # PSEL <- as.matrix(load_from_csv("p_MLE.csv")
  b <- as.matrix(load_from_csv("b_input.csv"))
  # load parameters specifically for hake
  parms_scalar <- load_from_csv("parms_scalar.csv")
  parms_sel <- load_from_csv("selectivity.csv")
  initN <- as.matrix(load_from_csv("initN.csv"))
  Rdev <- as.matrix(load_from_csv("Rdev.csv"))
  psel <- NA
  if(sel_hist){
    psel <- as.matrix(load_from_csv("PSEL.csv"))
  }
  catch_country <- load_from_csv("catch_per_country.csv")

  list(wage_ss = wage_ss,
       wage_unfished = wage_unfished,
       catch = catch,
       df_survey = df_survey,
       age_survey_df = age_survey_df,
       age_catch_df = age_catch_df,
       survey = survey,
       age_survey_tmp = age_survey_tmp,
       age_catch_tmp = age_catch_tmp,
       ac_data = ac_data,
       b = b,
       parms_scalar = parms_scalar,
       parms_sel = parms_sel,
       initN = initN,
       Rdev = Rdev,
       psel = psel,
       catch_country = catch_country)
}

#' Construct blank objects for Operating model outputs
#'
#' @param yrs A vector of years
#' @param ages A vector of ages
#' @param nspace The number of spaces in the movement model
#' @param nseason The number of seasons in the movement model
#'
#' @return A list of the blank OM outputs
#' @export
setup_blank_om_objects <- function(yrs,
                                   ages,
                                   nspace,
                                   nseason){

  nyr <- length(yrs)
  nage <- length(ages)

  ssb <- matrix(NA,
                nyr,
                nspace,
                dimnames = list(yrs = yrs,
                                space = seq_len(nspace)))
  ssb_all <- array(NA,
                   dim = c(nyr, nspace, nseason),
                   dimnames = list(yrs = yrs,
                                   space = seq_len(nspace),
                                   season = seq_len(nseason)))
  ssb_weight <- matrix(NA,
                       nyr,
                       nspace,
                       dimnames = list(yrs = yrs,
                                       space = seq_len(nspace)))
  biomass_save <- matrix(NA,
                         nyr,
                         nspace,
                         dimnames = list(yrs = yrs,
                                         space = seq_len(nspace)))
  catch <- matrix(NA,
                  nyr,
                  dimnames = list(yrs = yrs))
  catch_age <- matrix(NA,
                      nage,
                      nyr,
                      dimnames = list(ages = ages,
                                      yrs = yrs))
  catch_n <- matrix(NA,
                    nyr,
                    dimnames = list(yrs = yrs))
  catch_n_age <- matrix(NA,
                        nage,
                        nyr,
                        dimnames = list(ages = ages,
                                        yrs = yrs))
  fsel_save <- array(NA,
                     dim = c(nage, nyr, nspace),
                     dimnames = list(ages = ages,
                                     yrs = yrs,
                                     space = seq_len(nspace)))
  fseason_ave <- array(NA,
                       dim = c(nage, nyr, nspace, nseason),
                       dimnames = list(ages = ages,
                                       yrs = yrs,
                                       space = seq_len(nspace),
                                       season = seq_len(nseason)))
  fout_save <- array(NA,
                     dim = c(nyr, nseason, nspace),
                     dimnames = list(yrs = yrs,
                                     season = seq_len(nseason),
                                     space = seq_len(nspace)))
  n_save_age <- array(NA,
                      dim = c(nage, nyr + 1, nspace, nseason),
                      dimnames = list(ages = ages,
                                      yrs = c(yrs, max(yrs) + 1),
                                      space = seq_len(nspace),
                                      season = seq_len(nseason)))
  n_save_age_mid <- array(NA,
                          dim = c(nage, nyr + 1, nspace, nseason),
                          dimnames = list(ages = ages,
                                          yrs = c(yrs, max(yrs) + 1),
                                          space = seq_len(nspace),
                                          season = seq_len(nseason)))
  r_save <- matrix(NA,
                   nyr,
                   nspace)
  v_save <- array(NA,
                  dim = c(nyr, nspace, nseason),
                  dimnames = list(yrs = yrs,
                                  space = seq_len(nspace),
                                  season = seq_len(nseason)))
  catch_save_age <- array(NA,
                          dim = c(nage, nyr, nspace, nseason),
                          dimnames = list(ages = ages,
                                          yrs = yrs,
                                          space = seq_len(nspace),
                                          season = seq_len(nseason)))
  catch_n_save_age <- array(NA,
                            dim = c(nage, nyr, nspace, nseason),
                            dimnames = list(ages = ages,
                                            yrs = yrs,
                                            space = seq_len(nspace),
                                            season = seq_len(nseason)))
  catch_quota <- array(NA,
                       dim = c(nyr, nspace, nseason),
                       dimnames = list(yrs = yrs,
                                       space = seq_len(nspace),
                                       season = seq_len(nseason)))
  catch_quota_n <- array(0,
                         dim = c(nyr, nspace, nseason),
                         dimnames = list(yrs = yrs,
                                         space = seq_len(nspace),
                                         season = seq_len(nseason)))
  survey <- array(NA,
                  dim = c(nyr),
                  dimnames = list(yrs = yrs))
  survey_true <- array(NA,
                       dim = c(nspace, nyr),
                       dimnames = list(space = seq_len(nspace),
                                       yrs = yrs))
  surv_tot <- matrix(NA,
                     nyr,
                     nspace,
                     dimnames = list(yrs = yrs,
                                     space = seq_len(nspace)))
  age_comps_surv <- array(NA,
                          dim = c(max(ages), nyr),
                          dimnames = list(ages = seq_len(max(ages)),
                                          yrs = yrs))
  age_comps_surv_space <- array(NA,
                                dim = c(max(ages), nyr, nspace),
                                dimnames = list(ages = seq_len(max(ages)),
                                                yrs = yrs))
  n_survey <- matrix(NA,
                     max(ages),
                     nyr,
                     dimnames = list(ages = seq_len(max(ages)),
                                     yrs= yrs))
  age_comps_catch <- array(NA,
                           dim = c(max(ages), nyr),
                           dimnames = list(ages = seq_len(max(ages)),
                                           yrs = yrs))
  age_comps_catch_space <- array(NA,
                                 dim = c(max(ages), nyr, nspace),
                                 dimnames = list(ages = seq_len(max(ages)),
                                                 yrs = yrs,
                                                 space = seq_len(nspace)))
  age_comps_om <- array(NA,
                        dim = c(nage, nyr, nspace,nseason),
                        dimnames = list(ages = ages,
                                        yrs= yrs,
                                        space = seq_len(nspace),
                                        season = seq_len(nseason)))
  z_save <- array(NA,
                  dim = c(nage, nyr, nspace, nseason),
                  dimnames = list(ages = ages,
                                  yrs = yrs,
                                  space = seq_len(nspace),
                                  season = seq_len(nseason)))
  list(ssb = ssb,
       ssb_all = ssb_all,
       ssb_weight = ssb_weight,
       biomass_save = biomass_save,
       catch = catch,
       catch_age = catch_age,
       catch_n = catch_n,
       catch_n_age = catch_n_age,
       fsel_save = fsel_save,
       fseason_ave = fseason_ave,
       fout_save = fout_save,
       n_save_age = n_save_age,
       n_save_age_mid = n_save_age_mid,
       r_save = r_save,
       v_save = v_save,
       catch_save_age = catch_save_age,
       catch_n_save_age = catch_n_save_age,
       catch_quota = catch_quota,
       catch_quota_n = catch_quota_n,
       survey = survey,
       survey_true = survey_true,
       surv_tot = surv_tot,
       age_comps_surv = age_comps_surv,
       age_comps_surv_space = age_comps_surv_space,
       n_survey = n_survey,
       age_comps_catch = age_comps_catch,
       age_comps_catch_space = age_comps_catch_space,
       age_comps_om = age_comps_om,
       z_save = z_save)
}

#' Extract row(s) of age data from a [data.frame]
#'
#' @param d A [data.frame] in the format of the `wage_*` data frames as
#' output by [load_data_seasons()]
#' @param yr A vector of years to extract row of data for
#'
#' @return A data frame with the rows requested
#' @export
get_age_dat <- function(d = NULL,
                        yr = NULL){
  stopifnot(!is.null(d))
  stopifnot(!is.null(yr))
  stopifnot(is.numeric(yr))
  stopifnot("data.frame" %in% class(d))
  stopifnot("Fleet" %in% names(d))
  stopifnot("Yr" %in% names(d))

  d %>% filter(Yr %in% yr) %>% select(-c(Yr, Fleet))
}

#' Verify that the argument `arg` is valid in the context of the arguments given
#'
#' @param arg The object to check
#' @param chk_class A character string of the name of the class to ensure `arg` is
#' @param chk_len A numeric value to ensure `arg` has the length of
#' @param chk_is_in A vector of values to ensure `arg` is in
#'
#' @return TRUE, invisibly or the function will throw an error if `arg` does not follow
#' the constraints given; FALSE is not returned
#' @export
#'
#' @examples
#' verify_argument(23, "numeric", 1) # Succeeds
#' verify_argument(23, "numeric", 1, 1:20) # Fails
verify_argument <- function(arg = NULL,
                            chk_class = NULL,
                            chk_len = NULL,
                            chk_is_in = NULL){

  stopifnot(!is.null(arg))
  stopifnot(is.null(chk_class) | length(chk_class) == 1)
  stopifnot(is.null(chk_len) | length(chk_len) == 1)

  calling_func_name <- func_name(levels_up = 2)
  if(!is.null(chk_class)){
    if(class(arg) != chk_class){
      message("Error from calling function ", calling_func_name, ":")
      stop("class(arg) == chk_class is not TRUE")
    }
  }
  if(!is.null(chk_len)){
    if(length(arg) != chk_len){
      message("Error from calling function ", calling_func_name, ":")
      stop("length(arg) == chk_len is not TRUE")
    }
  }
  if(!is.null(chk_is_in)){
    if(sum(!is.na(match(chk_is_in, arg))) != length(arg)){
      message("Error from calling function ", calling_func_name, ":")
      stop("Not all values in arg are in chk_is_in")
    }
  }
  invisible(TRUE)
}

#' Get a calling function's name from within the function
#'
#' @param skip_frames The level in the calling stack to look. 1 is in the current
#' function, 2 is one before, etc.
#' @param skip_names Names returned to skip, these are not real function names but
#' generalized values used internally
#' @param ret_stack If TRUE, return the stack trace
#' @param extra_perf_per_level This is prepended by R and will be removed from the output
#'
#' @return The name of the calling function at level `skip_frames` in the stack trace
#' @export
fn_finder <- function(skip_frames = 1,
                      skip_names = "(FUN)|(.+apply)|(replicate)",
                      ret_stack = FALSE,
                      extra_perf_per_level = "\t"){

  prefix <- sapply(3 + skip_frames + 1:sys.nframe(), function(i){
    sys.call(sys.parent(n = i))[[1]]
  })
  prefix[grep(skip_names, prefix)] <- NULL
  prefix <- gsub("function \\(.*", "do.call", prefix)
  if(length(prefix)==0){
    stop("Could not find any calling function at stack level ", skip_frames,
         call. = FALSE)
  }else if(ret_stack){
    paste(rev(prefix), collapse = "|")
  }else{
    retval <- as.character(unlist(prefix[1]))
    if(length(prefix) > 1){
      retval <- paste0(paste(rep(extra_perf_per_level,
                                 length(prefix) - 1),
                             collapse = ""),
                       retval)
    }
    retval
  }
}

#' Returns a calling function's name `levels_up` levels up the stack trace
#'
#' @param levels_up How many levels back in the stack trace to look for the
#' function name
#'
#' @return A calling function's name `levels_up` levels up the stack trace
#' @export
#'
#' @examples
#' f <- function(){
#'   message("You are in ", func_name())
#' }
func_name <- function(levels_up = 1){
  stopifnot(!is.null(levels_up))
  stopifnot(class(levels_up) == "numeric")
  stopifnot(length(levels_up) == 1)
  stopifnot(levels_up >= 0)

  fn_name <- fn_finder(skip_frames = levels_up)
  fn_name <- gsub("\t+", "", fn_name)
  fn_name <- gsub("\ +", "", fn_name)
  fn_name
}
