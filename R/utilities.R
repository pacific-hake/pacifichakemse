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

#' Load data from CSV files
#'
#' @param sel_hist Logical. Load the PSEL file?
#'
#' @return A list of the loaded [data.frame]s
#' @importFrom readr read_csv cols
#' @export
csv_data <- function(sel_hist = TRUE){
  load_from_csv <- function(file){
    read_csv(system.file(file.path("extdata", file),
                         package = "PacifichakeMSE",
                         mustWork = TRUE),
             col_types = cols(),
             comment = "#")
  }
  lst <- NULL
  lst$wage_ss <- load_from_csv("wage_ss.csv")
  lst$wage_unfished <- load_from_csv("unfished_waa.csv")
  lst$catch <- load_from_csv("hake_totcatch.csv") %>%
    transmute(year, value = Fishery)
  lst$b <- as.matrix(load_from_csv("b_input.csv"))
  lst$init_n <- as.matrix(load_from_csv("initN.csv"))
  lst$r_dev <- as.matrix(load_from_csv("Rdev.csv"))
  lst$p_sel <- NA
  if(sel_hist){
    lst$p_sel <- as.matrix(load_from_csv("PSEL.csv"))
  }
  lst$catch_country <- load_from_csv("catch_per_country.csv")

  lst
}

#' Construct blank objects for Operating model outputs
#'
#' @param yrs A vector of years
#' @param ages A vector of ages
#' @param max_surv_age The maximum age in the survey age comp data
#' @param n_space The number of spaces in the movement model
#' @param n_season The number of seasons in the movement model
#'
#' @return A list of the blank OM outputs
#' @export
setup_blank_om_objects <- function(yrs,
                                   ages,
                                   max_surv_age,
                                   n_space,
                                   n_season){

  n_yr <- length(yrs)
  n_age <- length(ages)

  ssb <- matrix(NA,
                n_yr,
                n_space,
                dimnames = list(yrs = yrs,
                                space = seq_len(n_space)))
  ssb_all <- array(NA,
                   dim = c(n_yr, n_space, n_season),
                   dimnames = list(yrs = yrs,
                                   space = seq_len(n_space),
                                   season = seq_len(n_season)))
  ssb_weight <- matrix(NA,
                       n_yr,
                       n_space,
                       dimnames = list(yrs = yrs,
                                       space = seq_len(n_space)))
  biomass_save <- matrix(NA,
                         n_yr,
                         n_space,
                         dimnames = list(yrs = yrs,
                                         space = seq_len(n_space)))
  catch <- matrix(NA,
                  n_yr,
                  dimnames = list(yrs = yrs))
  catch_age <- matrix(NA,
                      n_age,
                      n_yr,
                      dimnames = list(ages = ages,
                                      yrs = yrs))
  catch_n <- matrix(NA,
                    n_yr,
                    dimnames = list(yrs = yrs))
  catch_n_age <- matrix(NA,
                        n_age,
                        n_yr,
                        dimnames = list(ages = ages,
                                        yrs = yrs))
  f_sel_save <- array(NA,
                      dim = c(n_age, n_yr, n_space),
                      dimnames = list(ages = ages,
                                      yrs = yrs,
                                      space = seq_len(n_space)))
  f_season_save <- array(NA,
                         dim = c(n_age, n_yr, n_space, n_season),
                         dimnames = list(ages = ages,
                                         yrs = yrs,
                                         space = seq_len(n_space),
                                         season = seq_len(n_season)))
  f_out_save <- array(NA,
                      dim = c(n_yr, n_season, n_space),
                      dimnames = list(yrs = yrs,
                                      season = seq_len(n_season),
                                      space = seq_len(n_space)))
  n_save_age <- array(NA,
                      dim = c(n_age, n_yr + 1, n_space, n_season),
                      dimnames = list(ages = ages,
                                      yrs = c(yrs, max(yrs) + 1),
                                      space = seq_len(n_space),
                                      season = seq_len(n_season)))
  n_save_age_mid <- array(NA,
                          dim = c(n_age, n_yr + 1, n_space, n_season),
                          dimnames = list(ages = ages,
                                          yrs = c(yrs, max(yrs) + 1),
                                          space = seq_len(n_space),
                                          season = seq_len(n_season)))
  r_save <- matrix(NA,
                   n_yr,
                   n_space)
  v_save <- array(NA,
                  dim = c(n_yr, n_space, n_season),
                  dimnames = list(yrs = yrs,
                                  space = seq_len(n_space),
                                  season = seq_len(n_season)))
  catch_save_age <- array(NA,
                          dim = c(n_age, n_yr, n_space, n_season),
                          dimnames = list(ages = ages,
                                          yrs = yrs,
                                          space = seq_len(n_space),
                                          season = seq_len(n_season)))

  catch_n_save_age <- array(NA,
                            dim = c(n_age, n_yr, n_space, n_season),
                            dimnames = list(ages = ages,
                                            yrs = yrs,
                                            space = seq_len(n_space),
                                            season = seq_len(n_season)))
  catch_quota <- array(NA,
                       dim = c(n_yr, n_space, n_season),
                       dimnames = list(yrs = yrs,
                                       space = seq_len(n_space),
                                       season = seq_len(n_season)))
  catch_quota_n <- array(0,
                         dim = c(n_yr, n_space, n_season),
                         dimnames = list(yrs = yrs,
                                         space = seq_len(n_space),
                                         season = seq_len(n_season)))
  # survey <- array(NA,
  #                 dim = c(n_yr),
  #                 dimnames = list(yrs = yrs))
  survey <- NULL
  survey_true <- array(NA,
                       dim = c(n_space, n_yr),
                       dimnames = list(space = seq_len(n_space),
                                       yrs = yrs))
  surv_tot <- matrix(NA,
                     n_yr,
                     n_space,
                     dimnames = list(yrs = yrs,
                                     space = seq_len(n_space)))

  age_comps_surv <- array(NA,
                          dim = c(max_surv_age, n_yr),
                          dimnames = list(ages = seq_len(max_surv_age),
                                          yrs = yrs))
  age_comps_surv_space <- array(NA,
                                dim = c(max(ages), n_yr, n_space),
                                dimnames = list(ages = seq_len(max(ages)),
                                                yrs = yrs))
  n_survey <- matrix(NA,
                     max(ages),
                     n_yr,
                     dimnames = list(ages = seq_len(max(ages)),
                                     yrs= yrs))
  age_comps_catch <- array(NA,
                           dim = c(max_surv_age, n_yr),
                           dimnames = list(ages = seq_len(max_surv_age),
                                           yrs = yrs))
  age_comps_catch_space <- array(NA,
                                 dim = c(max(ages), n_yr, n_space),
                                 dimnames = list(ages = seq_len(max(ages)),
                                                 yrs = yrs,
                                                 space = seq_len(n_space)))
  age_comps_om <- array(NA,
                        dim = c(n_age, n_yr, n_space,n_season),
                        dimnames = list(ages = ages,
                                        yrs= yrs,
                                        space = seq_len(n_space),
                                        season = seq_len(n_season)))
  z_save <- array(NA,
                  dim = c(n_age, n_yr, n_space, n_season),
                  dimnames = list(ages = ages,
                                  yrs = yrs,
                                  space = seq_len(n_space),
                                  season = seq_len(n_season)))
  list(ssb = ssb,
       ssb_all = ssb_all,
       ssb_weight = ssb_weight,
       biomass_save = biomass_save,
       catch = catch,
       catch_age = catch_age,
       catch_n = catch_n,
       catch_n_age = catch_n_age,
       f_sel_save = f_sel_save,
       f_season_save = f_season_save,
       f_out_save = f_out_save,
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

  verify_argument(d, "data.frame")
  verify_argument(yr, c("integer", "numeric"))
  stopifnot("Yr" %in% names(d))

  d %>% filter(Yr %in% yr) %>% select(-c(Yr))
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
#' \dontrun{
#' verify_argument(23, "numeric", 1) # Succeeds
#' verify_argument(23, "numeric", 1, 1:20) # Fails
#' }
verify_argument <- function(arg = NULL,
                            chk_class = NULL,
                            chk_len = NULL,
                            chk_is_in = NULL){

  calling_func_name <- func_name(levels_up = 2)
  calling_args <- get_args()
  if(is.null(arg)){
    message("Error from calling function ", calling_func_name, ":")
    stop("is.null(", calling_args$arg, ") is TRUE",
         call. = FALSE)
  }
  if(!is.null(chk_len) & length(chk_len) != 1){
    message("Error from calling function ", calling_func_name, ":")
    stop("length(", calling_args$chk_len, ") is not equal to 1",
         call. = FALSE)
  }
  if(!is.null(chk_class)){
    if(!any(class(arg) %in% chk_class)){
      message("Error from calling function ", calling_func_name, ":")
      stop("Class requirements (", calling_args$chk_class,
           ") do not include the actual class of ", calling_args$arg, " (",
           class(arg), ")",
           call. = FALSE)
    }
  }
  if(!is.null(chk_len)){
    if(length(arg) != chk_len){
      message("Error from calling function ", calling_func_name, ":")
      stop("length(", calling_args$arg, ") == ", calling_args$chk_len, " is not TRUE",
           call. = FALSE)
    }
  }
  if(!is.null(chk_is_in)){
    if(sum(!is.na(match(chk_is_in, arg))) != length(arg)){
      message("Error from calling function ", calling_func_name, ":")
      stop("Not all values in ", calling_args$arg, " are in ", calling_args$chk_is_in,
           call. = FALSE)
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

#' Add a new row to the bottom of the `wage` [data.frame], which is a copy of the
#' last row with the `Yr` value being one more than the last row's
#'
#' @param wage A weight-at-age [data.frame] as created by [load_data_ss()]
#'
#' @return The same `wage` [data.frame] with a new row added to the bottom,
#' which is a copy of the last row with the `Yr` value being one more than
#' the last row's
#' @export
wage_add_yr <- function(wage = NULL){

  verify_argument(wage, "data.frame")
  stopifnot(nrow(wage) >= 1)
  stopifnot("Yr" %in% names(wage))

  last_wage_yr <- wage[nrow(wage),]$Yr
  wage <- wage %>%
    bind_rows(wage[nrow(wage),])
  wage[nrow(wage),]$Yr <- last_wage_yr + 1
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
