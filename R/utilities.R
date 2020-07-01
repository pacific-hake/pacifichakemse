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
#' @return A list of the loaded [data.frame]s
#' @importFrom readr read_csv cols
#' @export
csv_data <- function(){
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
  lst$r_dev <- as.matrix(load_from_csv("Rdev.csv"))
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
  lst$survey <- array(NA,
                  dim = c(n_yr),
                  dimnames = list(yrs = yrs))
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
  lst$n_survey <- matrix(NA,
                         max(ages),
                         n_yr,
                         dimnames = list(ages = seq_len(max(ages)),
                                         yrs= yrs))
  lst$age_comps_catch <- array(NA,
                               dim = c(max_surv_age, n_yr),
                               dimnames = list(ages = seq_len(max_surv_age),
                                               yrs = yrs))
  lst$age_comps_catch_space <- array(NA,
                                     dim = c(max(ages), n_yr, n_space),
                                     dimnames = list(ages = seq_len(max(ages)),
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
    bind_rows(wage[1,])
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

#' Compare two data object inputs for input into `runHakeassessment.cpp`
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

  if("survey_x" %in% names(d2)){
    d2$survey_x <- NULL
  }
  if(!identical(d1$yrs, d2$years)){
    stop("d1$yrs not identical to d2$years", call. = FALSE)
  }
  if(!identical(d1$t_end, d2$tEnd)){
    stop("d1$t_end not identical to d2$tEnd", call. = FALSE)
  }
  if(!identical(d1$sel_change_yr, d2$selYear)){
    stop("d1$sel_change_yr not identical to d2$selYear", call. = FALSE)
  }
  if(!identical(d1$yr_sel, d2$year_sel)){
    stop("d1$yr_sel not identical to d2$year_sel", call. = FALSE)
  }
  if(!identical(d1$b, d2$b)){
    stop("d1$b not identical to d2$b", call. = FALSE)
  }
  if(!identical(d1$ages, d2$age)){
    stop("d1$ages not identical to d2$age", call. = FALSE)
  }
  if(!identical(d1$rdev_sd, d2$logSDR)){
    stop("d1$rdev_sd not identical to d2$logSDR", call. = FALSE)
  }
  if(!identical(d1$log_q, d2$logQ)){
    stop("d1$log_q not identical to d2$logQ", call. = FALSE)
  }
  if(!identical(d1$log_sd_catch, d2$logSDcatch)){
    stop("d1$log_sd_catch not identical to d2$logSDcatch", call. = FALSE)
  }
  if(!identical(d1$log_phi_survey, d2$logphi_survey)){
    stop("d1$log_phi_survey not identical to d2$logphi_survey", call. = FALSE)
  }
  if(!identical(d1$s_mul, d2$smul)){
    stop("d1$s_mul not identical to d2$smul", call. = FALSE)
  }
  if(!identical(d1$sigma_p_sel, d2$sigma_psel)){
    stop("d1$sigma_p_sel not identical to d2$sigma_psel", call. = FALSE)
  }
  if(!identical(d1$sum_zero, d2$sum_zero)){
    stop("d1$sum_zero not identical to d2$sum_zero", call. = FALSE)
  }
  if(!identical(d1$s_min, d2$Smin)){
    stop("d1$s_min not identical to d2$Smin", call. = FALSE)
  }
  if(!identical(d1$s_max, d2$Smax)){
    stop("d1$s_max not identical to d2$Smax", call. = FALSE)
  }
  if(!identical(d1$s_min_survey, d2$Smin_survey)){
    stop("d1$s_min_survey not identical to d2$Smin_survey", call. = FALSE)
  }
  if(!identical(d1$s_max_survey, d2$Smax_survey)){
    stop("d1$s_max_survey not identical to d2$Smax_survey", call. = FALSE)
  }
  if(!identical(d1$n_age, d2$nage)){
    stop("d1$n_age not identical to d2$nage", call. = FALSE)
  }
  if(!identical(d1$m_sel, d2$Msel)){
    stop("d1$m_sel not identical to d2$Msel", call. = FALSE)
  }
  if(!identical(d1$flag_sel, d2$flag_sel)){
    stop("d1$flag_sel not identical to d2$flag_sel", call. = FALSE)
  }
  if(!identical(d1$flag_survey, d2$flag_survey)){
    stop("d1$flag_survey not identical to d2$flag_survey", call. = FALSE)
  }
  if(!identical(d1$flag_catch, d2$flag_catch)){
    stop("d1$flag_catch not identical to d2$flag_catch", call. = FALSE)
  }

  if(class(d1$catch_obs) != "matrix" | class(d2$Catchobs) != "matrix"){
    stop("Both d1$catch_obs and d2$Catchobs must be class matrix",
         call. = FALSE)
  }
  if(!identical(dim(d1$catch_obs), dim(d2$Catchobs))){
    stop("d1$catch_obs not identical to d2$Catchobs. They have different dimensions",
         call. = FALSE)
  }

  catch_diff_tol <- 1e-4
  catch_diff <- d1$catch_obs %>%
    as_tibble() %>%
    add_column(d2$Catchobs) %>%
    mutate(diff = value - `d2$Catchobs`) %>%
    mutate(in_tol = diff < catch_diff_tol)
  if(!all(catch_diff$in_tol)){
    stop("d1$catch_obs not identical to d2$Catchgobs, difference tolerance of ",
         catch_diff_tol, " not upheld",
         call. = FALSE)
  }

  df_identical <- function(wa1, wa2, nm_wa1, nm_wa2){
    if(class(wa1) != "matrix" || class(wa2) != "matrix"){
      stop("Both ", nm_wa1, " and ", nm_wa2, " must be class matrix",
           call. = FALSE)
    }
    if(!identical(dim(wa1), dim(wa2))){
      stop(nm_wa1, " not identical to ", nm_wa2, ". They have different dimensions",
           call. = FALSE)
    }
    if(identical(wa1, wa2)){
      message(nm_wa1, " is identical to ", nm_wa2, "\n")
    }
    wa1 <- as.data.frame(wa1)
    wa2 <- as.data.frame(wa2)
    diff_tol <- 1e-14
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
        mutate(diff = value - `wa2[, .x]`) %>%
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
  df_identical(d1$wage_catch, d2$wage_catch, "d1$wage_catch", "d2$wage_catch")
  df_identical(d1$wage_survey, d2$wage_survey, "d1$wage_survey", "d2$wage_survey")
  df_identical(d1$wage_ssb, d2$wage_ssb, "d1$wage_ssb", "d2$wage_ssb")
  df_identical(d1$wage_mid, d2$wage_mid, "d1$wage_mid", "d2$wage_mid")

  if(!identical(d1$mat_sel, d2$Matsel)){
    stop("d1$mat_sel not identical to d2$Matsel", call. = FALSE)
  }

  if(class(d1$survey) != "numeric" | class(d2$survey) != "numeric"){
    stop("Both d1$survey and d2$survey must be class numeric",
         call. = FALSE)
  }
  survey_diff_tol <- 1e-7
  survey_diff <- d1$survey %>%
    as_tibble() %>%
    add_column(d2$survey) %>%
    mutate(diff = value - `d2$survey`) %>%
    mutate(in_tol = diff < survey_diff_tol)
  if(!all(survey_diff$in_tol)){
    stop("d1$survey not identical to d2$survey, difference tolerance of ",
         survey_diff_tol, " not upheld",
         call. = FALSE)
  }

  if(class(d1$survey_err) != "numeric" | class(d2$survey_err) != "numeric"){
    stop("Both d1$survey_err and d2$survey_err must be class numeric",
         call. = FALSE)
  }
  survey_err_diff_tol <- 1e-5
  survey_err_diff <- d1$survey_err %>%
    as_tibble() %>%
    add_column(d2$survey_err) %>%
    mutate(diff = value - `d2$survey_err`) %>%
    mutate(in_tol = diff < survey_err_diff_tol)
  if(!all(survey_err_diff$in_tol)){
    stop("d1$survey_err not identical to d2$survey_err, difference tolerance of ",
         survey_err_diff_tol, " not upheld",
         call. = FALSE)
  }

  if(!identical(d1$ss_survey, d2$ss_survey)){
    stop("d1$ss_survey not identical to d2$ss_survey", call. = FALSE)
  }
  if(!identical(d1$ss_catch, d2$ss_catch)){
    stop("d1$ss_catch not identical to d2$ss_catch", call. = FALSE)
  }
  if(!identical(d1$a_prior, d2$Aprior)){
    stop("d1$a_prior not identical to d2$Aprior", call. = FALSE)
  }
  if(!identical(d1$b_prior, d2$Bprior)){
    stop("d1$b_prior not identical to d2$Bprior", call. = FALSE)
  }

  df_identical(d1$age_survey, d2$age_survey, "d1$age_survey", "d2$age_survey")
  df_identical(d1$age_catch, d2$age_catch, "d1$age_catch", "d2$age_catch")

  # Parameters ----------------------------------------------------------------
  if(!identical(p1$log_r_init, p2$logRinit)){
    stop("p1$log_r_init not identical to p2$logRinit", call. = FALSE)
  }
  if(!identical(p1$log_m_init, p2$logMinit)){
    if(p2$logMinit - p1$log_m_init > 1e-14){
      stop("p1$log_m_init not identical to p2$logMinit", call. = FALSE)
    }
  }
  if(!identical(p1$log_h, p2$logh)){
    if(p2$logh - p1$log_h > 1e-14){
      stop("p1$log_h not identical to p2$logh", call. = FALSE)
    }
  }
  if(!identical(p1$log_sd_surv, p2$logSDsurv)){
    if(p2$logSDsurv - p1$log_sd_surv > 1e-14){
      stop("p1$log_sd_surv not identical to p2$logSDsurv", call. = FALSE)
    }
  }
  if(!identical(p1$log_phi_catch, p2$logphi_catch)){
    stop("p1$log_phi_catch not identical to p2$logphi_catch", call. = FALSE)
  }
  if(!identical(p1$p_sel_fish, p2$psel_fish)){
    stop("p1$p_sel_fish not identical to p2$psel_fish", call. = FALSE)
  }
  if(!identical(p1$p_sel_surv, p2$psel_surv)){
    stop("p1$p_sel_surv not identical to p2$psel_surv", call. = FALSE)
  }

  df_identical(p1$init_n, p2$initN, "p1$init_n", "p2$initN")

  if(!identical(p1$r_in, p2$Rin)){
    stop("p1$r_in not identical to p2$Rin", call. = FALSE)
  }

  df_identical(p1$p_sel, p2$PSEL, "p1$p_sel", "p2$PSEL")

  if(!identical(p1$f_0, p2$F0)){
    if(any(p2$F0 - p1$f_0 > 1e-14)){
      stop("p1$f_0 not identical to p2$F0", call. = FALSE)
    }
  }
}

#' Round all values in an arbitrarily complex [list]
#'
#' @details This is a recursive function and therefore can have an arbitrary
#' nesting of lists.
#'
#' @param lst A [list] of arbitrary complexity
#' @param digits The number of decimal points to round all numeric values to
#'
#' @return A [list] in the same format as `lst` but with all values rounded to
#' `digits` decimal points
#' @export
round_list <- function(lst, digits = 2){
  if(is.null(lst)){
    return(NULL)
  }
  if(!length(lst)){
    return(NULL)
  }
  if(class(lst) != "list"){
    # At this point lst is a single non-list object (data frame, matrix, vector, etc)
    if(class(lst) == "data.frame"){
      return(round_data_frame(lst, digits))
    }else if(class(lst) == "matrix"){
      return(as.matrix(round_data_frame(as.data.frame(lst))))
    }else if(class(lst) == "array"){
      n_arr_dims <- length(dim(lst))
      if(n_arr_dims == 3){
        return(round_3d_array(lst, digits))
      }else if(n_arr_dims == 4){
        return(round_4d_array(lst, digits))
      }else if(n_arr_dims == 5){
        return(round_5d_array(lst, digits))
      }else{
        stop("Arrays greater than 5 dimensions are not implemented",
             call. = FALSE)
      }
    }else{
      return(round(lst, digits))
    }
  }

  # At this point lst is guaranteed to be a list of one or greater
  nms <- names(lst)
  out_first <- round_list(lst[[1]], digits)
  out_therest <- round_list(lst[-1], digits)
  if(class(out_therest) == "list"){
    out <- c(list(out_first), out_therest)
  }else{
    out <- list(out_first, out_therest)
  }
  names(out) <- nms
  out[sapply(out, is.null)] <- NULL
  out
}

#' Round all numeric values found in a [data.frame] to a specified number of decimal points
#'
#' @details Columns which are not numeric will be returned unmodified
#' @param df A [data.frame]
#' @param digits The number of decimal points to round all numeric values to
#'
#' @return A [data.frame] identical to the input `df` but with all numerical values
#' rounded
#' @importFrom purrr map_df
#' @export
round_data_frame <- function(df, digits = 2){
  map_df(df,~{
    tryCatch(round(.x, digits),
             error = function(e) .x)

  })
}

#' Round all numeric values found in a multidimensional [array]
#' to a specified number of decimal points
#'
#' @param arr The array
#' @param digits  The number of decimal points to round all numeric values to
#'
#' @return An array identical to the input `arr` but with all numerical values rounded
#' @export
round_3d_array <- function(arr, digits = 2){
  dims <- dim(arr)
  if(length(dims) != 3){
    stop("Not a 3D array",
         call. = FALSE)
  }
  new_arr <- array(NA, dim = dims, dimnames = dimnames(arr))
  for(i in seq_len(dims[1])){
    new_arr[i, , ] <- as.matrix(round_data_frame(as.data.frame(arr[i, , ]), digits = digits))
  }
  new_arr
}

#' @rdname round_3d_array
#' @export
round_4d_array <- function(arr, digits = 2){
  dims <- dim(arr)
  if(length(dims) != 4){
    stop("Not a 4D array",
         call. = FALSE)
  }
  new_arr <- array(NA, dim = dims, dimnames = dimnames(arr))
  for(i in seq_len(dims[1])){
    for(j in seq_len(dims[2])){
      new_arr[i, j, , ] <- as.matrix(round_data_frame(as.data.frame(arr[i, j, , ]), digits = digits))
    }
  }
  new_arr
}

#' @rdname round_3d_array
#' @export
round_5d_array <- function(arr, digits = 2){
  dims <- dim(arr)
  if(length(dims) != 5){
    stop("Not a 5D array",
         call. = FALSE)
  }
  new_arr <- array(NA, dim = dims, dimnames = dimnames(arr))
  for(i in seq_len(dims[1])){
    for(j in seq_len(dims[2])){
      for(k in seq_len(dims[3])){
        new_arr[i, j, k, , ] <- as.matrix(round_data_frame(as.data.frame(arr[i, j, k, , ]), digits = digits))
      }
    }
  }
  new_arr
}