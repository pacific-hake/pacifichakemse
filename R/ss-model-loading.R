#' Create an rds file to hold an SS model's data and outputs.
#'
#' @param model_dir Directory name of model to be loaded
#'
#' @return [base::invisible()]
#' @export
create_rds_file <- function(model_dir = NULL){

  verify_argument(model_dir, "character", 1)

  if(!dir.exists(model_dir)){
    stop("Error - the directory ", model_dir, " does not exist.\n",
         "Fix the problem and try again.", call. = FALSE)
  }

  # The RDS file will have the same name as the directory it is in
  rds_file <- file.path(model_dir, paste0(basename(model_dir), ".rds"))
  if(file.exists(rds_file)){
    unlink(rds_file, force = TRUE)
  }

  message("Creating a new RDS file in ", model_dir, "\n")

  # If this point is reached, no RDS file exists so it has to be built from scratch
  model <- load_ss_files(model_dir)

  saveRDS(model, file = rds_file)
  invisible()
}

#' Load models from files created using [create_rds_file()]
#'
#' @details Load an SS3 model from an RDS file and return as a [list]
#'
#' @param model_dir A [vector] of model directory names
#'
#' @return A [list] of model inputs/outputs for the requested model
#' @export
load_ss_model_from_rds <- function(model_dir = NULL){

  verify_argument(model_dir, "character", 1)

  model_rds_file <- file.path(model_dir, paste0(basename(model_dir), ".rds"))
  if(!file.exists(model_rds_file)){
    create_rds_file(model_dir)
  }
  readRDS(model_rds_file)
}

#' Load all the SS files for output and input, and return the model object
#'
#' @param model_dir Directory the model resides in
#' @param printstats Print info on each model loaded via [r4ss::SS_output()]
#'
#' @return A model object representing the output from the SS model
#' @importFrom r4ss SS_output SS_readdat
#' @export
load_ss_files <- function(model_dir = NULL,
                          printstats = FALSE){

  verify_argument(model_dir, "character", 1)
  verify_argument(printstats, "logical", 1)

  # Load MPD results
  model <- tryCatch({
    SS_output(dir = model_dir,
              verbose = FALSE,
              printstats = printstats,
              covar = FALSE)
  }, error = function(e){
    SS_output(dir = model_dir,
              verbose = FALSE,
              printstats = printstats,
              covar = FALSE,
              forecast = FALSE)
  })

  ## Load the data file and control file for the model
  ## Get the file whose name contains "_data.ss" and "_control.ss"
  ## If there is not exactly one of each, stop with error.
  model_dir_listing <- dir(model_dir)
  dat_fn_ind <- grep("_data.ss", model_dir_listing)
  ctl_fn_ind <- grep("_control.ss", model_dir_listing)
  par_fn_ind <- grep("ss.par", model_dir_listing)
  if(!length(dat_fn_ind)){
    stop("Error in model ", model_dir,
         ", there is no data file. A data file is any file whose name contains the text _data.ss.\n\n",
         call. = FALSE)
  }
  if(length(dat_fn_ind) > 1){
    stop("Error in model ", model_dir,
         ", there is more than one data file. A data file is any file whose name contains the text _data.ss.\n\n",
         call. = FALSE)
  }
  if(!length(ctl_fn_ind)){
    stop("Error in model ", model_dir,
         ", there is no control file. A control file is any file whose name contains the text _control.ss.\n\n",
         call. = FALSE)
  }
  if(length(ctl_fn_ind) > 1){
    stop("Error in model ", model_dir,
         ", there is more than one control file. A control file is any file whose name contains the text _control.ss.\n\n",
         call. = FALSE)
  }
  dat_fn <- file.path(model_dir, model_dir_listing[dat_fn_ind])
  ctl_fn <- file.path(model_dir, model_dir_listing[ctl_fn_ind])
  par_fn <- file.path(model_dir, model_dir_listing[par_fn_ind])
  model$path <- model_dir
  model$dat_file <- dat_fn
  model$dat <- SS_readdat(dat_fn, verbose = FALSE)
  model$ctl_file <- ctl_fn
  model$ctl <- readLines(ctl_fn)
  model$par_file <- par_fn
  model$par <- readLines(par_fn)

  model
}

#' Load the SS model input and output data needed by this package in correct format
#'
#' @param ss_model SS3 model output as created by [create_rds_file()]
#' @param ... Arguments to be passed to [load_ss_sel_parameters()]
#'
#' @return A [list] with objects to be used throughout the package code
load_ss_model_data <- function(ss_model,
                               ...){

  verify_argument(ss_model, "list")
  lst <- NULL

  lst$parms_scalar <- load_ss_parameters(ss_model)
  lst$parms_sel <- load_ss_sel_parameters(ss_model,
                                          ...)
  lst$age_survey_df <- extract_age_comps(ss_model,
                                         age_comps_fleet = 2,
                                         ...)
  lst$age_catch_df <- extract_age_comps(ss_model,
                                        age_comps_fleet = 1,
                                        ...)
  browser()
  lst
}

#' Extract the age comps from the SS assessment model output into a format
#' required for input into the TMB assessment model
#'
#' @details Proportions at age for each year are calculated and returned
#'
#' @param ss_model SS model input/output as read in by [load_ss_model_from_rds()]
#' @param age_comps_fleet 1 for fishery, 2 for survey
#' @param s_yr See [load_data_seasons()]
#' @param m_yr See [load_data_seasons()]
#' @param age_comps_fill Value to replace NAs in the table, it can also be NA, but not NULL
#' @param yr_col The name of the column in `ss_model$dat$agecomp` that contains
#' the year
#' @param age_comps_fleet_col The name of the column in `ss_model$dat$agecomp` that contains
#' the fleet code
#' @param ... Arguments absorbed which are meant for other functions
#'
#' @return The matrix representing all years from `s_yr` to `m_yr` with data included
#' for years which are found in the `ss_model` data. Years not in the `ss_model` data
#' will be filled with the value of `age_comps_fill`
#' @importFrom tidyselect matches
#' @importFrom tidyr complete
#' @export
extract_age_comps <- function(ss_model = NULL,
                              age_comps_fleet = 1,
                              s_yr = NULL,
                              m_yr = NULL,
                              age_comps_fill = NA,
                              yr_col = "Yr",
                              age_comps_fleet_col = "FltSvy",
                              ...){

  verify_argument(ss_model, "list")
  verify_argument(age_comps_fleet, "numeric", 1)
  stopifnot(age_comps_fleet %in% c(1, 2))
  verify_argument(s_yr, "numeric", 1)
  verify_argument(m_yr, "numeric", 1)
  if(is.na(age_comps_fill)){
    age_comps_fill <- NA_real_
  }
  verify_argument(age_comps_fill, "numeric", 1)
  verify_argument(yr_col, "character", 1)
  verify_argument(age_comps_fleet_col, "character", 1)

  age_comp_data <- ss_model$dat$agecomp
  if(!yr_col %in% names(age_comp_data)){
    stop("The column `", yr_col, "` does not exist in the SS age comp data table.",
         call. = FALSE)
  }
  if(!age_comps_fleet_col %in% names(age_comp_data)){
    stop("The column `", age_comps_fleet_col, "` does not exist in the SS age comp data table.",
         call. = FALSE)
  }
  age_comps <- age_comp_data %>%
    filter(!!sym(age_comps_fleet_col) == age_comps_fleet)
  if(nrow(age_comps) == 0){
    stop("The fleet number `", age_comps_fleet, "` was not found in the SS age comp data table. ",
         call. = FALSE)
  }
  age_comps_yrs <- age_comps %>% select(!!sym(yr_col)) %>% pull()
  age_comps <- age_comps %>%
    select(matches("^a\\d+$")) %>%
    mutate(row_sum = rowSums(.)) %>%
    mutate_at(.vars = vars(-row_sum), .funs = list(~ . / row_sum)) %>%
    select(-row_sum) %>%
    mutate(yr = age_comps_yrs) %>%
    select(yr, everything()) %>%
    complete(yr = seq(s_yr, m_yr)) %>%
    replace(is.na(.), age_comps_fill) %>%
    t()
  colnames(age_comps) <- age_comps[1,]
  age_comps <- age_comps[-1,]
  age_comps
}

#' Load a list of parameter estimates from the SS model output
#'
#' @param ss_model SS3 model output as created by [create_rds_file()]
#'
#' @return A named [list] of parameter estimates
#' @importFrom tibble as_tibble
#' @export
load_ss_parameters <- function(ss_model = NULL){

  verify_argument(ss_model, "list")
  parm_tbl <- ss_model$parameters %>% as_tibble()
  if(!"Value" %in% names(parm_tbl)){
    stop("The column `Value` does not exist in the SS output parameter table.",
         call. = FALSE)
  }
  if(!length(grepl("^SR_LN", parm_tbl$Label))){
    stop("The `log_r_init` parameter was not found in the SS model output. ",
         "The regular expression on the parameter label is `^SR_LN`.",
         call. = FALSE)
  }

  if(!length(grepl("^SR_BH", parm_tbl$Label))){
    stop("The `log_h` parameter was not found in the SS model output. ",
         "The regular expression on the parameter label is `^SR_BH`.",
         call. = FALSE)
  }

  if(!length(grepl("^NatM", parm_tbl$Label))){
    stop("The `log_m_init` parameter was not found in the SS model output. ",
         "The regular expression on the parameter label is `^NatM`.",
         call. = FALSE)
  }

  if(!length(grepl("^NatM", parm_tbl$Label))){
    stop("The `log_sd_surv` parameter was not found in the SS model output. ",
         "The regular expression on the parameter label is `^Q_extraSD_Acoustic_Survey`.",
         call. = FALSE)
  }

  if(!length(grepl("^SR_sigmaR", parm_tbl$Label))){
    stop("The `log_sd_r` parameter was not found in the SS model output. ",
         "The regular expression on the parameter label is `^SR_sigmaR`.",
         call. = FALSE)
  }

  if(!length(grepl("^ln\\(EffN_mult\\)_1$", parm_tbl$Label))){
    stop("The `log_phi_catch` parameter was not found in the SS model output. ",
         "The regular expression on the parameter label is `^ln\\(EffN_mult\\)_1$`.",
         call. = FALSE)
  }

  lst <- NULL

  lst$log_r_init <- parm_tbl %>% filter(grepl("^SR_LN", Label)) %>%
    pull(Value)
  lst$log_h <- parm_tbl %>% filter(grepl("^SR_BH", Label)) %>%
    pull(Value) %>% log()
  lst$log_m_init <- parm_tbl %>% filter(grepl("^NatM", Label)) %>%
    pull(Value) %>% log()
  lst$log_sd_surv <- parm_tbl %>% filter(grepl("^Q_extraSD_Acoustic_Survey", Label)) %>%
    pull(Value) %>% log()
  lst$log_sd_r <- parm_tbl %>% filter(grepl("^SR_sigmaR", Label)) %>%
    pull(Value) %>% log()
  lst$log_phi_catch <- parm_tbl %>% filter(grepl("^ln\\(EffN_mult\\)_1$", Label)) %>%
    pull(Value)

  lst
}

#' Load a selectivity parameter estimates from the SS model output
#'
#' @param ss_model SS3 model output as created by [create_rds_file()]
#' @param s_min Minimum age in fishery selectivity
#' @param s_max Maximum age in fishery selectivity
#' @param s_min_survey Minimum age in survey selectivity
#' @param s_max_survey Maximum age in survey selectivity
#' @param ... Arguments absorbed which are meant for other functions
#'
#' @return A [data.frame] of estimates, age, and source (fishery and survey)
#' @export
load_ss_sel_parameters <- function(ss_model = NULL,
                                   s_min = NULL,
                                   s_max = NULL,
                                   s_min_survey = NULL,
                                   s_max_survey = NULL,
                                   ...){

  verify_argument(ss_model, "list")
  verify_argument(s_min, c("integer", "numeric"), 1)
  verify_argument(s_max, c("integer", "numeric"), 1)
  verify_argument(s_min_survey, c("integer", "numeric"), 1)
  verify_argument(s_max_survey, c("integer", "numeric"), 1)
  if(s_max < s_min){
    stop("`s_max` must be greater or equal to `s_min`",
         call. = FALSE)
  }
  if(s_max_survey < s_min_survey){
    stop("`s_max_survey` must be greater or equal to `s_min_survey`",
         call. = FALSE)
  }
  fish_ages <- s_min:s_max
  survey_ages <- s_min_survey:s_max_survey

  parm_tbl <- ss_model$parameters %>% as_tibble()
  if(!"Value" %in% names(parm_tbl)){
    stop("The column `Value` does not exist in the SS output parameter table.",
         call. = FALSE)
  }
  if(!length(grepl("^SR_LN", parm_tbl$Label))){
    stop("The `log_r_init` parameter was not found in the SS model output. ",
         "The regular expression on the parameter label is `^SR_LN`.",
         call. = FALSE)
  }
  # assume the parameters start at 0
  fish <- parm_tbl %>%
    filter(grepl("^AgeSel_.*_Fishery", Label))
  fish <- fish[-(1:min(fish_ages)),]
  fish <- fish[-(max(fish_ages):nrow(fish)),]
  fish <- fish %>%
    transmute(value = Value) %>%
    mutate(source = "fish") %>%
    mutate(age = fish_ages)

  survey <- parm_tbl %>%
    filter(grepl("^AgeSel_.*_Acoustic_Survey", Label))
  survey <- survey[-(1:min(survey_ages)),]
  survey <- survey[-(max(survey_ages):nrow(survey)),]
  survey <- survey %>%
    transmute(value = Value) %>%
    mutate(source = "survey") %>%
    mutate(age = survey_ages)

  bind_rows(fish, survey)
}