#' Load all the SS files for output and input, and return the model object
#'
#' @param ss_model_output_dir Directory the model resides in
#' @param printstats Print info on each model loaded via [r4ss::SS_output()]
#'
#' @return A model object representing the output from the SS model
#' @importFrom r4ss SS_output SS_readdat
#' @export
load_ss_files <- function(ss_model_output_dir = NULL,
                          printstats = FALSE){

  # Load MPD results
  model <- tryCatch({
    SS_output(dir = ss_model_output_dir,
              verbose = FALSE,
              printstats = printstats,
              covar = FALSE)
  }, error = function(e){
    SS_output(dir = ss_model_output_dir,
              verbose = FALSE,
              printstats = printstats,
              covar = FALSE,
              forecast = FALSE)
  })

  model$report <- readLines(file.path(ss_model_output_dir, "Report.sso"))

  ## Load the data file and control file for the model
  ## Get the file whose name contains "_data.ss" and "_control.ss"
  ## If there is not exactly one of each, stop with error.
  model_dir_listing <- dir(ss_model_output_dir)
  dat_fn_ind <- grep("_data.ss", model_dir_listing)
  ctl_fn_ind <- grep("_control.ss", model_dir_listing)
  par_fn_ind <- grep("ss.par", model_dir_listing)
  if(!length(dat_fn_ind)){
    stop("Error in model ", ss_model_output_dir,
         ", there is no data file. A data file is any file whose name contains the text _data.ss.\n\n",
         call. = FALSE)
  }
  if(length(dat_fn_ind) > 1){
    stop("Error in model ", ss_model_output_dir,
         ", there is more than one data file. A data file is any file whose name contains the text _data.ss.\n\n",
         call. = FALSE)
  }
  if(!length(ctl_fn_ind)){
    stop("Error in model ", ss_model_output_dir,
         ", there is no control file. A control file is any file whose name contains the text _control.ss.\n\n",
         call. = FALSE)
  }
  if(length(ctl_fn_ind) > 1){
    stop("Error in model ", ss_model_output_dir,
         ", there is more than one control file. A control file is any file whose name contains the text _control.ss.\n\n",
         call. = FALSE)
  }
  dat_fn <- file.path(ss_model_output_dir, model_dir_listing[dat_fn_ind])
  ctl_fn <- file.path(ss_model_output_dir, model_dir_listing[ctl_fn_ind])
  par_fn <- file.path(ss_model_output_dir, model_dir_listing[par_fn_ind])
  model$path <- ss_model_output_dir
  model$dat_file <- dat_fn
  model$dat <- SS_readdat(dat_fn, verbose = FALSE)
  model$ctl_file <- ctl_fn
  model$ctl <- readLines(ctl_fn)
  if(length(par_fn)){
    model$par_file <- par_fn
    model$par <- readLines(par_fn)
  }

  model
}

#' Load the SS model input and output data needed by this package in correct format
#'
#' @param s_min Minimum age in fishery selectivity
#' @param s_max Maximum age in fishery selectivity
#' @param s_min_survey Minimum age in survey selectivity
#' @param s_max_survey Maximum age in survey selectivity
#' @param weight_factor A factor to multiply and divide SSB values by
#' @param n_space The number of spaces (areas) in which fishing takes place
#' and to which a selectivity-at-age is to be set. See the value of
#' `p_sel_fish` in the returned list
#' @param selex_fill_val The selectivity value to fill in for missing
#' selectivity estimates, e.g. Used to fill in Canadian selectivity because the
#' estimation model only estimates one selectivity which is used for the USA
#' @param ... Arguments to be passed to [load_ss_sel_parameters()]
#'
#' @return A [list] with objects to be used throughout the package code
#' @export
#' @importFrom dplyr distinct
#' @importFrom reshape2 dcast
load_ss_model_data <- function(s_min = 1,
                               s_max = 6,
                               s_min_survey = 2,
                               s_max_survey = 6,
                               weight_factor = 1000,
                               n_space = 2,
                               selex_fill_val = 1,
                               ss_model_output_dir,
                               ...){

  rds_file <- file.path(ss_model_output_dir,
                        paste0(basename(ss_model_output_dir), ".rds"))
  # The RDS file will have the same name as the directory it is in
  if(!file.exists(rds_file)){
    stop("The RDS file does not exist.\n",
         rds_file)
  }

  ss_model <- readRDS(rds_file)
  ss_model$catch_seas_country <- extract_catch_country(...)
browser()
  lst <- NULL

  # Catch observations --------------------------------------------------------
  lst$catch_obs <- ss_model$dat$catch |>
    transmute(yr = year,
              value = catch) |>
    filter(yr > 1900) |>
    as.matrix()

  lst$s_yr <- min(lst$catch_obs[, "yr"])
  lst$m_yr <- max(lst$catch_obs[, "yr"])
  yrs <- lst$s_yr:lst$m_yr


  lst$catch_props_space_season <- ss_model$catch_seas_country
  lst$catch_country <- ss_model$catch_country

  # Weight-at-age data --------------------------------------------------------
  waa <- ss_model$wtatage[!names(ss_model$wtatage) %in% c("Seas", "Sex", "Bio_Pattern", "BirthSeas")]
  lst$wage_catch_df <- waa[waa[["Fleet"]] == 1,]
  lst$wage_catch_df <- lst$wage_catch_df[names(lst$wage_catch_df) != "Fleet"]
  lst$wage_catch_df <- lst$wage_catch_df[lst$wage_catch_df[["Yr"]] <= lst$m_yr,]

  lst$wage_survey_df <- waa[waa[["Fleet"]] == 2,]
  lst$wage_survey_df <- lst$wage_survey_df[names(lst$wage_survey_df) != "Fleet"]
  lst$wage_survey_df <- lst$wage_survey_df[lst$wage_survey_df[["Yr"]] <= lst$m_yr,]

  lst$wage_mid_df <- waa[waa[["Fleet"]] == -1,]
  lst$wage_mid_df <- lst$wage_mid_df[names(lst$wage_mid_df) != "Fleet"]
  lst$wage_mid_df <- lst$wage_mid_df[lst$wage_mid_df[["Yr"]] <= lst$m_yr,]

  lst$wage_ssb_df <- waa[waa[["Fleet"]] == -2,]
  lst$wage_ssb_df <- lst$wage_ssb_df[names(lst$wage_ssb_df) != "Fleet"]
  lst$wage_ssb_df <- lst$wage_ssb_df[lst$wage_ssb_df[["Yr"]] <= lst$m_yr,]

  # Maturity from first year only ---------------------------------------------
  lst$mat_sel <- as.matrix(lst$wage_ssb[1,])

  lst$parms_scalar <- load_ss_parameters(ss_model)
  lst$age_survey <- extract_age_comps(ss_model,
                                      age_comps_fleet = 2,
                                      s_yr = lst$s_yr,
                                      m_yr = lst$m_yr,
                                      ...)
  lst$age_catch <- extract_age_comps(ss_model,
                                     age_comps_fleet = 1,
                                     s_yr = lst$s_yr,
                                     m_yr = lst$m_yr,
                                     ...)

  # Selectivity estimates -----------------------------------------------------
  # row names are the ages
  sels <- ss_model$parameters %>%
    filter(grepl("DEVadd", Label))
  sels_lab <- sels$Label
  sels_yrs <- as.numeric(str_extract(sels_lab, "[0-9]+$"))
  sels_ages <- as.numeric(str_extract(sels_lab, "[0-9]+")) - 2
  sels_vals <- sels$Value
  lst$sel_by_yrs <- tibble(yr = sels_yrs,
                           age = sels_ages,
                           val = sels_vals) %>%
    dcast(age ~ yr, value.var = "val")
  rownames(lst$sel_by_yrs) <- lst$sel_by_yrs$age
  lst$sel_by_yrs <- lst$sel_by_yrs %>%
    select(-age)

  # Selectivity parameter estimates -------------------------------------------
  sel_param_ests <- load_ss_sel_parameters(ss_model,
                                           s_min = s_min,
                                           s_max = s_max,
                                           s_min_survey = s_min_survey,
                                           s_max_survey = s_max_survey,
                                           selex_fill_val = selex_fill_val,
                                           ...)

  lst$p_sel_fish <- sel_param_ests[sel_param_ests[, "source"] == 1, ]
  lst$p_sel_surv <- sel_param_ests[sel_param_ests[, "source"] == 2, ]

  # Add more selectivities by space (area).
  if(n_space == 1){
    spc <- rep(1, nrow(lst$p_sel_fish))
    lst$p_sel_fish <- cbind(lst$p_sel_fish, spc)
  }else{
    spc <- rep(2, nrow(lst$p_sel_fish))
    lst$p_sel_fish <- cbind(lst$p_sel_fish[, 1:2], spc, lst$p_sel_fish[, 3])
    colnames(lst$p_sel_fish)[3:4] <- c("space", "age")
    spc1_mat <- matrix(rep(selex_fill_val, length(s_min:(s_max - 1))), ncol = 1)
    spc1_mat <- cbind(spc1_mat, spc1_mat, spc1_mat, s_min:(s_max - 1))
    lst$p_sel_fish <- rbind(spc1_mat, lst$p_sel_fish)
  }

  if(nrow(lst$age_survey) != nrow(lst$age_catch)){
    stop("There was a problem loading the estimates of survey and catch age proportions ",
         "from the SS model. The number of ages do not match. ",
         "There are ", nrow(lst$age_survey), " ages in the survey output and ",
         nrow(lst$age_catch), " ages in the catch output.",
         call. = FALSE)
  }
  # Survey index and error (log SD) -------------------------------------------
  surv <- ss_model$dat$CPUE
  surv <- surv %>%
    filter(index == 2) %>%
    transmute(yr = year, value = obs, err = se_log) %>%
    complete(yr = seq(lst$s_yr, lst$m_yr)) %>%
    replace(is.na(.), 1) %>%
    as.matrix()

  lst$survey <- surv[, "value"]
  lst$survey_err <- surv[, "err"]

  # Sample sizes for fishery and survey ---------------------------------------
  ss <- ss_model$agedbase %>%
    transmute(yr = Yr, fleet = Fleet, ss = Nsamp_adj) %>%
    distinct()
  ss_catch <- ss %>%
    filter(fleet == 1) %>%
    select(-fleet) %>%
    mutate(flag = 1) %>%
    complete(yr = seq(lst$s_yr, lst$m_yr)) %>%
    replace(is.na(.), 0) %>%
    mutate(flag = ifelse(flag == 0, -1, flag)) %>%
    as.matrix()
  lst$ss_catch <- ss_catch[, "ss"]
  lst$flag_catch <- ss_catch[, "flag"]
  ss_survey <- ss %>%
    filter(fleet == 2) %>%
    select(-fleet) %>%
    mutate(flag = 1) %>%
    complete(yr = seq(lst$s_yr, lst$m_yr)) %>%
    replace(is.na(.), 0) %>%
    mutate(flag = ifelse(flag == 0, -1, flag)) %>%
    as.matrix()
  lst$ss_survey <- ss_survey[, "ss"]
  lst$flag_survey <- ss_survey[, "flag"]

  # Median population estimates -----------------------------------------------
  # The following is shown in a table in the assessment doc and made by the
  # make.median.posterior.table() function in the hake-assessment repository
  mc <- ss_model$mcmccalcs
  if(is.null(mc)){
    stop("You must have a valid MCMC folder in the SS model directory to  load from",
         call. = FALSE)
  }
  vals_mc <- mc[c("smed", "dmed", "rmed", "pmed", "fmed")]
  vals_mc <- map(vals_mc, ~{
    .x[names(.x) %in% yrs]
  })

  if(is.null(ss_model$extra_mcmc)){
    lst$med_popests <-  tibble(f_ssb = vals_mc$smed * weight_factor,
                               rel_ssb = vals_mc$dmed,
                               r = vals_mc$rmed * weight_factor,
                               spr_f = vals_mc$pmed,
                               e = vals_mc$fmed)
  }else{
    extra_mc <- ss_model$extra_mcmc$timeseries
    b <- extra_mc$Bio_all[extra_mc$Yr %in% yrs] / weight_factor
    lst$med_popests <-  tibble(f_ssb = vals_mc$smed * weight_factor,
                               rel_ssb = vals_mc$dmed,
                               b = b,
                               r = vals_mc$rmed * weight_factor,
                               spr_f = vals_mc$pmed,
                               e = vals_mc$fmed)
  }

  lst$med_popests <- lst$med_popests %>%
    mutate(yr = yrs) %>%
    select(yr, everything())
  # Make Relative fishing intensity and Exploitation fraction `NA` for the last year
  # because the catch for that year has not occurred yet and the values output
  # by the model are meaningless\
  nrow_p <- nrow(lst$med_popests)
  ncol_p <- ncol(lst$med_popests)
  lst$med_popests[nrow_p, ncol_p - 1] <- NA
  lst$med_popests[nrow_p, ncol_p] <- NA

  # Recruitment deviations ----------------------------------------------------
  rownames(ss_model$recruitpars) <- NULL
  recr_rows <- grepl("Early_RecrDev|Main_RecrDev", ss_model$recruitpars[, "type"])
  lst$r_dev <- ss_model$recruitpars[recr_rows, ]
  lst$r_dev <- lst$r_dev[, colnames(lst$r_dev) %in% c("Yr", "Value")]
  lst$r_dev <- as.matrix(lst$r_dev[, c("Yr", "Value")])
  colnames(lst$r_dev) <- c("yr", "value")

  # Bias ramp adjustment ------------------------------------------------------
  ctl <- ss_model$ctl
  b_breakpoints <- ss_model$breakpoints_for_bias_adjustment_ramp[1,] %>% as.numeric
  yb_1 <- b_breakpoints[1]
  yb_2 <- b_breakpoints[2]
  yb_3 <- b_breakpoints[3]
  yb_4 <- b_breakpoints[4]
  b_max <- b_breakpoints[5]
  b_yrs <- lst$s_yr:lst$m_yr
  lst$b <- NULL
  for(j in 1:length(b_yrs)){
    if(b_yrs[j] <= yb_1){
      lst$b[j] <- 0
    }
    if(b_yrs[j] > yb_1 && b_yrs[j]< yb_2){
      lst$b[j] <- b_max * ((b_yrs[j] - yb_1) / (yb_2 - yb_1))
    }
    if(b_yrs[j] >= yb_2 && b_yrs[j] <= yb_3){
      lst$b[j] <- b_max
    }
    if(b_yrs[j] > yb_3 && b_yrs[j] < yb_4){
      lst$b[j] <- b_max * (1 - (yb_3 - b_yrs[j]) / (yb_4 - yb_3))
    }
    if(b_yrs[j] >= yb_4){
      lst$b[j] <- 0
    }
  }

  # Initial numbers-at-age ---------------------------------------------------
  init_n <- ss_model$report[grep("Early_InitAge", ss_model$report)]
  lst$init_n <- gsub("^.*Early_InitAge_[0-9]+ ([-]?[0-9\\.]+) .*$", "\\1", init_n) %>%
    rev %>%
    as.numeric

  lst$ctl_file <- ss_model$ctl_file
  lst$ctl <- ss_model$ctl
  lst$dat_file <- ss_model$dat_file
  lst$dat <- ss_model$dat

  cat(green(symbol$tick),
      green("Loaded SS data successfully\n"))

  lst
}

#' Extract the age comp estimates from the SS assessment model output into a format
#' required for input into the TMB assessment model
#'
#' @details Proportions at age for each year are calculated and returned
#'
#' @param ss_model SS model input/output as read in by [load_ss_model_data()]
#' @param age_comps_fleet 1 for fishery, 2 for survey
#' @param s_yr See [load_data_om()]
#' @param m_yr See [load_data_om()]
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
#' @importFrom dplyr bind_cols
#' @export
extract_age_comps <- function(ss_model = NULL,
                              age_comps_fleet = 1,
                              s_yr = NULL,
                              m_yr = NULL,
                              age_comps_fill = -1,
                              yr_col = "Yr",
                              age_comps_fleet_col = "Fleet",
                              ...){

  stopifnot(age_comps_fleet %in% c(1, 2))
  if(is.na(age_comps_fill)){
    age_comps_fill <- NA_real_
  }

  age_comp_data <- ss_model$agedbase |>
    as_tibble()
  ages <- age_comp_data |>
    filter(Fleet == age_comps_fleet) |>
    pull(Bin) |>
    unique()

  if(!yr_col %in% names(age_comp_data)){
    stop("The column `", yr_col, "` does not exist in the SS age ",
         "comp data table.")
  }
  if(!age_comps_fleet_col %in% names(age_comp_data)){
    stop("The column `", age_comps_fleet_col, "` does not exist in the SS ",
         "age comp data table.")
  }
  age_comps <- age_comp_data |>
    filter(!!sym(age_comps_fleet_col) == age_comps_fleet)
  if(nrow(age_comps) == 0){
    stop("The fleet number `", age_comps_fleet, "` was not found in the SS ",
         "age comp data table. ")
  }

  # Reformat the age estimates into a table with ages as rows and years as columns
  age_comps <- age_comps %>%
    select(!!sym(yr_col), Obs) %>%
    group_by(!!sym(yr_col)) %>%
    group_nest()
  age_comps_yrs <- age_comps[[yr_col]]
  age_comps <- age_comps %>% select(-!!sym(yr_col))
  age_comps <- map(age_comps$data, ~{
    tibble(.x)
  })
  age_comps <- bind_cols(age_comps, .name_repair = "minimal")
  age_comps <- age_comps %>%
    t() %>%
    as_tibble(.name_repair = "minimal") %>%
    set_names(ages)

  # Fill in missing years with `age_comps_fill` value and return a matrix type
  age_comps <- age_comps %>%
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

#' Extract the age comp input data from the SS assessment model output into a format
#' required for input into the TMB assessment model
#'
#' @details Proportions at age for each year are calculated and returned
#'
#' @param ss_model SS model input/output as read in by [load_ss_model_data()]
#' @param age_comps_fleet 1 for fishery, 2 for survey
#' @param s_yr See [load_data_om()]
#' @param m_yr See [load_data_om()]
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
extract_age_comp_data <- function(ss_model = NULL,
                                  age_comps_fleet = 1,
                                  s_yr = NULL,
                                  m_yr = NULL,
                                  age_comps_fill = -1,
                                  yr_col = "Yr",
                                  age_comps_fleet_col = "FltSvy",
                                  ...){

  stopifnot(age_comps_fleet %in% c(1, 2))
  if(is.na(age_comps_fill)){
    age_comps_fill <- NA_real_
  }

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

  parm_tbl <- ss_model$parameters %>%
    as_tibble()
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

  lst$log_r_init <- parm_tbl %>%
    filter(grepl("^SR_LN", Label)) %>%
    pull(Value)
  lst$log_h <- parm_tbl %>%
    filter(grepl("^SR_BH", Label)) %>%
    pull(Value) %>%
    log()
  lst$log_m_init <- parm_tbl %>%
    filter(grepl("^NatM", Label)) %>%
    pull(Value) %>%
    log()
  lst$log_sd_surv <- parm_tbl %>%
    filter(grepl("^Q_extraSD_Acoustic_Survey", Label)) %>%
    pull(Value) %>%
    log()
  lst$log_sd_r <- parm_tbl %>%
    filter(grepl("^SR_sigmaR", Label)) %>%
    pull(Value) %>%
    log()
  lst$log_phi_catch <- parm_tbl %>%
    filter(grepl("^ln\\(EffN_mult\\)_1$", Label)) %>%
    pull(Value)
  lst$log_phi_survey <- parm_tbl %>%
    filter(grepl("^ln\\(EffN_mult\\)_2$", Label)) %>%
    pull(Value)

  # TODO: Remove these hardwired values. They are set up to match what was in the
  # old code for the 2018 SS model and are not correct
  lst$log_h <- -0.145394626195752
  lst$log_m_init <- -1.54379999585323
  lst$log_sd_surv <- -1.34655070780058

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
#' @return A [matrix] of estimates, age, and source (fishery and survey coded as 1 and 2 respectively)
#' @importFrom dplyr add_row
#' @export
load_ss_sel_parameters <- function(ss_model = NULL,
                                   s_min = NULL,
                                   s_max = NULL,
                                   s_min_survey = NULL,
                                   s_max_survey = NULL,
                                   ...){

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
         "Debug the load_ss_sel_parameters() function.",
         call. = FALSE)
  }

  # Assume the parameters start at 0, and that the "Used" field signifies unused parameters if negative
  fish <- parm_tbl %>%
    filter(grepl("^AgeSel_.*_Fishery", Label)) %>%
    filter(Used >= 0)
  fish <- fish[-(1:min(fish_ages)),]
  fish <- fish[-(max(fish_ages):nrow(fish)),]
  if(nrow(fish) != length(fish_ages) - 1){
    stop("The SS model does not have selectivity values matching the ages you entered (", s_min, "-", s_max, "). ",
         "Debug the `fish` object in load_ss_sel_parameters() to see why. This could be due to parameter",
         "names changing in SS which are matched by a regular expression.",
         call. = FALSE)
  }
  fish <- fish %>%
    transmute(value = Value) %>%
    mutate(source = 1) %>%
    mutate(age = fish_ages[-length(fish_ages)])

  survey <- parm_tbl %>%
    filter(grepl("^AgeSel_.*_Acoustic_Survey", Label)) %>%
    filter(Used >= 0)
  survey <- survey[-(1:(min(survey_ages) - 1)),]
  survey <- survey[-((max(survey_ages) - 1):nrow(survey)),]
  if(nrow(survey) != length(survey_ages) - 1){
    stop("The SS model does not have selectivity values matching the ages you entered (", s_min, "-", s_max, "). ",
         "Debug the `survey` object in load_ss_sel_parameters() to see why. This could be due to parameter",
         "names changing in SS which are matched by a regular expression.",
         call. = FALSE)
  }
  survey <- survey %>%
    transmute(value = Value) %>%
    mutate(source = 2) %>%
    mutate(age = survey_ages[-length(survey_ages)])

  as.matrix(bind_rows(fish, survey))
}

#' Create and return a list of stats to attach to the main model by
#' looking in the model's path for the report files.
#'
#' @param model The model object as output by [load_ss_files()]
#' @param ss_mcmc_quants Quantile probability values to use
#' @param ... Absorb arguments meant for other functions
#'
#' @return A [list] of values ectracted and calculed from the extra MCMC runs
#' @importFrom readr read_table2 cols
#' @importFrom purrr flatten
#' @importFrom tibble add_column
#' @importFrom dplyr group_nest slice
#' @importFrom crayon yellow
#' @export
fetch_extra_mcmc <- function(model = NULL,
                             ss_mcmc_quants = NULL,
                             ...){

  posts_file_name <- "posteriors.sso"

  extra_mcmc_reports_dir <- file.path(model$extra_mcmc_dir, "reports")
  extra_mcmc <- NULL

  if(!dir.exists(model$extra_mcmc_dir)){
    cat(red(symbol$cross),
            red(" The", model$extra_mcmc_dir,
                "directory does not exist, so the extra-mcmc wass not loaded\n"))
    return(NULL)
  }
  if(!dir.exists(extra_mcmc_reports_dir)){
    cat(red(symbol$cross),
            red("The", extra_mcmc_reports_dir,
                "directory does not exist, so the extra-mcmc wass not loaded\n"))
    return(NULL)
  }

  ## Get the extra-mcmc directories
  extra_mcmc_dirs <- dir(model$extra_mcmc_dir)
  extra_mcmc_dirs <- file.path(model$extra_mcmc_dir,
                               extra_mcmc_dirs[grepl("extra-mcmc", extra_mcmc_dirs)])

  ## Get the number of Report.sso files in the directory
  dir_list <- dir(extra_mcmc_reports_dir)
  if(!length(dir_list)){
    cat(red(symbol$cross),
            red("There are no report files in the",
                extra_mcmc_reports_dir, "directory\n"))
    return(NULL)
  }
  report_files <- grep("^Report_[[:digit:]]+\\.sso$", dir_list)
  num_reports <- length(report_files)
  comp_files <- grep("^CompReport_[[:digit:]]+\\.sso$", dir_list)
  num_comp_reports <- length(comp_files)
  cat(green("Loading extra MCMC report files\n"))

  ## Suppress warnings because there is an extra whitespace at the end of the header line in the file.
  suppressWarnings(
    posts <- read_table2(file.path(model$mcmc_dir, posts_file_name),
                         col_types = cols())
  )
  ## Remove extra MLE run outputs. SS appends a new header followed by a 0-Iter row for an MLE run.
  ## Sometimes MLEs are run by accident or on purpose at another time and forgotten about.
  posts <- posts %>% filter(Iter != "Iter",
                            Iter != 0)

  ## Break up the loading of report files into the number of posteriors in each extra-mcmc subdir
  num_reports_each <- map_int(extra_mcmc_dirs, ~{
    ## Suppress warnings because there may be extra 'Iter' lines followed by '0' lines
    ## because the SS MLE just appends these to the posteriors.sso file
    suppressWarnings(
      posts <- read_table2(file.path(.x, posts_file_name),
                           col_types = cols())
    )
    posts <- posts %>% filter(Iter != "Iter",
                              Iter != 0)
    nrow(posts)
  })

  ## from_to is a two-column dataframe with the indices from and to for each processor to load report files
  from_to <- tibble(from = 1, to = num_reports_each[1])
  for(i in 2:length(num_reports_each)){
    nxt <- tibble(from = from_to[i - 1,]$to + 1, to = from_to[i - 1,]$to + num_reports_each[i])
    from_to <- bind_rows(from_to, nxt)
  }

  ## Load all report files into a list, 1 element for each report file. Elements that are NA had no file found

  reps <- map(1:nrow(from_to), ~{
    inds <- as.numeric(from_to[.x, 1]):as.numeric(from_to[.x, 2])
    map(inds, ~{
      rep_file <- file.path(extra_mcmc_reports_dir, paste0("Report_", .x, ".sso"))
      if(!file.exists(rep_file)){
        return(NA)
      }
      readLines(rep_file)
    })
  }) %>%
    flatten()

  ## Load all compreport files into a list, 1 element for each report file. Elements that are NA had no file found
  compreps <- map(1:nrow(from_to), ~{
    inds <- as.numeric(from_to[.x, 1]):as.numeric(from_to[.x, 2])
    map(inds, ~{
      comprep_file <- file.path(extra_mcmc_reports_dir, paste0("CompReport_", .x, ".sso"))
      if(!file.exists(comprep_file)){
        return(NA)
      }
      readLines(comprep_file)
    })
  }) %>%
    flatten()

  cat(green(symbol$tick),
      green("Finished loading extra MCMC report files\n"))
  cat(green("Extracting outputs from extra MCMC report data frames\n"))

  # Make custom reps_ objects for each output. Only relevant portions of the report file will be passed to
  # the table-making map2() calls later (speeds up the map2() calls)
  rep_example <- reps[[which(!is.na(reps))[1]]]
  comprep_example <- compreps[[which(!is.na(compreps))[1]]]

  # Biomass -------------------------------------------------------------------
  bio_header_ind <- grep("^TIME_SERIES", rep_example) + 1
  bio_header_line <- rep_example[bio_header_ind]
  bio_header <- str_split(bio_header_line, " +")[[1]]
  bio_start_ind <- bio_header_ind + 1
  bio_end_ind <- grep("^SPR_series", rep_example) - 2
  reps_bio <- map(reps, ~{.x[bio_start_ind:bio_end_ind]})

  # Likelihood ----------------------------------------------------------------
  like_start_ind <- grep("^LIKELIHOOD", rep_example) + 1
  like_end_ind <- like_start_ind + 17
  reps_like <- map(reps, ~{.x[like_start_ind:like_end_ind]})

  # Selectivity ---------------------------------------------------------------
  next_yr <- model$endyr + 1
  sel_header_ind <- grep("Factor Fleet Yr Seas", rep_example)
  sel_header_line <- rep_example[sel_header_ind]
  sel_header <- str_split(sel_header_line, " +")[[1]]
  sel_ind <- grep(paste0(next_yr, "_1Asel"), rep_example)
  reps_sel <- map(reps, ~{.x[sel_ind]})
  sel <- extract_rep_table(reps_sel, sel_header) %>%
    select(-c(2, 3, 5, 6, 7, 8)) %>%
    map_df(as.numeric) %>%
    filter(Yr == next_yr) %>%
    select(-c(Iter, Yr))

  # Selectivity * Weight ------------------------------------------------------
  selwt_ind <- grep(paste0(next_yr, "_1_sel\\*wt"), rep_example)
  reps_selwt <- map(reps, ~{.x[selwt_ind]})
  selwt <- extract_rep_table(reps_selwt, sel_header) %>%
    select(-c(2, 3, 5, 6, 7, 8)) %>%
    map_df(as.numeric) %>%
    filter(Yr == next_yr) %>%
    select(-c(Iter, Yr))

  # Numbers-at-age ------------------------------------------------------------
  natage_header_ind <- grep("NUMBERS_AT_AGE_Annual_2 With_fishery", rep_example) + 1
  natage_header <- str_split(rep_example[natage_header_ind], " +")[[1]]
  natage_start_ind <- natage_header_ind + 1
  natage_end_ind <- grep("Z_AT_AGE_Annual_2 With_fishery", rep_example) - 2
  reps_natage <- map(reps, ~{.x[natage_start_ind:natage_end_ind]})
  natage <- extract_rep_table(reps_natage, natage_header) %>%
    select(-c(2, 3)) %>%
    map_df(as.numeric) %>%
    filter(Yr == next_yr) %>%
    select(-c(Iter, Yr))

  # Q -------------------------------------------------------------------------
  q_header_ind <- grep("^INDEX_2", rep_example) + 1
  q_header <- str_split(rep_example[q_header_ind], " +")[[1]]
  q_start_ind <- q_header_ind + 1
  ncpue <- nrow(model$dat$CPUE)
  q_end_ind <- q_start_ind + ncpue - 1
  reps_q <- map(reps, ~{.x[q_start_ind:q_end_ind]})

  # Comp tables ---------------------------------------------------------------
  comp_header_ind <- grep("Composition_Database", comprep_example) + 1
  comp_header <- str_split(comprep_example[comp_header_ind], " +")[[1]]
  comp_start_ind <- comp_header_ind + 1
  comp_end_ind <- grep("End_comp_data", comprep_example) - 1
  reps_comp <- map(compreps, ~{.x[comp_start_ind:comp_end_ind]})

  # Apply selectivity to numbers-at-age ---------------------------------------
  natsel <- natage * sel
  natselwt <- natage * selwt
  extra_mcmc$natsel.prop <- natsel %>%
    mutate(rsum = rowSums(.)) %>%
    mutate_at(.vars = vars(-rsum), .funs = ~{.x / rsum}) %>%
    select(-rsum)
  extra_mcmc$natselwt.prop <- natselwt %>%
    mutate(rsum = rowSums(.)) %>%
    mutate_at(.vars = vars(-rsum), .funs = ~{.x / rsum}) %>%
    select(-rsum)

  # Extra-mcmc CPUE table and values (Q) --------------------------------------
  q <- extract_rep_table(reps_q, q_header) %>%
    select(Iter, Exp, Calc_Q)
  iter <- unique(q$Iter)
  cpue <- q %>%
    select(-Calc_Q) %>%
    group_by(Iter) %>%
    group_nest()
  cpue <- do.call(cbind, cpue$data)
  names(cpue) <- iter
  extra_mcmc$cpue.table <- cpue %>%
    as_tibble() %>%
    map_df(~{as.numeric(.x)})

  extra_mcmc$Q_vector <- q %>%
    group_by(Iter) %>%
    slice(1) %>%
    pull(Calc_Q) %>%
    as.numeric()

  cpue <- apply(extra_mcmc$cpue.table,
                MARGIN = 1,
                FUN = function(x){quantile(as.numeric(x),
                                           probs = ss_mcmc_quants)
                })
  extra_mcmc$cpue.0.025 <- as.numeric(cpue[1,])
  extra_mcmc$cpue.median <- as.numeric(cpue[2,])
  extra_mcmc$cpue.0.975 <- as.numeric(cpue[3,])

  # Extra-mcmc timeseries data ------------------------------------------------
  # Add info on distribution of total biomass to existing time series data frame
  timeseries <- extract_rep_table(reps_bio, bio_header) %>%
    select(Iter, Bio_all, Bio_smry)
  iter <- unique(timeseries$Iter)
  Bio_all <- timeseries %>%
    select(Iter, Bio_all) %>%
    group_by(Iter) %>%
    group_nest()
  Bio_all <- do.call(cbind, Bio_all$data)
  names(Bio_all) <- iter
  Bio_all <- apply(Bio_all,
                   MARGIN = 1,
                   FUN = function(x){quantile(as.numeric(x),
                                              probs = ss_mcmc_quants)
                   })
  extra_mcmc$timeseries <- model$timeseries
  extra_mcmc$timeseries$Bio_all.0.025 <- as.numeric(Bio_all[1,])
  extra_mcmc$timeseries$Bio_all.median <- as.numeric(Bio_all[2,])
  extra_mcmc$timeseries$Bio_all.0.975 <- as.numeric(Bio_all[3,])

  Bio_smry <- timeseries %>%
    select(Iter, Bio_smry) %>%
    group_by(Iter) %>%
    group_nest()
  Bio_smry <- do.call(cbind, Bio_smry$data)
  names(Bio_smry) <- iter
  Bio_smry <- apply(Bio_smry,
                    MARGIN = 1,
                    FUN = function(x){quantile(as.numeric(x),
                                               probs = ss_mcmc_quants)
                    })
  extra_mcmc$timeseries$Bio_smry.0.025 <- as.numeric(Bio_smry[1,])
  extra_mcmc$timeseries$Bio_smry.median <- as.numeric(Bio_smry[2,])
  extra_mcmc$timeseries$Bio_smry.0.975 <- as.numeric(Bio_smry[3,])

  # Extra-mcmc Pearson residuals ----------------------------------------------
  comp <- extract_rep_table(reps_comp, comp_header)
  ## median and quantiles of expected values and Pearsons
  iter <- unique(comp$Iter)
  comp <- comp %>%
    filter(!is.na(N), N > 0)
  exp_table <- comp %>%
    select(Iter, Exp) %>%
    group_by(Iter) %>%
    group_nest()
  exp_table <- do.call(cbind, exp_table$data)
  names(exp_table) <- iter
  exp_table <- apply(exp_table,
                     MARGIN = 1,
                     FUN = function(x){quantile(as.numeric(x),
                                                probs = ss_mcmc_quants)
                     })
  extra_mcmc$agedbase <- model$agedbase
  extra_mcmc$agedbase$Exp.025 <- exp_table[1,]
  extra_mcmc$agedbase$Exp <- exp_table[2,]
  extra_mcmc$agedbase$Exp.975 <- exp_table[3,]

  pearson_table <- comp %>%
    select(Iter, Pearson) %>%
    group_by(Iter) %>%
    group_nest()
  pearson_table <- do.call(cbind, pearson_table$data)
  names(pearson_table) <- iter
  pearson_table <- apply(pearson_table,
                         MARGIN = 1,
                         FUN = function(x){quantile(as.numeric(x),
                                                    probs = ss_mcmc_quants)
                         })
  extra_mcmc$agedbase$Pearson.025 <- pearson_table[1,]
  extra_mcmc$agedbase$Pearson <- pearson_table[2,]
  extra_mcmc$agedbase$Pearson.975 <- pearson_table[3,]
  cat(green(symbol$tick),
      green("Finished extracting outputs from extra MCMC report data frames\n"))

  extra_mcmc
}
