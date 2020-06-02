#' Create an rds file to hold an SS model's data and outputs.
#'
#' @param model_dir Directory name of model to be loaded
#' @param overwrite_ss_rds Logical. Overwrite the RDS file if it exists
#' @param ... Absorb arguments destined for other functions
#'
#' @return [base::invisible()]
#' @importFrom r4ss SSgetMCMC
#' @export
create_rds_file <- function(model_dir = NULL,
                            overwrite_ss_rds = TRUE,
                            ...){

  verify_argument(model_dir, "character", 1)
  verify_argument(overwrite_ss_rds, "logical", 1)

  if(!dir.exists(model_dir)){
    stop("Error - the directory ", model_dir, " does not exist.\n",
         "Fix the problem and try again.", call. = FALSE)
  }

  rds_file <- file.path(model_dir, paste0(basename(model_dir), ".rds"))
  # The RDS file will have the same name as the directory it is in
  if(file.exists(rds_file) && overwrite_ss_rds){
    unlink(rds_file, force = TRUE)
  }

  if(!file.exists(rds_file)){
    cat(green("Creating a new RDS file in", model_dir, "\n"))
    # If this point is reached, no RDS file exists so it has to be built from scratch
    model <- load_ss_files(model_dir)
    model$mcmc_dir <- file.path(model_dir, "mcmc")
    if(dir.exists(model$mcmc_dir)){
      cat(green("Loading MCMC posteriors\n"))
      mcmc_out <- SSgetMCMC(dir = model$mcmc_dir,
                            writecsv = FALSE,
                            verbose = FALSE)
      model$mcmccalcs <- calc_mcmc(mcmc_out, ...)
      cat(green(symbol$tick),
          green(" Finished loading MCMC posteriors\n"))
    }else{
      cat(red(symbol$cross),
          red(" MCMC posteriors not loaded (mcmc directory not found)\n"))
    }
    model$extra_mcmc_dir <- file.path(model_dir, "extra-mcmc")
    if(dir.exists(model$extra_mcmc_dir)){
      cat(green("Loading extra MCMC outputs\n"))
      model$extra_mcmc <- fetch_extra_mcmc(model, ...)
      cat(green(symbol$tick),
          green(" Finished loading extra MCMC output\n"))
    }else{
      cat(red(symbol$cross),
          red(" Extra MCMC outputs not loaded (extra-mcmc directory not found)\n"))
    }
    saveRDS(model, file = rds_file)
    cat(green(symbol$tick),
        green("Created RDS file successfully\n"))
  }
  rds_file
}

#' Load models from files created using [create_rds_file()]
#'
#' @details Load an SS3 model from an RDS file and return as a [list]
#'
#' @param model_dir A [vector] of model directory names
#' @param ... Arguments to be passed to [create_rds_file()]
#'
#' @return A [list] of model inputs/outputs for the requested model
#' @export
load_ss_model_from_rds <- function(model_dir = NULL,
                                   ...){

  verify_argument(model_dir, "character", 1)

  rds_file <- create_rds_file(model_dir, ...)
  readRDS(rds_file)
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

#' Return a list of mcmc calculations, e.g. quantiles for various values
#'
#' @param mcmc The output of the [r4ss::SSgetMCMC()] function as a data.frame
#' @param lower Lower quantile value
#' @param upper Upper quantile value
#' @param biomass_scale Scale the biomass by this amount. The default is 2e6 because
#' biomass will be shown in the millions of tonnes and it is female only
#' @param recruitment_scale Scale the recruitment by this amount. The default is 1e6
#' because recruitment will be shown in millions of tonnes
#' @param ... Absorb arguments destined for other functions
#'
#' @return
#' @importFrom r4ss SSgetMCMC
#' @importFrom dplyr mutate_all
#' @export
calc_mcmc <- function(mcmc,
                      ss_mcmc_quants = NULL,
                      biomass_scale = 2e6,
                      recruitment_scale = 1e6,
                      ...){

  verify_argument(mcmc, "data.frame")
  verify_argument(ss_mcmc_quants, "numeric", 3)
  verify_argument(biomass_scale, "numeric", 1)
  verify_argument(recruitment_scale, "numeric", 1)
  if(ss_mcmc_quants[1] > ss_mcmc_quants[2] || ss_mcmc_quants[2] > ss_mcmc_quants[3]){
    stop("`ss_mcmc_quants` must be in ascending order",
         call. = FALSE)
  }
  lower <- ss_mcmc_quants[1]
  med <- ss_mcmc_quants[2]
  upper <- ss_mcmc_quants[3]

  lst <- NULL
  mcmc <- as_tibble(mcmc)

  ssb <- mcmc %>%
    select(contains("SSB"))
  ssb <- ssb / biomass_scale
  ssb <- ssb %>%
    set_names(gsub("SSB_", "", names(.)))

  lst$svirg <- quantile(ssb %>% select(Virgin) %>% pull(),
                        ss_mcmc_quants)
  lst$sinit <- quantile(ssb %>% select(Initial) %>% pull(),
                        ss_mcmc_quants)

  # ssb_initial is added back later so that depletion calculations can be done
  ssb_initial <- ssb %>% select(Initial) %>% pull()
  names_to_remove <- c("Virgin", "Initial", "unfished", "Btgt", "SPR", "MSY")
  if("B_MSY/unfished" %in% names(ssb)){
    # Only in post-2018 SS3 outputs
    names_to_remove <- c(names_to_remove, "B_MSY/unfished")
  }
  ssb <- ssb %>% select(-names_to_remove)
  lst$slower <- apply(ssb, 2, quantile, prob = lower)
  lst$smed   <- apply(ssb, 2, quantile, prob = med)
  lst$supper <- apply(ssb, 2, quantile, prob = upper)

  ssb <- ssb %>% mutate(Initial = ssb_initial)
  depl <- ssb %>%
    mutate_at(.vars = vars(-Initial), .funs = ~{. / Initial}) %>%
    select(-Initial)
  lst$dlower <- apply(depl, 2, quantile, prob = lower)
  lst$dmed   <- apply(depl, 2, quantile, prob = med)
  lst$dupper <- apply(depl, 2, quantile, prob = upper)

  recr <- mcmc %>%
    select(contains("Recr_"))
  recr <- recr / recruitment_scale
  recr <- recr %>%
    set_names(gsub("Recr_", "", names(.)))
  recr <- recr %>% select(-starts_with("Fore"))
  lst$rvirg <- quantile(recr %>% select(Virgin) %>% pull(),
                        ss_mcmc_quants)
  lst$rinit <- quantile(recr %>% select(Initial) %>% pull(),
                        ss_mcmc_quants)
  lst$runfished <- quantile(recr %>% select(unfished) %>% pull(),
                            ss_mcmc_quants)
  recr <- recr %>% select(-c("Virgin", "Initial", "unfished"))
  lst$rmed <- apply(recr, 2, quantile, prob = med)
  lst$rmean <- apply(recr, 2, mean)
  lst$rlower <- apply(recr, 2, quantile, prob = lower)
  lst$rupper <- apply(recr, 2, quantile, prob = upper)

  dev <- mcmc %>%
    select(starts_with(c("Early_InitAge_",
                         "Early_RecrDev_",
                         "Main_RecrDev_",
                         "Late_RecrDev_",
                         "ForeRecr_")))
  names(dev) <- gsub("Early_RecrDev_", "", names(dev))
  names(dev) <- gsub("Main_RecrDev_", "", names(dev))
  names(dev) <- gsub("Late_RecrDev_", "", names(dev))
  names(dev) <- gsub("ForeRecr_", "", names(dev))

  # Change the Early_Init names to be the correct preceding years
  start_yr <- as.numeric(min(names(dev)))
  early <- grep("Early_InitAge_", names(dev))
  num_early_yrs <- length(early)
  early_yrs <- seq(start_yr - num_early_yrs, start_yr - 1, 1)
  late_yrs <- names(dev[-early])
  names(dev) <- c(as.character(early_yrs), late_yrs)

  lst$devlower <- apply(dev, 2, quantile, prob = lower)
  lst$devmed <- apply(dev, 2, quantile, prob = med)
  lst$devupper <- apply(dev, 2, quantile, prob = upper)

  spr <- mcmc %>%
    select(contains("SPRratio_"))
  spr <- spr %>%
    set_names(gsub("SPRratio_", "", names(.)))
  lst$plower <- apply(spr, 2, quantile, prob = lower)
  lst$pmed <- apply(spr, 2, quantile, prob = med)
  lst$pupper <- apply(spr, 2, quantile, prob = upper)

  f <- mcmc %>%
    select(contains("F_"))
  f <- f %>%
    set_names(gsub("F_", "", names(.)))
  lst$flower <- apply(f, 2, quantile, prob = lower)
  lst$fmed   <- apply(f, 2, quantile, prob = med)
  lst$fupper <- apply(f, 2, quantile, prob = upper)

  # Reference point calculations
  lst$unfish_fem_bio <- quantile(mcmc$SSB_Virgin,
                                 prob = ss_mcmc_quants) / biomass_scale * 1000
  lst$unfish_recr <- quantile(mcmc$Recr_Virgin,
                              prob = ss_mcmc_quants) / recruitment_scale * 1000
  lst$f_spawn_bio_bf40 <- quantile(mcmc$SSB_SPR,
                                   prob = ss_mcmc_quants) / biomass_scale * 1000
  lst$exp_frac_spr <- quantile(mcmc$Fstd_SPR,
                               prob = ss_mcmc_quants)
  lst$yield_bf40 <- quantile(mcmc$Dead_Catch_SPR,
                             prob = ss_mcmc_quants) / recruitment_scale * 1000
  lst$fem_spawn_bio_b40 <- quantile(mcmc$SSB_Btgt,
                                    prob = ss_mcmc_quants) / biomass_scale * 1000
  lst$spr_b40 <- quantile(mcmc$SPR_Btgt,
                          prob = ss_mcmc_quants)
  lst$exp_frac_b40 <- quantile(mcmc$Fstd_Btgt,
                               prob = ss_mcmc_quants)
  lst$yield_b40 <- quantile(mcmc$Dead_Catch_Btgt,
                            prob = ss_mcmc_quants) / recruitment_scale * 1000
  lst$fem_spawn_bio_bmsy <- quantile(mcmc$SSB_MSY,
                                     prob = ss_mcmc_quants) / biomass_scale * 1000
  lst$spr_msy <- quantile(mcmc$SPR_MSY,
                          prob = ss_mcmc_quants)
  lst$exp_frac.sprmsy <- quantile(mcmc$Fstd_MSY,
                                  prob = ss_mcmc_quants)
  lst$msy <- quantile(mcmc$Dead_Catch_MSY,
                      prob = ss_mcmc_quants) / recruitment_scale * 1000

  lst
}

#' Load the SS model input and output data needed by this package in correct format
#'
#' @param ss_model SS3 model output as created by [create_rds_file()]
#' @param s_yr Start year
#' @param m_yr End year
#' @param weight_factor A factor to multiply and divide SSB values by
#' @param ... Arguments to be passed to [load_ss_sel_parameters()]
#'
#' @return A [list] with objects to be used throughout the package code
load_ss_model_data <- function(ss_model,
                               s_yr = NULL,
                               m_yr = NULL,
                               weight_factor = 1000,
                               ...){

  verify_argument(ss_model, "list")
  verify_argument(s_yr, "numeric", 1)
  verify_argument(m_yr, "numeric", 1)

  lst <- NULL
  yrs <- s_yr:m_yr

  lst$parms_scalar <- load_ss_parameters(ss_model)
  lst$parms_sel <- load_ss_sel_parameters(ss_model,
                                          ...)
  lst$age_survey_df <- extract_age_comps(ss_model,
                                         age_comps_fleet = 2,
                                         s_yr = s_yr,
                                         m_yr = m_yr,
                                         ...)
  lst$age_catch_df <- extract_age_comps(ss_model,
                                        age_comps_fleet = 1,
                                        s_yr = s_yr,
                                        m_yr = m_yr,
                                        ...)

  # The following is shown in a table in the assessment doc and made by the
  # make.median.posterior.table() function in the hake-assessment repository
  mc <- ss_model$mcmccalcs
  extra_mc <- ss_model$extra_mcmc$timeseries
  vals_mc <- mc[c("smed", "dmed", "rmed", "pmed", "fmed")]
  vals_mc <- map(vals_mc, ~{
    .x[names(.x) %in% yrs]
  })
  b <- extra_mc$Bio_all[extra_mc$Yr %in% yrs] / weight_factor
  lst$med_popests <-  tibble(f_ssb = vals_mc$smed * weight_factor,
                             rel_ssb = vals_mc$dmed,
                             b = b,
                             r = vals_mc$rmed * weight_factor,
                             spr_f = vals_mc$pmed,
                             e = vals_mc$fmed) %>%
    mutate(yr = yrs) %>%
    select(yr, everything())
  # Make Relative fishing intensity and Exploitation fraction `NA` for the last year
  # because the catch for that year has not occurred yet and the values output
  # by the model are meaningless\
  nrow_p <- nrow(lst$med_popests)
  ncol_p <- ncol(lst$med_popests)
  lst$med_popests[nrow_p, ncol_p - 1] <- NA
  lst$med_popests[nrow_p, ncol_p] <- NA

  # Recruitment deviations
  lst$r_dev <- ss_model$recruitpars %>%
    as_tibble() %>%
    filter(grepl("RecrDev", type)) %>%
    filter(!grepl("^Late_", type)) %>%
    select(yr = Yr, value = Value)

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

#' Create and return a list of stats to attach to the main model by
#' looking in the model's path for the report files.
#'
#' @param model The model object as output by [load_ss_files()]
#' @param ss_mcmc_quants Quantile probability values to use
#' @param ... Absorb arguments meant for other functions
#' @return
#' @importFrom readr read_table2 cols
#' @importFrom purrr flatten
#' @importFrom tibble add_column
#' @importFrom dplyr group_nest slice
#' @importFrom crayon yellow
#' @export
fetch_extra_mcmc <- function(model = NULL,
                             ss_mcmc_quants = NULL,
                             ...){

  verify_argument(model, "list")
  verify_argument(ss_mcmc_quants, "numeric", 3)

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

  ## Make custom reps_ objects for each output. Only relevant portions of the report file will be passed to
  ## the table-making map2() calls later (speeds up the map2() calls)
  rep_example <- reps[[which(!is.na(reps))[1]]]
  comprep_example <- compreps[[which(!is.na(compreps))[1]]]
  ## Biomass
  bio_header_ind <- grep("^TIME_SERIES", rep_example) + 1
  bio_header_line <- rep_example[bio_header_ind]
  bio_header <- str_split(bio_header_line, " +")[[1]]
  bio_start_ind <- bio_header_ind + 1
  bio_end_ind <- grep("^SPR_series", rep_example) - 2
  reps_bio <- map(reps, ~{.x[bio_start_ind:bio_end_ind]})
  # Likelihood
  like_start_ind <- grep("^LIKELIHOOD", rep_example) + 1
  like_end_ind <- like_start_ind + 17
  reps_like <- map(reps, ~{.x[like_start_ind:like_end_ind]})
  ## Selectivity
  next_yr <- model$endyr + 1
  sel_header_ind <- grep("Factor Fleet Yr Seas", rep_example)
  sel_header_line <- rep_example[sel_header_ind]
  sel_header <- str_split(sel_header_line, " +")[[1]]
  sel_ind <- grep(paste0(next_yr, "_1Asel"), rep_example)
  reps_sel <- map(reps, ~{.x[sel_ind]})
  ## Selectivity * Weight
  selwt_ind <- grep(paste0(next_yr, "_1_sel\\*wt"), rep_example)
  reps_selwt <- map(reps, ~{.x[selwt_ind]})
  ## Natage
  natage_header_ind <- grep("NUMBERS_AT_AGE_Annual_2 With_fishery", rep_example) + 1
  natage_header <- str_split(rep_example[natage_header_ind], " +")[[1]]
  natage_start_ind <- natage_header_ind + 1
  natage_end_ind <- grep("Z_AT_AGE_Annual_2 With_fishery", rep_example) - 2
  reps_natage <- map(reps, ~{.x[natage_start_ind:natage_end_ind]})
  ## Q
  q_header_ind <- grep("^INDEX_2", rep_example) + 1
  q_header <- str_split(rep_example[q_header_ind], " +")[[1]]
  q_start_ind <- q_header_ind + 1
  ncpue <- nrow(model$dat$CPUE)
  q_end_ind <- q_start_ind + ncpue - 1
  reps_q <- map(reps, ~{.x[q_start_ind:q_end_ind]})
  ## Comp tables
  comp_header_ind <- grep("Composition_Database", comprep_example) + 1
  comp_header <- str_split(comprep_example[comp_header_ind], " +")[[1]]
  comp_start_ind <- comp_header_ind + 1
  comp_end_ind <- grep("End_comp_data", comprep_example) - 1
  reps_comp <- map(compreps, ~{.x[comp_start_ind:comp_end_ind]})

  #' Extract the vectors from a list into a [tibble::tibble()]
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
      as_tibble()
  }

  sel <- extract_rep_table(reps_sel, sel_header) %>%
    select(-c(2, 3, 5, 6, 7, 8)) %>%
    map_df(as.numeric) %>%
    filter(Yr == next_yr) %>%
    select(-c(Iter, Yr))
  selwt <- extract_rep_table(reps_selwt, sel_header) %>%
    select(-c(2, 3, 5, 6, 7, 8)) %>%
    map_df(as.numeric) %>%
    filter(Yr == next_yr) %>%
    select(-c(Iter, Yr))
  natage <- extract_rep_table(reps_natage, natage_header) %>%
    select(-c(2, 3)) %>%
    map_df(as.numeric) %>%
    filter(Yr == next_yr) %>%
    select(-c(Iter, Yr))

  ## Apply selectivity to numbers-at-age
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

  # CPUE table and values (Q)
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

  ## Add info on distribution of total biomass to existing time series data frame
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
