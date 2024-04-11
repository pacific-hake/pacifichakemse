

#' Create and return a list of stats to attach to the main model by
#' looking in the model's path for the report files.
#'
#' @param model The model object as output by [load_ss_files()]
#' @param ss_mcmc_quants Quantile probability values to use
#' @param ... Absorb arguments meant for other functions
#'
#' @return A [list] of values ectracted and calculed from the extra MCMC runs
#'
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
