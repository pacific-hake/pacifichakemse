#' Load a selectivity parameter estimates from the SS model output
#'
#' @param ss_model SS3 model output as created by [create_rds_file()]
#' @param ... Arguments absorbed which are meant for other functions
#'
#' @return A data frame of estimates (value column), age, and source
#' (fishery and survey are source 1 and 2 respectively)
#'
#' @export
load_ss_sel_parameters <- function(ss_model = NULL,
                                   ...){

  # Get fishery ages from the names of the MCMC columns
  fishery_pat <- "^AgeSel_P(\\d+)_Fishery\\(1\\)$"
  mcmc_df <- ss_model$mcmc |>
    as_tibble()
  nms <- grep(fishery_pat, names(mcmc_df), val = TRUE)

  # Extract the ages from the parameter names. subtract 1 to account for
  #  age zero in the parameters
  fishery_ages <- gsub(fishery_pat,
                       "\\1",
                       nms) |>
    as.numeric()
  fishery_ages <- fishery_ages - 1
  if(!1 %in% fishery_ages){
    fishery_ages <- c(1, fishery_ages)
  }

  # Get survey ages from the names of the MCMC columns
  surv_pat <- "^AgeSel_P(\\d+)_Acoustic_Survey\\(2\\)$"
  nms <- grep(surv_pat,
              names(mcmc_df),
              val = TRUE)
  # Extract the ages from the parameter names. subtract 1 to account for
  #  age zero in the parameters
  survey_ages <- gsub(surv_pat,
                       "\\1",
                       nms) |>
    as.numeric()
  survey_ages <- survey_ages - 2

  extract_sel <- function(d, src, ages){
    d |>
      select(-yr) |>
      summarize(across(everything(), ~{median(.x)})) |>
      pivot_longer(everything()) |>
      mutate(source = src) |>
      rename(age = name) |>
      filter(age %in% ages) |>
      select(source, age, value) |>
      mutate(age = as.numeric(age))
  }
  fishery_sel_df <- extract_sel(ss_model$extra_mcmc$sel_fishery_med,
                                1,
                                fishery_ages)
  survey_sel_df <- extract_sel(ss_model$extra_mcmc$sel_survey_med,
                               2,
                               survey_ages)

  bind_rows(fishery_sel_df, survey_sel_df)
}
