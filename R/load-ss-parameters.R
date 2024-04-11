#' Load a list of parameter estimates from the SS model output
#'
#' @param ss_model SS3 model output as created by [hake::create_rds_file()]
#'
#' @return A named list of parameter estimates
#'
#' @export
load_ss_parameters <- function(ss_model = NULL){

  if(!is.list(ss_model)){
    stop("`ss_model` object must be a list")
  }

  mle_parm_tbl <- ss_model$parameters |>
    as_tibble()

  # Find the MCMC parameters that are present in the MLE outputs.
  # This might be done better, but was  a quick way to guarantee the
  # rest of the code works. We should only load the parameters used in the
  # OM
  #  xx <- parm_tbl$Label |> map_lgl(~{.x %in% names(ss_model$mcmc)})

  mcmc_param_tbl <- ss_model$mcmc |>
    as_tibble()
  nms <- names(mcmc_param_tbl)

  if(!length(grep("^SR_LN", nms))){
    stop("The `log_r_init` parameter was not found in the SS model output. ",
         "The regular expression on the parameter label is `^SR_LN`.",
         call. = FALSE)
  }

  if(!length(grep("^SR_BH", nms))){
    stop("The `log_h` parameter was not found in the SS model output. ",
         "The regular expression on the parameter label is `^SR_BH`.",
         call. = FALSE)
  }

  if(!length(grep("^NatM", nms))){
    stop("The `log_m_init` parameter was not found in the SS model output. ",
         "The regular expression on the parameter label is `^NatM`.",
         call. = FALSE)
  }

  if(!length(grep("^NatM", nms))){
    stop("The `log_sd_surv` parameter was not found in the SS model output. ",
         "The regular expression on the parameter label is ",
         "`^Q_extraSD_Acoustic_Survey`.",
         call. = FALSE)
  }

  if(!length(grep("^SR_sigmaR", nms))){
    stop("The `log_sd_r` parameter was not found in the SS model output. ",
         "The regular expression on the parameter label is `^SR_sigmaR`.",
         call. = FALSE)
  }

  if(!length(grep("^ln\\(EffN_mult\\)_1$", nms))){
    if(!length(grep("^ln\\(DM_theta\\)_Age_P1$", nms))){
      stop("The `log_phi_catch` parameter was not found in the SS ",
           "model output. The regular expression on the parameter label is ",
           "`^ln\\(DM_theta\\)_Age_P1$`.",
           call. = FALSE)
    }
  }

  if(!length(grep("^ln\\(EffN_mult\\)_2$", nms))){
    if(!length(grep("^ln\\(DM_theta\\)_Age_P2$", nms))){
      stop("The `log_phi_survey` parameter was not found in the SS ",
           "model output. The regular expression on the parameter label is ",
           "`^ln\\(DM_theta\\)_Age_P2$`.",
           call. = FALSE)
    }
  }

  lst <- NULL

  lst$log_r_init <- parm_tbl |>
    filter(grepl("^SR_LN", Label)) |>
    pull(Value)

  lst$log_h <- parm_tbl |>
    filter(grepl("^SR_BH", Label)) |>
    pull(Value) |>
    log()

  lst$log_m_init <- parm_tbl |>
    filter(grepl("^NatM", Label)) |>
    pull(Value) |>
    log()

  lst$log_sd_surv <- parm_tbl |>
    filter(grepl("^Q_extraSD_Acoustic_Survey", Label)) |>
    pull(Value) |>
    log()

  lst$log_sd_r <- parm_tbl |>
    filter(grepl("^SR_sigmaR", Label)) |>
    pull(Value) |>
    log()

  lst$log_phi_catch <- parm_tbl |>
    filter(grepl("^ln\\(EffN_mult\\)_1$", Label)) |>
    pull(Value)
  if(!length(lst$log_phi_catch)){
    lst$log_phi_catch <- parm_tbl |>
      filter(grepl("^ln\\(DM_theta\\)_Age_P1$", Label)) |>
      pull(Value)
  }

  lst$log_phi_survey <- parm_tbl |>
    filter(grepl("^ln\\(EffN_mult\\)_2$", Label)) |>
    pull(Value)
  if(!length(lst$log_phi_survey)){
    lst$log_phi_survey <- parm_tbl |>
      filter(grepl("^ln\\(DM_theta\\)_Age_P2$", Label)) |>
      pull(Value)
  }

  lst
}
