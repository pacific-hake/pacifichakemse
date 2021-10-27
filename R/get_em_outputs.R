#' Get outputs from the Estimation models
#'
#' @param em The estimation model object for a single scenario
#' @param quants A vector of quantiles to apply
#' @param scen_name Scenario name to be appended to each table
#' @param ... Absorbs unused arguments
#'
#' @return A list of the EM outputs
#' @export
get_em_outputs <- function(em,
                           scen_name = NULL,
                           quants = c(0.05, 0.5, 0.95),
                           ...){

  verify_argument(em, "list")
  verify_argument(scen_name, "character", 1)
  verify_argument(quants, "numeric")

  yrs <- names(em[[1]]$ssb_se[[length(em[[1]]$ssb_se)]])
  out <- NULL

  get_output_by_run <- function(x, item = NULL){
    item <- sym(item)
    out <- map2(x, seq_along(x), ~{
      names(.x[[item]]) <- as.character(seq_along(.x[[item]]))
      # Choose one of the ones with extended output when others may be NULL
      history_len <- length(.x[[item]][map_lgl(.x$has_extended_output, ~{.x})][[1]])
      proj_len <- length(.x[[item]])
      dat <- map(.x[[item]], ~{
        if(is.null(.x)){
          NA
        }else{
          length(.x) <- history_len #+ proj_len - 1
          .x
        }
      }) %>%
        map_dfr(~{.x}) %>%
        t() %>%
        as_tibble()

      names(dat) <- yrs
      dat <- dat %>%
        mutate(sim_year = seq_len(proj_len)) %>%
        mutate(run = .y) %>%
        select(sim_year, run, everything())
    }) %>%
      map_dfr(~{.x})

    out
  }

  # Outputs by run ------------------------------------------------------------
  out$catch <- get_output_by_run(em, "catch_values")
  out$f <- get_output_by_run(em, "fyear_values")
  out$r <- get_output_by_run(em, "r_values")
  out$ssb <- get_output_by_run(em, "ssb_save")
  out$survey <- get_output_by_run(em, "survey_values")

  get_em_quants <- function(out, type, yrs){
    qbr <- sym(paste0(type, "_quants_by_run"))
    qby <- sym(paste0(type, "_quants_by_year"))
    ret <- NULL
    type <- sym(type)
    proj_yrs <- yrs[(length(yrs) - length(unique(out$sim_year)) + 1):length(yrs)]

    # Quants by run -----------------------------------------------------------
    ret[[qbr]] <- out %>%
      group_by(sim_year) %>%
      group_map(~{
        .x %>%
          quantile(probs = quants, na.rm = TRUE)
      }) %>%
      map_dfr(~{.x}) %>%
      mutate(year = proj_yrs)

    ret[[qbr]] <- ret[[qbr]] %>%
      mutate(scenario = scen_name) %>%
      select(scenario, year, everything())
    nms <- names(ret[[qbr]])
    quant_inds <- grep("%", nms)
    nms[quant_inds] <- gsub("([0-9]+)\\%", "\\1", nms[quant_inds])
    nms[quant_inds] <- as.numeric(nms[quant_inds]) / 100
    names(ret[[qbr]]) <- nms

    # Quants by year ----------------------------------------------------------
    j <- NULL
    for(i in seq_along(unique(out$run))){
      j[[i]] <- out %>%
        filter(run == i)
      yrs_runs <- j[[i]] %>% select(sim_year, run)
      j[[i]] <- j[[i]] %>%
        select(-sim_year, -run) %>%
        map_dfr(~{quantile(.x, probs = quants, na.rm = TRUE)}) %>%
        mutate(year = yrs) %>%
        mutate(run = i) %>%
        select(year, run, everything())
    }

    ret[[qby]] <- j %>%
      map_dfr(~{.x}) %>%
      mutate(scenario = scen_name) %>%
      select(scenario, everything())
    nms <- names(ret[[qby]])
    quant_inds <- grep("%", nms)
    nms[quant_inds] <- gsub("([0-9]+)\\%", "\\1", nms[quant_inds])
    nms[quant_inds] <- as.numeric(nms[quant_inds]) / 100
    names(ret[[qby]]) <- nms

    ret
  }

  # Quantiles by sim_year -----------------------------------------------------
  out <- append(out, get_em_quants(out$catch, "catch", yrs))
  out <- append(out, get_em_quants(out$f, "f", yrs))
  out <- append(out, get_em_quants(out$r, "r", yrs))
  out <- append(out, get_em_quants(out$ssb, "ssb", yrs))
  out <- append(out, get_em_quants(out$survey, "survey", yrs))

  get_em_runmeans <- function(out, type){
    ret <- NULL
    qbym <- sym(paste0(type, "_runmeans"))
    ret[[qbym]] <- out %>%
      select(-scenario) %>%
      group_by(year) %>%
      group_map(~{
        summarize_at(.x, .vars = vars(-run), .funs = mean)
      }) %>%
      map_dfr(~{.x}) %>%
      mutate(scenario = scen_name,
             year = yrs) %>%
      select(scenario, year, everything())

    ret
  }

  # Runmeans of quantiles by sim_year -----------------------------------------
  out <- append(out, get_em_runmeans(out$catch_quants_by_year, "catch_quants_by_year"))
  out <- append(out, get_em_runmeans(out$f_quants_by_year, "f_quants_by_year"))
  out <- append(out, get_em_runmeans(out$r_quants_by_year, "r_quants_by_year"))
  out <- append(out, get_em_runmeans(out$ssb_quants_by_year, "ssb_quants_by_year"))
  out <- append(out, get_em_runmeans(out$survey_quants_by_year, "survey_quants_by_year"))

  prepend_scen_name <- function(d, scen_name){
    d %>%
      mutate(scenario = scen_name) %>%
      select(scenario, everything())
  }
  out$catch <- prepend_scen_name(out$catch, scen_name)
  out$f <- prepend_scen_name(out$f, scen_name)
  out$r <- prepend_scen_name(out$r, scen_name)
  out$ssb <- prepend_scen_name(out$ssb, scen_name)
  out$survey <- prepend_scen_name(out$survey, scen_name)

  out
}