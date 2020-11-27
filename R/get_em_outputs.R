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

  # SSB by run ----------------------------------------------------------------
  out$ssb <- map2(em, seq_along(em), ~{
    names(.x$ssb_save) <- as.character(seq_along(.x$ssb_save))
    history_len <- length(.x$ssb_save[[1]])
    proj_len <- length(.x$ssb_save)
    ssb <- map(.x$ssb_save, ~{
      length(.x) <- history_len + proj_len - 1
      .x
    }) %>%
      map_dfr(~{.x}) %>%
      t() %>%
      as_tibble()

    names(ssb) <- yrs

    ssb <- ssb %>%
      mutate(sim_year = seq_len(proj_len)) %>%
      mutate(run = .y) %>%
      select(sim_year, run, everything())
  }) %>%
    map_dfr(~{.x})

  # SSB quants ----------------------------------------------------------------
  proj_yrs <- yrs[(length(yrs) - length(unique(out$ssb$sim_year)) + 1):length(yrs)]
  out$ssb_quants_by_run <- out$ssb %>%
    group_by(sim_year) %>%
    group_map(~{
      .x %>%
        quantile(probs = quants, na.rm = TRUE)
    }) %>%
    map_dfr(~{.x}) %>%
    mutate(year = proj_yrs)

  j <- NULL
  for(i in seq_along(unique(out$ssb$run))){
    j[[i]] <- out$ssb %>%
      filter(run == i)
    yrs_runs <- j[[i]] %>% select(sim_year, run)
    j[[i]] <- j[[i]] %>%
      select(-sim_year, -run) %>%
      map_dfr(~{quantile(.x, probs = quants, na.rm = TRUE)}) %>%
      mutate(year = yrs) %>%
      mutate(run = i) %>%
      select(year, run, everything())
  }

  out$ssb_quants_by_year <- j %>%
    map_dfr(~{.x}) %>%
    mutate(scenario = scen_name) %>%
    select(scenario, everything())
  nms <- names(out$ssb_quants_by_year)
  quant_inds <- grep("%", nms)
  nms[quant_inds] <- gsub("([0-9]+)\\%", "\\1", nms[quant_inds])
  nms[quant_inds] <- as.numeric(nms[quant_inds]) / 100
  names(out$ssb_quants_by_year) <- nms

  out$ssb_quants_by_run <- out$ssb_quants_by_run %>%
    mutate(scenario = scen_name) %>%
    select(scenario, year, everything())
  nms <- names(out$ssb_quants_by_run)
  quant_inds <- grep("%", nms)
  nms[quant_inds] <- gsub("([0-9]+)\\%", "\\1", nms[quant_inds])
  nms[quant_inds] <- as.numeric(nms[quant_inds]) / 100
  names(out$ssb_quants_by_run) <- nms

  out$ssb <- out$ssb %>%
    mutate(scenario = scen_name) %>%
    select(scenario, everything())

  out
}