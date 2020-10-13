context("Test run_om() function, by comparing with the original code's results")

ss_model_yr <- 2018
ss_model_output_dir <- file.path(system.file(package = "pacifichakemse", mustWork = TRUE),
                                 "extdata", paste0("SS3_", ss_model_yr))
ss_model_data_csv_dir <- file.path(system.file(package = "pacifichakemse", mustWork = TRUE),
                                   "extdata", "csv-data")
ss_model_raw <- load_ss_model_from_rds(ss_model_output_dir,
                                       ss_model_data_csv_dir,
                                       load_extra_mcmc = FALSE,
                                       overwrite_ss_rds = TRUE)
ss_model <- load_ss_model_data(ss_model_raw)
om <- load_data_om(ss_model, n_sim_yrs = 5)

if(file.exists("fselvals.csv")){
  unlink("fselvals.csv", force = TRUE)
}
om$selectivity_change <- 0
om_0 <- run_om(om, verbose = FALSE, testing = TRUE)

test_that("Selectivity choice 0 is correct", {
  d <- readr::read_csv("fselvals0.csv", col_types = cols()) %>%
    select(everything())
  d_new <- readr::read_csv("fselvals.csv", col_types = cols()) %>%
    head(-8)
  expect_true(identical(d, d_new))
})

if(file.exists("fselvals.csv")){
  unlink("fselvals.csv", force = TRUE)
}
om$selectivity_change <- 1
om_1 <- run_om(om, verbose = FALSE, testing = TRUE)
test_that("Selectivity choice 1 is correct", {
  d <- readr::read_csv("fselvals1.csv", col_types = cols()) %>%
    select(everything())
  d_new <- readr::read_csv("fselvals.csv", col_types = cols()) %>%
    head(-8)
  expect_true(identical(d, d_new))
})

if(file.exists("fselvals.csv")){
  unlink("fselvals.csv", force = TRUE)
}
om$selectivity_change <- 2
om_2 <- run_om(om, verbose = FALSE, testing = TRUE)
test_that("Selectivity choice 2 is correct", {
  d <- readr::read_csv("fselvals2.csv", col_types = cols()) %>%
    select(everything())
  d_new <- readr::read_csv("fselvals.csv", col_types = cols()) %>%
    head(-8)
  expect_true(identical(d, d_new))
})

if(file.exists("fselvals.csv")){
  unlink("fselvals.csv", force = TRUE)
}

# -----------------------------------------------------------------------------

test_that("SSBs are the same", {
  d <- data.frame(yr = om_0$yrs,
                  ssb = rowSums(om_0$ssb))
  d1 <- readRDS("ssb_out.rds")
  expect_equivalent(d, d1, tolerance = 1e-9)
})

test_that("Age proportions in season 1 are the same", {
  nyr <- length(om_0$yrs)
  ac <- om_0$age_comps_om[,,,1]
  ac <- apply(ac, c(1, 2), sum) / 2
  ac <- as_tibble(ac)
  ac1 <- readRDS("ac_seas1_out.rds")
  ac1 <- apply(ac1, c(1, 2), sum) / 2
  ac1 <- as_tibble(ac1)

  expect_equivalent(ac, ac1)
})

test_that("Age proportions in season 2 are the same", {
  nyr <- length(om_0$yrs)
  ac <- om_0$age_comps_om[,,,2]
  ac <- apply(ac, c(1, 2), sum) / 2
  ac <- as_tibble(ac)
  ac1 <- readRDS("ac_seas2_out.rds")
  ac1 <- apply(ac1, c(1, 2), sum) / 2
  ac1 <- as_tibble(ac1)

  expect_equivalent(ac, ac1, tolerance = 1e-1)
})

test_that("Age proportions in season 3 are the same", {
  nyr <- length(om_0$yrs)
  ac <- om_0$age_comps_om[,,,3]
  ac <- apply(ac, c(1, 2), sum) / 2
  ac <- as_tibble(ac)
  ac1 <- readRDS("ac_seas3_out.rds")
  ac1 <- apply(ac1, c(1, 2), sum) / 2
  ac1 <- as_tibble(ac1)

  expect_equivalent(ac, ac1)
})

test_that("Age proportions in season 4 are the same", {
  nyr <- length(om_0$yrs)
  ac <- om_0$age_comps_om[,,,4]
  ac <- apply(ac, c(1, 2), sum) / 2
  ac <- as.data.frame(ac)
  ac1 <- readRDS("ac_seas4_out.rds")
  ac1 <- apply(ac1, c(1, 2), sum) / 2
  ac1 <- as.data.frame(ac1)

  expect_equal(ac, ac1)
})

