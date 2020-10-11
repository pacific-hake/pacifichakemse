context("Test run_om() function")

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

