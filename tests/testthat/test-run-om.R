set.seed(12345)
seed <- floor(runif(n = 1, min = 1, max = 1e6))

ss_model_yr <- 2018
ss_model_output_dir <- file.path(system.file(package = "pacifichakemse", mustWork = TRUE),
                                 "extdata", paste0("SS3_", ss_model_yr))
ss_model_data_csv_dir <- file.path(system.file(package = "pacifichakemse", mustWork = TRUE),
                                   "extdata", "csv-data")

ss_model <- load_ss_model_data(ss_model_output_dir = ss_model_output_dir,
                               ss_model_data_csv_dir = ss_model_data_csv_dir,
                               load_extra_mcmc = FALSE,
                               overwrite_ss_rds = TRUE)

om <- load_data_om(ss_model, yr_future = 50)

if(file.exists("fselvals.csv")){
  unlink("fselvals.csv", force = TRUE)
}
om$selectivity_change <- 0
om_0 <- run_om(om, random_seed = seed, verbose = FALSE, testing = TRUE)

test_that("Selectivity choice 0 is correct", {
  d <- readr::read_csv("fselvals0.csv", col_types = cols()) %>%
    select(everything())
  d_new <- readr::read_csv("fselvals.csv", col_types = cols())
  attr(d_new, c("spec")) <- NULL
  attr(d, c("class")) <- c("spec_tbl_df", attr(d, c("class")))
  expect_equal(d, d_new)


if(file.exists("fselvals.csv")){
  unlink("fselvals.csv", force = TRUE)
}
om$selectivity_change <- 1
om_1 <- run_om(om, random_seed = seed, verbose = FALSE, testing = TRUE)
})

test_that("Selectivity choice 1 is correct", {
  d <- readr::read_csv("fselvals1.csv", col_types = cols()) %>%
    select(everything())
  d_new <- readr::read_csv("fselvals.csv", col_types = cols())
  attr(d_new, c("spec")) <- NULL
  attr(d, c("class")) <- c("spec_tbl_df", attr(d, c("class")))
  expect_equal(d, d_new)
})

if(file.exists("fselvals.csv")){
  unlink("fselvals.csv", force = TRUE)
}
om$selectivity_change <- 2
om_2 <- run_om(om, random_seed = seed, verbose = FALSE, testing = TRUE)
test_that("Selectivity choice 2 is correct", {
  d <- readr::read_csv("fselvals2.csv", col_types = cols()) %>%
    select(everything())
  d_new <- readr::read_csv("fselvals.csv", col_types = cols())
  attr(d_new, c("spec")) <- NULL
  attr(d, c("class")) <- c("spec_tbl_df", attr(d, c("class")))
  expect_equal(d, d_new)
})

if(file.exists("fselvals.csv")){
  unlink("fselvals.csv", force = TRUE)
}

# -----------------------------------------------------------------------------
om0_old <- readRDS("om0_old.rds")
om1_old <- readRDS("om1_old.rds")
om2_old <- readRDS("om2_old.rds")

test_that("SSBs are the same", {
  d <- om_0$ssb %>%
    as_tibble() %>%
    mutate(yr = om_0$yrs) %>%
    rename(ssb.1 = 1, ssb.2 = 2) %>%
    select(yr, everything())
  d1 <- om0_old$SSB
  yrs <- rownames(d1)
  d1 <- d1 %>%
    as_tibble() %>%
    mutate(yr = yrs) %>%
    rename(ssb.1 = 1, ssb.2 = 2) %>%
    select(yr, everything())
  d1$yr <- as.integer(d1$yr)
  expect_equal(d, d1, tolerance = 1e-4)
})

test_that("SSBs by space and season are the same", {
  ssb_all <- om_0$ssb_all
  ssb_all1 <- om0_old$SSB.all
  names(dimnames(ssb_all1))[1] <- "yrs"
  expect_equal(ssb_all, ssb_all1, tolerance = 1e-4)
})

test_that("Age proportions in season 1 are the same", {
  ac <- om_0$age_comps_om[,,,1]
  ac <- apply(ac, c(1, 2), sum) / 2
  ac <- as_tibble(ac)

  ac1 <- om0_old$age_comps_OM[,,,1]
  ac1 <- apply(ac1, c(1, 2), sum) / 2
  ac1 <- as_tibble(ac1)
  expect_equal(ac, ac1, tolerance = 1e-4)
})

test_that("Age proportions in season 2 are the same", {
  ac <- om_0$age_comps_om[,,,2]
  ac <- apply(ac, c(1, 2), sum) / 2
  ac <- as_tibble(ac)

  ac1 <- om0_old$age_comps_OM[,,,2]
  ac1 <- apply(ac1, c(1, 2), sum) / 2
  ac1 <- as_tibble(ac1)

  expect_equal(ac, ac1, tolerance = 1e-4)
})

test_that("Age proportions in season 3 are the same", {
  ac <- om_0$age_comps_om[,,,3]
  ac <- apply(ac, c(1, 2), sum) / 2
  ac <- as_tibble(ac)

  ac1 <- om0_old$age_comps_OM[,,,3]
  ac1 <- apply(ac1, c(1, 2), sum) / 2
  ac1 <- as_tibble(ac1)

  expect_equal(ac, ac1, tolerance = 1e-4)
})

test_that("Age proportions in season 4 are the same", {
  ac <- om_0$age_comps_om[,,,4]
  ac <- apply(ac, c(1, 2), sum) / 2
  ac <- as.data.frame(ac)

  ac1 <- om0_old$age_comps_OM[,,,4]
  ac1 <- apply(ac1, c(1, 2), sum) / 2
  ac1 <- as.data.frame(ac1)

  expect_equal(ac, ac1, tolerance = 1e-4)
})

test_that("Age in catch is the same", {
  age_catch <- om_0$catch_n_save_age
  age_catch1 <- om0_old$CatchN.save.age
  names(dimnames(age_catch1))[1] <- "ages"
  names(dimnames(age_catch1))[2] <- "yrs"

  expect_equal(age_catch, age_catch1, tolerance = 1e-1)
})

test_that("Age in catch with weight-at-age applied is the same", {
  age_catch <- om_0$catch_save_age
  age_catch1 <- om0_old$Catch.save.age
  names(dimnames(age_catch1))[1] <- "ages"
  names(dimnames(age_catch1))[2] <- "yrs"

  expect_equal(age_catch, age_catch1, tolerance = 1e-1)
})

test_that("Numbers at age by season and space are the same", {
  nage <- om_0$n_save_age
  nage1 <- om0_old$N.save.age
  names(dimnames(nage1))[1] <- "ages"
  names(dimnames(nage1))[2] <- "yrs"

  expect_equal(nage, nage1, tolerance = 1e-4)
})

test_that("Recruitment values are the same", {
  r <- om_0$r_save
  r1 <- om_0$r_save

  expect_equal(r, r1)
})

test_that("Vulnerability by season and space are the same", {
  v <- om_0$v_save
  v1 <- om0_old$V.save
  names(dimnames(v1))[1] <- "yrs"

  expect_equal(v, v1, tolerance = 1e-4)
})

# test_that("Catch quota by season and space are the same", {
#   cq <- om_0$catch_quota
#   cq1 <- om0_old$Catch.quota
#   names(dimnames(cq1))[1] <- "yrs"
#
#   expect_equal(cq, cq1)
# })

test_that("Standalone OM run catches n_sim_yrs set", {
  om <- load_data_om(ss_model, random_seed = seed, n_sim_yrs = 5)
  expect_error(run_om(om, random_seed = seed, verbose = FALSE))
})