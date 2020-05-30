context("Test the extract_age_comps() function")

ss_agecomps <- tibble::tribble(
  ~Yr, ~FltSvy,  ~a1, ~a2, ~a3, ~a4,  ~a5,
  1995,       2, 20.4, 6.8, 3.2, 2.1, 30.1,
  1998,       2, 21.1, 6.7, 3.6, 241, 33.1,
  2001,       2, 21.4, 3.8, 5.2, 2.8, 19.1,
  1975,       1, 20.4, 6.8, 3.2, 2.1, 30.1,
  1976,       1, 21.1, 6.7, 3.6, 241, 33.1,
  1977,       1, 21.4, 3.8, 5.2, 2.8, 19.1)
ss_model <- NULL
ss_model$dat$agecomp <- ss_agecomps

test_that("extract_age_comps() - Tests for argument errors", {
  expect_error(extract_age_comps(ss_model = NULL,
                                 fleet = 2,
                                 s_yr = 1993,
                                 m_yr = 2002,
                                 fill = -1))
  expect_error(extract_age_comps(ss_model = ss_model,
                                 fleet = NULL,
                                 s_yr = 1993,
                                 m_yr = 2002,
                                 fill = -1))
  expect_error(extract_age_comps(ss_model = ss_model,
                                 fleet = 2,
                                 s_yr = NULL,
                                 m_yr = 2002,
                                 fill = -1))
  expect_error(extract_age_comps(ss_model = ss_model,
                                 fleet = 2,
                                 s_yr = 1993,
                                 m_yr = NULL,
                                 fill = -1))
  expect_error(extract_age_comps(ss_model = ss_model,
                                 fleet = 2,
                                 s_yr = 1993,
                                 m_yr = 2002,
                                 fill = NULL))
  j <- ss_agecomps
  names(j) <- c("Wrong", "FltSvy", "a1", "a2", "a3", "a4", "a5")
  j_model <- NULL
  j_model$dat$agecomp <- j
  expect_error(extract_age_comps(ss_model = j_model,
                                 fleet = 2,
                                 s_yr = 1993,
                                 m_yr = 2002,
                                 fill = -1))

  j <- ss_agecomps
  names(j) <- c("Yr", "Wrong", "a1", "a2", "a3", "a4", "a5")
  j_model <- NULL
  j_model$dat$agecomp <- j
  expect_error(extract_age_comps(ss_model = j_model,
                                 fleet = 2,
                                 s_yr = 1993,
                                 m_yr = 2002,
                                 fill = -1))

  j <- ss_agecomps
  j <- j %>% mutate(FltSvy = ifelse(FltSvy == 2, 3, FltSvy))
  j_model <- NULL
  j_model$dat$agecomp <- j
  expect_error(extract_age_comps(ss_model = j_model,
                                 fleet = 2,
                                 s_yr = 1993,
                                 m_yr = 2002,
                                 fill = -1))
})

test_that("extract_age_comps() - Tests for outputs, simple case", {
  df <- extract_age_comps(ss_model = ss_model,
                          fleet = 2,
                          s_yr = 1993,
                          m_yr = 2002,
                          fill = -1)
  expect_true(nrow(df) == 5)
  expect_true(ncol(df) == 10)
  expect_equivalent(colnames(df), as.character(1993:2002))

  df_na <- df[, as.character(c(1993, 1994, 1996, 1997, 1999, 2000, 2002))]
  expect_true(all(df_na == -1))

  df <- extract_age_comps(ss_model = ss_model,
                          fleet = 2,
                          s_yr = 1993,
                          m_yr = 2002,
                          fill = NA)
  df_na <- df[, as.character(c(1993, 1994, 1996, 1997, 1999, 2000, 2002))]
  expect_true(all(is.na(df_na)))
})
