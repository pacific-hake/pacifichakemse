context("Test the extract_age_comps() function")

ss_agecomps <- tibble::tribble(
  ~Yr,   ~Fleet,  ~Bin,    ~Obs,
  1995,        2,     1,   20.4,
  1995,        2,     2,    6.8,
  1995,        2,     3,    3.2,
  1995,        2,     4,    2.1,
  1995,        2,     5,   30.1,
  1998,        2,     1,   21.1,
  1998,        2,     2,    6.7,
  1998,        2,     3,    3.6,
  1998,        2,     4,   22.4,
  1998,        2,     5,   33.1,
  2001,        2,     1,   21.4,
  2001,        2,     2,    3.8,
  2001,        2,     3,    5.2,
  2001,        2,     4,    2.8,
  2001,        2,     5,   19.1,
  1975,        1,     1,   20.4,
  1975,        1,     2,    6.8,
  1975,        1,     3,    3.2,
  1975,        1,     4,    2.1,
  1975,        1,     5,   30.1,
  1976,        1,     1,   21.1,
  1976,        1,     2,    6.7,
  1976,        1,     3,    3.6,
  1976,        1,     4,   24.1,
  1976,        1,     5,   33.1,
  1977,        1,     1,   21.4,
  1977,        1,     2,    3.8,
  1977,        1,     3,    5.2,
  1977,        1,     4,    2.8,
  1977,        1,     5,   19.1)

ss_model <- NULL
ss_model$agedbase <- ss_agecomps

test_that("extract_age_comps() - Tests for argument errors", {
  expect_error(extract_age_comps(ss_model = NULL,
                                 age_comps_fleet = 2,
                                 s_yr = 1993,
                                 m_yr = 2002,
                                 age_comps_fill = -1))
  expect_error(extract_age_comps(ss_model = ss_model,
                                 age_comps_fleet = NULL,
                                 s_yr = 1993,
                                 m_yr = 2002,
                                 age_comps_fill = -1))
  expect_error(extract_age_comps(ss_model = ss_model,
                                 age_comps_fleet = 2,
                                 s_yr = NULL,
                                 m_yr = 2002,
                                 age_comps_fill = -1))
  expect_error(extract_age_comps(ss_model = ss_model,
                                 age_comps_fleet = 2,
                                 s_yr = 1993,
                                 m_yr = NULL,
                                 age_comps_fill = -1))
  expect_error(extract_age_comps(ss_model = ss_model,
                                 age_comps_fleet = 2,
                                 s_yr = 1993,
                                 m_yr = 2002,
                                 age_comps_fill = NULL))
  j <- ss_agecomps
  names(j) <- c("Year", "Fleet", "Bin", "Obs")
  j_model <- NULL
  j_model$agedbase <- j
  expect_error(extract_age_comps(ss_model = j_model,
                                 age_comps_fleet = 2,
                                 s_yr = 1993,
                                 m_yr = 2002,
                                 age_comps_fill = -1))

  j <- ss_agecomps
  names(j) <- c("Yr", "Wrong", "Bin", "Obs")
  j_model <- NULL
  j_model$agedbase <- j
  expect_error(extract_age_comps(ss_model = j_model,
                                 age_comps_fleet = 2,
                                 s_yr = 1993,
                                 m_yr = 2002,
                                 age_comps_fill = -1))

  j <- ss_agecomps
  j <- j %>% mutate(Fleet = ifelse(Fleet == 2, 3, Fleet))
  j_model <- NULL
  j_model$agedbase <- j
  expect_error(extract_age_comps(ss_model = j_model,
                                 age_comps_fleet = 2,
                                 s_yr = 1993,
                                 m_yr = 2002,
                                 age_comps_fill = -1))
})

test_that("extract_age_comps() - Tests for outputs, simple case", {
  df <- extract_age_comps(ss_model = ss_model,
                          age_comps_fleet = 2,
                          s_yr = 1993,
                          m_yr = 2002,
                          age_comps_fill = -1)
  expect_true(nrow(df) == 5)
  expect_true(ncol(df) == 10)
  expect_equivalent(colnames(df), as.character(1993:2002))

  df_na <- df[, as.character(c(1993, 1994, 1996, 1997, 1999, 2000, 2002))]
  expect_true(all(df_na == -1))

  df <- extract_age_comps(ss_model = ss_model,
                          age_comps_fleet = 2,
                          s_yr = 1993,
                          m_yr = 2002,
                          age_comps_fill = NA)
  df_na <- df[, as.character(c(1993, 1994, 1996, 1997, 1999, 2000, 2002))]
  expect_true(all(is.na(df_na)))
})
