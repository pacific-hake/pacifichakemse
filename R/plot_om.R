#' Plot a comparison biomass (SSB) from the OM and the old OM, which is read in as an rds object
#'
#' @param om Output from the [run_om()] function
#' @param which_om Which number OM to use. These correspond to selectivity change values
#' @param fn Location of the om_output.rds file from the old code"s base OM run
#'
#' @return a [ggplot2::ggplot()] object
#' @export
compare_biomass <- function(om,
                            which_om = 0,
                            fn = paste0("C:/github/pacific-hake/pacifichakemse/tests/testthat/om", which_om, "_old.rds")){

  if(!file.exists(fn)){
    stop("File ", fn, " does not exist.", call. = FALSE)
  }
  d <- data.frame(yr = om$yrs,
                  new_code = rowSums(om$ssb))
  om1 <- readRDS(fn)
  d1 <- data.frame(yr = om$yrs,
                   old_code = rowSums(om1$SSB))
  dd <- d %>% left_join(d1, by = "yr") %>% as_tibble()
  ddd <- melt(dd, id = "yr")

  p <- ggplot(data = ddd, aes(x = yr, y = value, color = variable)) +
    geom_line(size = 2)

  p
}

#' Plot a comparison between age in catch for the OM and the old OM, which is read in as an rds object
#'
#' @param om Output from the [run_om()] function
#' @param country If NULL, a sum of both countries, if "ca" data for Canada and if "us", data for the US
#' @param season Numeric 1 through 4
#' @param which_om Which number OM to use. These correspond to selectivity change values
#' @param fn Location of the om_output.rds file from the old code"s base OM run
#'
#' @return a [ggplot2::ggplot()] object
#' @export
compare_age_comps <- function(om,
                              country = NULL,
                              season = 3,
                              which_om = 0,
                              fn = paste0("C:/github/pacific-hake/pacifichakemse/tests/testthat/om", which_om, "_old.rds")){

  if(!file.exists(fn)){
    stop("File ", fn, " does not exist.", call. = FALSE)
  }
  om1 <- readRDS(fn)

  nyr <- om$yrs %>% length
  if(is.null(country)){
    ac <- om$age_comps_om[, , , season]
    ac <- apply(ac, c(1, 2), sum) / 2
    ac1 <- om1$age_comps_OM[, , , season]
    ac1 <- apply(ac1, c(1, 2), sum) / 2
  }else if(country == "ca"){
    ac <- om$age_comps_om[,,1,season]
    ac1 <- om1$age_comps_OM[, , 1, season]
  }else if(country == "us"){
    ac <- om$age_comps_om[,,2,season]
    ac1 <- om1$age_comps_OM[, , 2, season]
  }else{
    stop("Country is not correct", call. = FALSE)
  }
  ac <- as_tibble(ac)
  ac1 <- as_tibble(ac1)

  am <- map_dbl(ac, ~{
    sum(.x * om$ages)
  })
  am1 <- map_dbl(ac1, ~{
    sum(.x * om$ages)
  })
  aa <- am1 %>% bind_cols(am1) %>% mutate(yr = om$yrs) %>%
    rename(new_om = 1, orig_om = 2)
  a <- melt(aa, id.var = "yr")

  p <- ggplot(data = a, aes(x = yr, y = value, color = variable)) +
    geom_line(size = 2)

  p
}

#' Plot the average age in catch for the OM
#'
#' @param obj One of the OM list elements "catch_save_age" or "catch_n_save_age"
#'
#' @return a [ggplot2::ggplot()] object
#' @export
compare_age_in_catch <- function(obj){
  age_catch <- obj
  age_catch_all <- apply(age_catch, c(1, 2), mean)
  am_catch <- matrix(NA, om$n_yr)
  for(i in 1:(om$n_yr - 1)){
    am_tmp <- rep(NA, 15)
    am_tmp[1:14] <- age_catch_all[2:15, i] / sum(age_catch_all[, i])
    am_tmp[15] <- sum(age_catch_all[16:om$n_age, i]) / sum(age_catch_all[,i])
    am_catch[i] <- sum(om$ages[2:16] * am_tmp)
  }

  age_catch_can <- apply(obj[, , 1, ], c(1, 2), mean)
  am_catch_can <- matrix(NA, om$n_yr)
  for(i in 1:(om$n_yr - 1)){
    am_tmp <- rep(NA, 15)
    am_tmp[1:14] <- age_catch_can[2:15, i] / sum(age_catch_can[, i])
    am_tmp[15] <- sum(age_catch_can[16:om$n_age, i]) / sum(age_catch_can[,i])
    am_catch_can[i] <- sum(om$ages[2:16] * am_tmp)
  }

  age_catch_us <- apply(obj[, , 2, ], c(1, 2), mean)
  am_catch_us <- matrix(NA, om$n_yr)
  for(i in 1:(om$n_yr - 1)){
    am_tmp <- rep(NA, 15)
    am_tmp[1:14] <- age_catch_us[2:15, i] / sum(age_catch_us[, i])
    am_tmp[15] <- sum(age_catch_us[16:om$n_age, i]) / sum(age_catch_us[,i])
    am_catch_us[i] <- sum(om$ages[2:16] * am_tmp)
  }

  df_am_catch <- data.frame(year = rep(om$yrs,3),
                            am = c(am_catch, am_catch_can, am_catch_us),
                            Country = rep(c("All","Can","US"), each = length(om$yrs)))


  p <- ggplot(df_am_catch, aes(x = year, y = am, color = Country)) +
    geom_line(size = 1) +
    scale_y_continuous(name = "Average age in catch", limits = c(2,10)) +
    scale_x_continuous()

  p
}

#' Plot movement into each country by season. To use:
#' p <- compare_move(om)
#' do.call("grid.arrange", c(p, ncol = 2))
#'
#' @param om Output from the [run_om()] function
#'
#' @return a list of 4 [ggplot2::ggplot()] objects
#' @export
#' @importFrom ggplot2 ggtitle
compare_move <- function(om){

  d <- data.frame(age = rep(om$ages, 8), movement = NA, country = rep(c("CAN","USA"), each = om$n_age),
                  season = rep(1:4, each = om$n_age * 2))

  d$age[d$country == "CAN"] <-  d$age[d$country == "CAN"] + 0.3 # For plotting

  for(i in 1:4){
    mm.tmp <- om$move_mat[,,i,1]
    d[d$season == i & d$country == "USA",]$movement <- mm.tmp[2,]
    d[d$season == i & d$country == "CAN",]$movement <- mm.tmp[1,]
  }
  p <- list()
  d$country <- as.factor(d$country)
  for(i in 1:4){
    p[[i]] <- ggplot(d[d$season == i,], aes(x = age, y = movement, color = country)) +
      geom_line() +
      scale_y_continuous(limits = c(0, 1)) +
      geom_point() +
      ggtitle(paste("season", i))
  }
  p
}

#' Plot a 3-pane selectivity plot by country for three scenarios:
#' Base, Low US, and last year selectivity
#'
#' @param om_0 Operating model output from [run_om()]
#' @param om_1 Operating model output from [run_om()]
#' @param om_2 Operating model output from [run_om()]
#' @param type "new or "old"
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_selectivity_scenarios <- function(om_0, om_1, om_2, type = "new"){
  # Check the three OMs - final year selectivity at age
  offset <- 0 # How many years prior to the final year
  yr_ind <- dim(om_0$f_sel_save)[2] - offset
  yr <- om_0$m_yr - offset
  d <- data.frame(sel = c(om_0$f_sel_save[,yr_ind,1], om_1$f_sel_save[,yr_ind,1], om_2$f_sel_save[,yr_ind,1],
                          om_0$f_sel_save[,yr_ind,2], om_1$f_sel_save[,yr_ind,2], om_2$f_sel_save[,yr_ind,2]),
                  country = rep(c("CAN", "USA"), each = 3 * om_0$n_age),
                  age = rep(om_0$ages, 6),
                  run = rep(rep(c("Base scenario",
                                  "Low US selectivity",
                                  paste0(yr, " selectivity")),
                                each = om_0$n_age), 2)) %>%
    as_tibble()
  d$run <- factor(d$run, levels = c("Base scenario",
                                    "Low US selectivity",
                                    paste0(yr, " selectivity")))

  p <- ggplot(data = d,
              aes(x = age, y = sel, color = country)) +
    theme_classic() +
    geom_line(size = 1.2) +
    facet_wrap(~run) +
    scale_x_continuous(limits = c(0, om_0$age_max_age)) +
    scale_color_manual(values = c("darkred","blue4"))

  if(type == "old"){
    p_old <- file.path(system.file(package = "pacifichakemse", mustWork = TRUE),
                       "extdata", "selectivity_scenarios_old.rds")
    p <- readRDS(p_old)
  }

  p
}