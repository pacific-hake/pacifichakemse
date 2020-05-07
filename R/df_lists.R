#' Create a list of [data.frame]s containing key values of interest output from the
#' MSE run provided
#'
#' @details Create a list of 3 lists of data frames which contain various values
#' of interest such as SSB by country, SSB total, F by country, Catch
#' by country, q by country and other data frame values used for plotting, containing
#' quantile information and run (scenario) name
#'
#' @param lst list of MSE data as read in from an MSE data file (.rds) using [base::readRDS()]
#' @param max_yr The last year before simulations start
#' @param can.prop The proportion of TAC that Canada gets each year
#' @param us.prop The proportion of TAC that the US gets each year
#'
#' @return A list of 3 lists of [data.frame]s containing key values of interest output from the
#' MSE run provided.
#' @export
df_lists <- function(lst = NULL,
                     max_yr = 2018,
                     can.prop = 0.2488,
                     us.prop = 0.7612,
                     quants = c(0.05, 0.25, 0.5, 0.75, 0.95)){

  stopifnot(!is.null(lst))

  yrs <- as.numeric(attributes(lst[[1]]$Catch)$dimnames$year)
  min_yr <- min(yrs)
  nyrs <- length(yrs)
  if(nyrs == 1){
    nyrs <- nrow(lst[[1]]$Catch)
  }
  simyears <- nyrs - (length(min_yr:max_yr)) + 1
  nruns <- length(lst)
  nfailed <- rep(1, nruns)

  ls.df <- map2(lst, seq_along(lst), ~{
    if(length(.x) > 1){
      if(nyrs == 1){
        catch_us <- .x$Catch * us_prop
        catch_can <- .x$Catch * can_prop
        catch <- .x$Catch
        catch_q_us <- rowSums(.x$Catch.quota[,2,])
        catch_q_can <- rowSums(.x$Catch.quota[,1,])
        catch_q <- rowSums(.x$Catch.quota)
      }else{
        catchtmp <- apply(.x$Catch, MARGIN = c(2, 3), FUN = sum)
        catch <- rowSums(catchtmp)
        catch_us <- catchtmp[,2]
        catch_can <- catchtmp[,1]
        catch_q <- rowSums(.x$Catch.quota)
        catch_q_us <- rowSums(.x$Catch.quota[,2,])
        catch_q_can <- rowSums(.x$Catch.quota[,1,])
      }
      data.frame(year = yrs,
                 ssb_can = .x$SSB[,1],
                 ssb_US = .x$SSB[,2],
                 ssbtot =  .x$SSB[,1] + .x$SSB[,2],
                 F0_can = .x$F0[,1],
                 F0_us = .x$F0[,2],
                 amc = .x$amc$amc.tot,
                 ams = .x$ams$ams.tot,
                 amc_can = .x$amc$amc.can,
                 amc_us = .x$amc$amc.US,
                 ams_can = .x$ams$ams.can,
                 ams_us = .x$ams$ams.US,
                 ssb_mid_can = .x$SSB.mid[,1],
                 ssb_mid_us = .x$SSB.mid[,2],
                 run = .y,
                 catch_q = catch_q,
                 catch_q_us = catch_q_us,
                 catch_q_can = catch_q_can,
                 catch = catch,
                 catch_us = catch_us,
                 catch_can = catch_can,
                 f0_us = catch_us/.x$SSB.mid[,2],
                 f0_can = catch_can/.x$SSB.mid[,1])
    }else{
      NA
    }
  }) %>% map_df(~{.x})

  plotname <- attributes(lst)$plotname

  SSB.plotquant <- ls.df[ls.df$year > 2010,] %>%
    group_by(year) %>%
    summarise(med.can = median(ssb_can),
              p95.can = quantile(ssb_can, 0.95),
              p5.can = quantile(ssb_can,0.05),
              med.US = median(ssb_US),
              p95.US = quantile(ssb_US, 0.95),
              p5.US = quantile(ssb_US,0.05))
  SSB.plotquant$run <- plotname

  SSB.plottot <- ls.df[ls.df$year > 2010,] %>%
    group_by(year) %>%
    summarise(med = median(ssbtot),
              avg = mean(ssbtot),
              p95 = quantile(ssbtot, 0.95),
              p5 = quantile(ssbtot,0.05))
  SSB.plottot$run <- plotname

  SSB.plotmid <- ls.df[ls.df$year > 2010,] %>%
    group_by(year) %>%
    summarise(med.can = median(ssb_mid_can),
              p95.can = quantile(ssb_mid_can, 0.95),
              p5.can = quantile(ssb_mid_can,0.05),
              med.US = median(ssb_mid_us),
              p95.US = quantile(ssb_mid_us, 0.95),
              p5.US = quantile(ssb_mid_us,0.05))
  SSB.plotmid$run <- plotname

  Catch.plotquant <- ls.df[ls.df$year > 2010,] %>%
    group_by(year) %>%
    summarise(med = median(catch),
              p95 = quantile(catch, 0.95),
              p5 = quantile(catch,0.05)
    )
  Catch.plotquant$run <- plotname
  #
  ams.plotquant <- ls.df[ls.df$year > 2010,] %>%
    group_by(year) %>%
    summarise(med = median(ams,na.rm = TRUE),
              p95 = quantile(ams, 0.95,na.rm = TRUE),
              p5 = quantile(ams,0.05,na.rm = TRUE)
    )
  ams.plotquant$run <- plotname

  amc.plotquant <- ls.df[ls.df$year > 2010,] %>%
    group_by(year) %>%
    summarise(med= median(amc,na.rm = TRUE),
              p95 = quantile(amc, 0.95,na.rm = TRUE),
              p5 = quantile(amc,0.05,na.rm = TRUE)
    )
  amc.plotquant$run <- plotname

  ams.space <- ls.df[ls.df$year > 2010,] %>%
    group_by(year) %>%
    summarise(med.can = median(ams_can,na.rm = TRUE),
              p95.can = quantile(ams_can, 0.95,na.rm = TRUE),
              p5.can = quantile(ams_can,0.05,na.rm = TRUE),
              med.us = median(ams_us,na.rm = TRUE),
              p95.us = quantile(ams_us, 0.95,na.rm = TRUE),
              p5.us = quantile(ams_us,0.05,na.rm = TRUE)

    )
  ams.space$run <- plotname

  amc.space <- ls.df[ls.df$year > 2010,] %>%
    group_by(year) %>%
    summarise(med.can = median(amc_can,na.rm = TRUE),
              p95.can = quantile(amc_can, 0.95,na.rm = TRUE),
              p5.can = quantile(amc_can,0.05,na.rm = TRUE),
              med.us = median(amc_us,na.rm = TRUE),
              p95.us = quantile(amc_us, 0.95,na.rm = TRUE),
              p5.us = quantile(amc_us,0.05,na.rm = TRUE)

    )
  amc.space$run <- plotname

  F0.space <- ls.df[ls.df$year > 2010,] %>%
    group_by(year) %>%
    summarise(med.can = median(f0_can,na.rm = TRUE),
              p95.can = quantile(f0_can, 0.95,na.rm = TRUE),
              p5.can = quantile(f0_can,0.05,na.rm = TRUE),
              med.us = median(f0_us,na.rm = TRUE),
              p95.us = quantile(f0_us, 0.95,na.rm = TRUE),
              p5.us = quantile(f0_us,0.05,na.rm = TRUE)

    )
  F0.space$run <- plotname

  Catch.q <- ls.df[ls.df$year > 2010,] %>%
    group_by(year) %>%
    summarise(med.can = mean(catch_can/catch_q_can,na.rm = TRUE),
              p95.can = quantile(catch_can/catch_q_can, 0.95,na.rm = TRUE),
              p5.can = quantile(catch_can/catch_q_can,0.05,na.rm = TRUE),
              med.us = mean(catch_us/catch_q_us,na.rm = TRUE),
              p95.us = quantile(catch_us/catch_q_us, 0.95,na.rm = TRUE),
              p5.us = quantile(catch_us/catch_q_us,0.05,na.rm = TRUE),
              med.tot = mean(catch/catch_q),
              p5.tot = quantile(catch/catch_q,0.05,na.rm = TRUE),
              p95.tot = quantile(catch/catch_q,0.95,na.rm = TRUE)

    )
  Catch.q$run <- plotname

  ggplot(Catch.q, aes(x = year, y = med.can)) +
    geom_line() +
    coord_cartesian(ylim = c(0.2, 1.3)) +
    geom_line(aes(y = med.us)) +
    geom_line(aes(y = p5.can)) +
    geom_line(aes(y = p95.can), col = 'red') +
    geom_line(aes(y=  p95.us), col ='blue') +
    geom_line(aes(y = p5.us), col = 'blue')

  list(ls.df,
       nfailed,
       list(SSBplot = SSB.plotquant,
            SSBmid = SSB.plotmid,
            SSBtot = SSB.plottot,
            Catchplot = Catch.plotquant,
            amcplot = amc.plotquant,
            amsplot = ams.plotquant,
            amc.space = amc.space,
            ams.space = ams.space,
            F0 =F0.space,
            Catch.q = Catch.q))
}
