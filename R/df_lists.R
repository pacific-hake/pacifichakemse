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

  lst_df <- map2(lst, seq_along(lst), ~{
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
                 ssb_us = .x$SSB[,2],
                 ssbtot =  .x$SSB[,1] + .x$SSB[,2],
                 f0_can = .x$F0[,1],
                 f0_us = .x$F0[,2],
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

  allruns_yrs <- rep(yrs, length(lst))
  ssb_can <- conv_vec_to_mse_df(lst_df$ssb_can,
                                col = "ssb",
                                yr_vec = allruns_yrs,
                                country = "Canada",
                                probs = quants)
  ssb_us <- conv_vec_to_mse_df(lst_df$ssb_us,
                               col = "ssb",
                               yr_vec = allruns_yrs,
                               country = "US",
                               probs = quants)
  ssb_quant <- ssb_can %>%
    bind_rows(ssb_us) %>%
    mutate(run = plotname)

  ssb_tot_quant <- conv_vec_to_mse_df(lst_df$ssbtot,
                                      col = "ssb",
                                      yr_vec = allruns_yrs,
                                      probs = quants) %>%
    mutate(run = plotname)

  ssb_mid_can <- conv_vec_to_mse_df(lst_df$ssb_mid_can,
                                    col = "ssb",
                                    yr_vec = allruns_yrs,
                                    country = "Canada",
                                    probs = quants)
  ssb_mid_us <- conv_vec_to_mse_df(lst_df$ssb_mid_us,
                                   col = "ssb",
                                   yr_vec = allruns_yrs,
                                   country = "US",
                                   probs = quants)
  ssb_mid_quant <- ssb_mid_can %>%
    bind_rows(ssb_mid_us) %>%
    mutate(run = plotname)

  catch_quant <- conv_vec_to_mse_df(lst_df$catch,
                                    col = "catch",
                                    yr_vec = allruns_yrs,
                                    probs = quants) %>%
    mutate(run = plotname)

  ams_quant <- conv_vec_to_mse_df(lst_df$ams,
                                  col = "ams",
                                  yr_vec = allruns_yrs,
                                  probs = quants) %>%
    mutate(run = plotname)

  amc_quant <- conv_vec_to_mse_df(lst_df$amc,
                                  col = "amc",
                                  yr_vec = allruns_yrs,
                                  probs = quants) %>%
    mutate(run = plotname)

  ams_space_can <- conv_vec_to_mse_df(lst_df$ams_can,
                                      col = "ams",
                                      yr_vec = allruns_yrs,
                                      country = "Canada",
                                      probs = quants)
  ams_space_us <- conv_vec_to_mse_df(lst_df$ams_us,
                                     col = "ams",
                                     yr_vec = allruns_yrs,
                                     country = "US",
                                     probs = quants)
  ams_space_quant <- ams_space_can %>%
    bind_rows(ams_space_us) %>%
    mutate(run = plotname)

  amc_space_can <- conv_vec_to_mse_df(lst_df$amc_can,
                                      col = "ams",
                                      yr_vec = allruns_yrs,
                                      country = "Canada",
                                      probs = quants)
  amc_space_us <- conv_vec_to_mse_df(lst_df$amc_us,
                                     col = "ams",
                                     yr_vec = allruns_yrs,
                                     country = "US",
                                     probs = quants)
  amc_space_quant <- amc_space_can %>%
    bind_rows(amc_space_us) %>%
    mutate(run = plotname)

  f0_can <- conv_vec_to_mse_df(lst_df$f0_can,
                               col = "F0",
                               yr_vec = allruns_yrs,
                               country = "Canada",
                               probs = quants)
  f0_us <- conv_vec_to_mse_df(lst_df$f0_us,
                              col = "F0",
                              yr_vec = allruns_yrs,
                              country = "US",
                              probs = quants)
  f0_quant <- f0_can %>%
    bind_rows(f0_us) %>%
    mutate(run = plotname)

  catch_q_can <- conv_vec_to_mse_df(lst_df$catch_can / lst_df$catch_q_can,
                                    col = "catch",
                                    yr_vec = allruns_yrs,
                                    country = "Canada",
                                    probs = quants)
  catch_q_us <- conv_vec_to_mse_df(lst_df$catch_us / lst_df$catch_q_us,
                                   col = "catch",
                                   yr_vec = allruns_yrs,
                                   country = "Canada",
                                   probs = quants)
  catch_q_quant <- catch_q_can %>%
    bind_rows(catch_q_us) %>%
    mutate(run = plotname)

  list(lst_df,
       nfailed,
       list(ssb_quant = ssb_quant,
            ssb_mid_quant = ssb_mid_quant,
            ssb_tot_quant = ssb_tot_quant,
            catch_quant = catch_quant,
            amc_quant = amc_quant,
            ams_quant = ams_quant,
            amc_space_quant = amc_space_quant,
            ams_space_quant = ams_space_quant,
            f0_quant = f0_quant,
            catch_q_quant = catch_q_quant))
}
