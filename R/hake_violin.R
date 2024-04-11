#' Violin plot of the MSE results
#'
#' @param mse_output MSE output of a single scenario in the format enforced in [setup_mse_plot_objects()]
#' @param om_output OM output of a single scenario in the format enforced in [setup_mse_plot_objects()]
#' @param merged_run_data Objectives output of a single scenario in the format enforced in [merge_run_data()]
#' @param move Logical. If TRUE, stock movement is included
#'
#' @return A [data.frame] with columns SSB, SSB.10, SSB.40, Catch.short,
#' Catch.long, and AAV
#'
#' @export
hake_violin <- function(mse_output,
                        om_output,
                        merged_run_data,
                        move = NA){

  mse_output_run1 <- mse_output[[1]][[1]]
  om_output_run1 <- mse_output[[1]][[1]]
  nruns <- length(mse_output)
  nyears <- mse_output_run1$n_yr
  s_yr <- mse_output_run1$s_yr
  m_yr <- mse_output_run1$m_yr

  # Get the number of years run in that MSE
  simyears <- nyears - (length(s_yr:m_yr)) + 1
  yrs <- mse_output_run1$yrs

  # idx <- 1
  # if(all(is.na(ls.MSE[[idx]]))){
  #   idx <- 2
  # }
  # if(all(is.na(ls.MSE[[idx]]))){
  #   idx <- 3
  # }

  if(is.na(move)){
    ssb_plot <- map2(om_output, seq_along(om_output), ~{
      tmp <- .x$ssb / .x$ssb_0
      tmp %>%
        as_tibble(rownames = "year") %>%
        mutate(run = .y) %>%
        rename(ssb1 = `1`, ssb2 = `2`)
    }) %>%
      map_df(~{.x}) %>%
      as_tibble()
  }else{
    ssb_plot <- merged_run_data$ssb_plot
  }

  if(is.na(move)){
    # Don't know what the output will look like here without movement model
    # catch_plot <- map2(om_output, seq_along(om_output), ~{
    #   tmp <- .x$ssb / .x$ssb_0
    #   tmp %>%
    #     as_tibble(rownames = "year") %>%
    #     mutate(run = .y) %>%
    #     rename(ssb1 = `1`, ssb2 = `2`)
    # }) %>%
    #   map_df(~{.x}) %>%
    #   as_tibble()
    # Catch.plot <- data.frame(Catch = ls.MSE[[idx]]$Catch, year = yr, run = paste('run',1, sep=''))
  }else{
    catch_plot <- merged_run_data$catch_plot
    quota_tot <- merged_run_data$quota_tot
    quota_frac <- merged_run_data$quota_frac
    quota_ca_tot <- merged_run_data$quota_ca_tot
    quota_us_tot <- merged_run_data$quota_us_tot
  }
  aav_plot <- merged_run_data$aav_plot

  ## Probability of S < S10
  SSB.future <- SSB.plot[SSB.plot$year > 2018,]
  #p.export <- grid.arrange(p1,p2,p3)
  rns <- unique(SSB.future$run)
  p.vals <- matrix(0, length(unique(SSB.future$run)))
  for(i in 1:length(unique(SSB.future$run))){
    tmp <- SSB.future[SSB.future$run == rns[i],]
    for(j in 1:(length(tmp$year)-3)){
      if(tmp$SSB[j]< 0.4){
        if(tmp$SSB[j+1]<0.4 & tmp$SSB[j+2] <0.4 & tmp$SSB[j+2]<0.4){
          p.vals[i] <- 1+p.vals[i]
        }
      }
    }
  }
  p.vals <- p.vals/(length(tmp$year)-3)
  ## Calculate the median number of closed years
  nclosed <- rep(NA, length(rns))
  for(i in 1:length(unique(SSB.future$run))){
    tmp <- SSB.future[SSB.future$run == rns[i],]
    nclosed[i] <- length(which(tmp$SSB < 0.1))
  }
  # Create a table with all the objective data
  # Calculate the number of years the quota was met
  SSB <- SSB.plot[SSB.plot$year > 2018,] %>%  # median SSB/SSB0
    group_by(run) %>%
    summarise(value = sum(length(which(SSB > 0.1 & SSB <= 0.4))/simyears)
    )
  SSB$variable <- '0.1 < S < 0.4S0'
  SSB.40 <- SSB.plot[SSB.plot$year > 2018,] %>%  # median SSB/SSB0
    group_by(run) %>%
    summarise(value = sum(length(which(SSB > 0.4))/simyears))
  SSB.40$variable <- 'S > 0.4S0'
  SSB.10 <- SSB.plot[SSB.plot$year > 2018,] %>%  # median SSB/SSB0
    group_by(run) %>%
    summarise(value = sum(length(which(SSB < 0.1))/simyears))
  SSB.10$variable <- 'S < 0.1S0'
  Catch.short  <- Catch.plot[Catch.plot$year > 2018 & Catch.plot$year < 2029,] %>%  # median SSB/SSB0
    group_by(run) %>%
    summarise(value = median(Catch)*1e-6)
  Catch.short$variable <- 'Short term catch'
  Catch.long  <- Catch.plot[Catch.plot$year >= 2029,] %>%  # median SSB/SSB0
    group_by(run) %>%
    summarise(value = median(Catch)*1e-6)
  Catch.long$variable <- 'Long term catch'
  AAV <- AAV.plot[AAV.plot$year > 2018,] %>%  # median SSB/SSB0
    group_by(run) %>%
    summarise(value = median(AAV))
  AAV$variable <- 'AAV'
  df <- rbind(SSB,
              SSB.10,
              SSB.40,
              Catch.short,
              Catch.long,
              AAV)
  # df.obj2$indicator=factor(df.obj2$indicator,
  #                          levels=c('SSB <0.10 SSB0',
  #                                   'S>0.10<0.4S0',
  #                                   'S>0.4S0',
  #                                   'AAV',
  #                                   'short term catch',
  #                                   'long term catch'))
  df
}
