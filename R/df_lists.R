#' Create a list of [data.frame]s containing key values of interest output from the
#' MSE run provided
#'
#' @details Create a list of 3 lists of data frames which contain various values
#' of interest such as SSB by country, SSB total, F by country, Catch
#' by country, q by country and other data frame values used for plotting, containing
#' quantile information and run (scenario) name
#'
#' @param lst list of MSE data as read in using [readRDS("MSE_output.rds")]
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
                     us.prop = 0.7612){

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

  # lst_out <- map(lst, ~{
  #   map(.x, ~{
  #     if(length(.x) > 1){
  #
  #     }else{
  #       NA
  #     }
  #   })
  # })

  for(i in 1:nruns){
    if(length(lst[[i]]) > 1){

      if(i == 1){

        if(dim(lst[[1]]$Catch)[2] == 1){
          Catch.us <- lst[[i]]$Catch * us.prop
          Catch.can <- lst[[i]]$Catch * can.prop
          Catch = lst[[i]]$Catch

          # Allocated catch
          Catch.q.us <- rowSums(lst[[i]]$Catch.quota[,2,])
          Catch.q.can <- rowSums(lst[[i]]$Catch.quota[,1,])
          Catch.q = rowSums(lst[[i]]$Catch.quota)
        }else{

          catchtmp <- apply(lst[[i]]$Catch, MARGIN = c(2,3), FUN = sum)

          Catch = rowSums(catchtmp)
          Catch.us = catchtmp[,2]
          Catch.can = catchtmp[,1]

          Catch.q = rowSums(lst[[i]]$Catch.quota)
          Catch.q.us = rowSums(lst[[i]]$Catch.quota[,2,])
          Catch.q.can = rowSums(lst[[i]]$Catch.quota[,1,])

        }

        if(is.null(lst[[i]]$F0)){
          lst[[i]]$F0 <- matrix(NA, length(yr),2)
        }

        ls.df <- data.frame(year = yrs,
                            SSB.can = lst[[i]]$SSB[,1],
                            SSB.US = lst[[i]]$SSB[,2],
                            SSBtot =  lst[[i]]$SSB[,1]+lst[[i]]$SSB[,2],
                            F0.can = lst[[i]]$F0[,1],
                            F0.us = lst[[i]]$F0[,2],
                            amc = lst[[i]]$amc$amc.tot,
                            ams = lst[[i]]$ams$ams.tot,
                            amc.can = lst[[i]]$amc$amc.can,
                            amc.us = lst[[i]]$amc$amc.US,
                            ams.can = lst[[i]]$ams$ams.can,
                            ams.us = lst[[i]]$ams$ams.US,
                            SSB.mid.can = lst[[i]]$SSB.mid[,1],
                            SSB.mid.us = lst[[i]]$SSB.mid[,2],
                            run = paste('run',i, sep = '-'),
                            Catch.q = Catch.q,
                            Catch.q.us = Catch.q.us,
                            Catch.q.can = Catch.q.can,
                            Catch = Catch,
                            Catch.us = Catch.us,
                            Catch.can = Catch.can,
                            F0.us = Catch.us/lst[[i]]$SSB.mid[,2],
                            F0.can = Catch.can/lst[[i]]$SSB.mid[,1])

      }else{

        if(dim(lst[[i]]$Catch)[2] == 1){
          Catch.q.us <- lst[[i]]$Catch * us.prop
          Catch.q.can <- lst[[i]]$Catch * can.prop
          Catch = lst[[i]]$Catch

        }else{

          catchtmp <- apply(lst[[i]]$Catch, MARGIN = c(2,3), FUN = sum)

          Catch = rowSums(catchtmp)
          Catch.us = catchtmp[,2]
          Catch.can = catchtmp[,1]

          Catch.q = rowSums(lst[[i]]$Catch.quota)
          Catch.q.us = rowSums(lst[[i]]$Catch.quota[,2,])
          Catch.q.can = rowSums(lst[[i]]$Catch.quota[,1,])


        }

        if(is.null(lst[[i]]$F0)){
          lst[[i]]$F0 <- matrix(NA, length(yr),2)
        }

        ls.tmp <- data.frame(year = yrs, SSB.can = lst[[i]]$SSB[,1], SSB.US = lst[[i]]$SSB[,2],
                             SSBtot =  lst[[i]]$SSB[,1]+lst[[i]]$SSB[,2],
                             F0.can = lst[[i]]$F0[,1],
                             F0.us = lst[[i]]$F0[,2],
                             amc = lst[[i]]$amc$amc.tot,
                             ams = lst[[i]]$ams$ams.tot,
                             amc.can = lst[[i]]$amc$amc.can,
                             amc.us = lst[[i]]$amc$amc.US,
                             ams.can = lst[[i]]$ams$ams.can,
                             ams.us = lst[[i]]$ams$ams.US,
                             SSB.mid.can = lst[[i]]$SSB.mid[,1],
                             SSB.mid.us = lst[[i]]$SSB.mid[,2],
                             run = paste('run',i, sep = '-'),
                             Catch.q = Catch.q,
                             Catch.q.us = Catch.q.us,
                             Catch.q.can = Catch.q.can,
                             Catch = Catch,
                             Catch.us = Catch.us,
                             Catch.can = Catch.can,
                             F0.us = Catch.us/lst[[i]]$SSB.mid[,2],
                             F0.can = Catch.can/lst[[i]]$SSB.mid[,1])
        ls.df <- rbind(ls.df, ls.tmp)}

    }
    else{
      nfailed[i] <- 0
    }
  }

  plotname <- attributes(lst)$plotname
  ggplot(ls.df, aes(x = year, y = Catch, color = run)) +
    geom_line() +
    theme(legend.position = "none")

  SSB.plotquant <- ls.df[ls.df$year > 2010,] %>%
    group_by(year) %>%
    summarise(med.can = median(SSB.can),
              p95.can = quantile(SSB.can, 0.95),
              p5.can = quantile(SSB.can,0.05),
              med.US = median(SSB.US),
              p95.US = quantile(SSB.US, 0.95),
              p5.US = quantile(SSB.US,0.05))
  SSB.plotquant$run <- plotname

  SSB.plottot <- ls.df[ls.df$year > 2010,] %>%
    group_by(year) %>%
    summarise(med = median(SSBtot),
              avg = mean(SSBtot),
              p95 = quantile(SSBtot, 0.95),
              p5 = quantile(SSBtot,0.05))
  SSB.plottot$run <- plotname

  SSB.plotmid <- ls.df[ls.df$year > 2010,] %>%
    group_by(year) %>%
    summarise(med.can = median(SSB.mid.can),
              p95.can = quantile(SSB.mid.can, 0.95),
              p5.can = quantile(SSB.mid.can,0.05),
              med.US = median(SSB.mid.us),
              p95.US = quantile(SSB.mid.us, 0.95),
              p5.US = quantile(SSB.mid.us,0.05))
  SSB.plotmid$run <- plotname

  Catch.plotquant <- ls.df[ls.df$year > 2010,] %>%
    group_by(year) %>%
    summarise(med = median(Catch),
              p95 = quantile(Catch, 0.95),
              p5 = quantile(Catch,0.05)
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
    summarise(med.can = median(ams.can,na.rm = TRUE),
              p95.can = quantile(ams.can, 0.95,na.rm = TRUE),
              p5.can = quantile(ams.can,0.05,na.rm = TRUE),
              med.us = median(ams.us,na.rm = TRUE),
              p95.us = quantile(ams.us, 0.95,na.rm = TRUE),
              p5.us = quantile(ams.us,0.05,na.rm = TRUE)

    )
  ams.space$run <- plotname

  amc.space <- ls.df[ls.df$year > 2010,] %>%
    group_by(year) %>%
    summarise(med.can = median(amc.can,na.rm = TRUE),
              p95.can = quantile(amc.can, 0.95,na.rm = TRUE),
              p5.can = quantile(amc.can,0.05,na.rm = TRUE),
              med.us = median(amc.us,na.rm = TRUE),
              p95.us = quantile(amc.us, 0.95,na.rm = TRUE),
              p5.us = quantile(amc.us,0.05,na.rm = TRUE)

    )
  amc.space$run <- plotname

  F0.space <- ls.df[ls.df$year > 2010,] %>%
    group_by(year) %>%
    summarise(med.can = median(F0.can,na.rm = TRUE),
              p95.can = quantile(F0.can, 0.95,na.rm = TRUE),
              p5.can = quantile(F0.can,0.05,na.rm = TRUE),
              med.us = median(F0.us,na.rm = TRUE),
              p95.us = quantile(F0.us, 0.95,na.rm = TRUE),
              p5.us = quantile(F0.us,0.05,na.rm = TRUE)

    )
  F0.space$run <- plotname

  Catch.q <- ls.df[ls.df$year > 2010,] %>%
    group_by(year) %>%
    summarise(med.can = mean(Catch.can/Catch.q.can,na.rm = TRUE),
              p95.can = quantile(Catch.can/Catch.q.can, 0.95,na.rm = TRUE),
              p5.can = quantile(Catch.can/Catch.q.can,0.05,na.rm = TRUE),
              med.us = mean(Catch.us/Catch.q.us,na.rm = TRUE),
              p95.us = quantile(Catch.us/Catch.q.us, 0.95,na.rm = TRUE),
              p5.us = quantile(Catch.us/Catch.q.us,0.05,na.rm = TRUE),
              med.tot = mean(Catch/Catch.q),
              p5.tot = quantile(Catch/Catch.q,0.05,na.rm = TRUE),
              p95.tot = quantile(Catch/Catch.q,0.95,na.rm = TRUE)

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
