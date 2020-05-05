#' Hake objectives (TODO: Improve docs on this function)
#'
#' @param lst list of MSE results
#' @param ssb0 unfished biomass from OM
#' @param move Logical. Is movement in the OM?
#' @param short_term_yrs Years for short term plots
#' @param long_term_yrs Years greater than this will be in long term plots
#'
#' @return List of three: p.export, t.export, ls.season (TODO: explain better)
#' @importFrom ggplot2 alpha
#' @importFrom dplyr left_join lead lag n
#' @export
hake_objectives <- function(lst = NULL,
                            ssb0 = NULL,
                            move = NA,
                            short_term_yrs = 2018:2027,
                            long_term_yrs = 2027,
                            can.prop = 0.2488,
                            us.prop = 0.7612){
  stopifnot(!is.null(lst))
  stopifnot(!is.null(ssb0))

  yrs <- as.numeric(attributes(lst[[1]]$Catch)$dimnames$year)
  min_yr <- min(yrs)
  nyrs <- length(yrs)
  if(nyrs == 1){
    nyrs <- nrow(lst[[1]]$Catch)
  }
  simyears <- nyrs - (length(min_yr:short_term_yrs[1])) + 1
  nruns <- length(lst)

  df_ssb_plot <- map2(lst, seq_along(lst), ~{
    data.frame(year = yrs,
               ssb = rowSums(.x$SSB) / sum(ssb0),
               run = .y)
  }) %>% map_df(~{.x})

  # Vulnerable biomass at mid-year start of season 3 for each country
  df_v_ca_plot <- map2(lst, seq_along(lst), ~{
    data.frame(year = yrs,
               v = .x$V[,1,3],
               run = .y)
  }) %>% map_df(~{.x})

  df_v_us_plot <- map2(lst, seq_along(lst), ~{
    data.frame(year = yrs,
               v = .x$V[,2,3],
               run = .y)
  }) %>% map_df(~{.x})

  catch_plot <- map2(lst, seq_along(lst), ~{
    ct <- apply(.x$Catch, MARGIN = 2, FUN = sum)
    if(length(ct) == 1){
      data.frame(year = yrs,
                 catch = .x$Catch,
                 run = .y)
    }else{
      data.frame(year = yrs,
                 catch = ct,
                 run = .y)
    }
  }) %>% map_df(~{.x})

  quota_tot <- map2(lst, seq_along(lst), ~{
    quot <- apply(.x$Catch.quota, MARGIN = 1, FUN = sum)
    if(length(quot) == 1){
      data.frame(year = yrs,
                 catch = .x$Catch.quota,
                 run = .y)
    }else{
      data.frame(year = yrs,
                 catch = quot,
                 run = .y)
    }
  }) %>% map_df(~{.x})

  quota_plot <- map2(list(quota_tot), list(catch_plot), ~{
    .x %>%
      left_join(.y, by = c("year", "run")) %>%
      mutate(quota_frac = catch.x / catch.y) %>%
      select(year, quota_frac, run)
  })

  quota_us_tot <- map2(lst, seq_along(lst), ~{
    data.frame(year = yrs,
               quota = rowSums(.x$Catch.quota[,2,]),
               run = .y)
  }) %>% map_df(~{.x})

  quota_ca_tot <- map2(lst, seq_along(lst), ~{
    data.frame(year = yrs,
               quota = rowSums(.x$Catch.quota[,1,]),
               run = .y)
  }) %>% map_df(~{.x})

  catch_area <- map2(lst, seq_along(lst), ~{
    data.frame(year = yrs,
               area = apply(.x$Catch, MARGIN = c(2, 3), FUN = sum),
               run = .y)
  }) %>% map_df(~{.x}) %>%
    transmute(year,
              ca = area.1,
              us = area.2,
              run)
  catch_us_tot <- catch_area %>%
    transmute(year, catch = us, run)
  catch_ca_tot <- catch_area %>%
    transmute(year, catch = ca, run)

  vtac_us <- map2(list(df_v_us_plot), list(catch_us_tot), ~{
    .x %>%
      left_join(.y, by = c("year", "run")) %>%
      mutate(v_tac = v / catch) %>%
      select(year, v_tac, run)
  }) %>% map_df(~{.x})

  vtac_ca <- map2(list(df_v_us_plot), list(catch_ca_tot), ~{
    .x %>%
      left_join(.y, by = c("year", "run")) %>%
      mutate(v_tac = v / catch) %>%
      select(year, v_tac, run)
  }) %>% map_df(~{.x})

  vtac_us_seas <- map2(lst, seq_along(lst), ~{
    ctmp <- colSums(.x$Catch)
    data.frame(year = yrs,
               v_tac_sp = ctmp[, 2, 2] / .x$V[, 2, 2],
               v_tac_su = ctmp[, 2, 3] / .x$V[, 2, 3],
               v_tac_fa = ctmp[, 2, 4] / .x$V[, 2, 4],
               run = .y)
  }) %>% map_df(~{.x})

  vtac_ca_seas <- map2(lst, seq_along(lst), ~{
    ctmp <- colSums(.x$Catch)
    data.frame(year = yrs,
               v_tac_sp = ctmp[, 1, 2] / .x$V[, 1, 2],
               v_tac_su = ctmp[, 1, 3] / .x$V[, 1, 3],
               v_tac_fa = ctmp[, 1, 4] / .x$V[, 1, 4],
               run = .y)
  }) %>% map_df(~{.x})

  aav_plot <- map2(list(catch_plot), seq_along(list(catch_plot)), ~{
    .x %>%
      group_by(run) %>%
      mutate(catch_lag = lag(catch, 1)) %>%
      mutate(aav = abs(catch - catch_lag) / n()) %>%
      ungroup() %>%
      filter(!is.na(aav)) %>%
      select(year, aav, run)
  }) %>% map_df(~{.x})

  browser()


  for(i in 2:nruns){
    ls.tmp <- lst[[i]]
    if(is.list(ls.tmp)){
      if(is.na(move)){
        SSB.tmp <- data.frame(SSB = (ls.tmp$SSB)/(ssb0), year = yr, run =  paste("run",i, sep=""))
        Catch.tmp <- data.frame(Catch = ls.tmp$Catch, year = yr, run =  paste("run",i, sep=""))
        quota.tmp <- data.frame(Catch = ls.tmp$Catch/apply(ls.tmp$Catch.quota, MARGIN = 1, FUN = sum),
                                year = yr, run =  paste("run",i, sep=""))
        V.tmp<-data.frame(V = ls.tmp$V[,3], year = yr, run =  paste("run",i, sep=""))
      }else{
        catchtmp <-  as.numeric(apply(ls.tmp$Catch,MARGIN = 2, FUN = sum))
        if(length(catchtmp) == 1){
          catchtmp <- lst[[idx]]$Catch
        }
        SSB.tmp <- data.frame(SSB = rowSums(ls.tmp$SSB)/sum(ssb0), year = yr, run =  paste("run",i, sep=""))
        Catch.tmp <- data.frame(Catch = catchtmp, year = yr, run =  paste("run",i, sep=""))
        quota.tmp <- data.frame(Quota_frac = catchtmp/apply(ls.tmp$Catch.quota, MARGIN = 1, FUN = sum), year = yr, run =  paste("run",i, sep=""))
        if(ncol(ls.tmp$Catch) == 1){ #if a single area model, catch by country fixed as proportion of total
          Catch.can <- ls.tmp$Catch*0.26
          Catch.us <- ls.tmp$Catch*0.74
        }else{
          Catch.can <- apply(ls.tmp$Catch,MARGIN = c(2,3), FUN = sum)[,1]
          Catch.us <- apply(ls.tmp$Catch,MARGIN = c(2,3), FUN = sum)[,2]
        }
        quota.tmp.can <- data.frame(Catch = Catch.can/rowSums(ls.tmp$Catch.quota[,1,]),
                                    year = yr, run =  paste("run",i, sep=""))
        quota.tmp.us <- data.frame(Catch = Catch.us/rowSums(ls.tmp$Catch.quota[,2,]),
                                   year = yr, run =  paste("run",i, sep=""))
        quota.tmp.can.tot<-data.frame(quota = rowSums(ls.tmp$Catch.quota[,1,]),
                                      year = yr, run =  paste("run",i, sep=""))
        quota.tmp.us.tot<-data.frame(quota = rowSums(ls.tmp$Catch.quota[,2,]),
                                     year = yr, run =  paste("run",i, sep=""))
        v.tmp.can <-data.frame(V = ls.tmp$V[,1,3],
                               year = yr, run =  paste("run",i, sep=""))
        v.tmp.us <-data.frame(V = ls.tmp$V[,2,3],
                              year = yr, run =  paste("run",i, sep=""))
        catch.tmp.can <- data.frame(Catch=Catch.can,
                                    year = yr, run =  paste("run",i, sep=""))
        catch.tmp.us <-  data.frame(Catch=Catch.us,
                                    year = yr, run =  paste("run",i, sep=""))
        vtac.tmp.can<- data.frame(V.TAC=v.tmp.can$V/quota.tmp.can.tot$quota,
                                  year = yr, run =  paste("run",i, sep=""))
        vtac.tmp.us<- data.frame(V.TAC =v.tmp.us$V/quota.tmp.us.tot$quota,
                                 year = yr, run =  paste("run",i, sep=""))
        Ctmp <- colSums(ls.tmp$Catch)
        vtac.tmp.us.seas<-data.frame(V.TAC.sp=Ctmp[,2,2]/ls.tmp$V[,2,2],
                                     V.TAC.su=Ctmp[,2,3]/ls.tmp$V[,2,3],
                                     V.TAC.fa=Ctmp[,2,4]/ls.tmp$V[,2,4],
                                     year = yr, run =  paste("run",i, sep=""))
        vtac.tmp.can.seas<-data.frame(V.TAC.sp=Ctmp[,1,2]/ls.tmp$V[,1,2],
                                      V.TAC.su=Ctmp[,1,3]/ls.tmp$V[,1,3],
                                      V.TAC.fa=Ctmp[,1,4]/ls.tmp$V[,1,4],
                                      year = yr, run =  paste("run",i, sep=""))
      }
      SSB.plot <- rbind(SSB.plot,SSB.tmp)
      Catch.plot <- rbind(Catch.plot,Catch.tmp)
      quota.plot <- rbind(quota.plot, quota.tmp)
      #quota.us<- rbind(quota.us, quota.tmp.us)
      #quota.can<- rbind(quota.can, quota.tmp.can)
      V.ca.plot <- rbind(V.ca.plot, v.tmp.can)
      V.us.plot <- rbind(V.us.plot, v.tmp.us)
      Catch.can.tot <- rbind(Catch.can.tot, catch.tmp.can)
      Catch.us.tot <- rbind(Catch.us.tot, catch.tmp.us)
      quota.can.tot <- rbind(quota.can.tot, quota.tmp.can.tot)
      quota.us.tot <- rbind(quota.us.tot, quota.tmp.us.tot)
      vtac.can <- rbind(vtac.can, vtac.tmp.can)
      vtac.us <-  rbind(vtac.us, vtac.tmp.us)
      vtac.can.seas <- rbind(vtac.can.seas, vtac.tmp.can.seas)
      vtac.us.seas <- rbind(vtac.us.seas, vtac.tmp.us.seas)
      AAV.tmp <- data.frame(AAV  = abs(catchtmp[2:length(yr)]-catchtmp[1:(length(yr)-1)])/catchtmp[1:(length(yr)-1)],
                            year = yr[2:length(yr)], run =  paste("run",i, sep=""))
      AAV.plot <- rbind(AAV.plot, AAV.tmp)
    }
  }
  SSB.plotquant <- SSB.plot %>%
    group_by(year) %>%
    summarise(med = median(SSB),
              p95 = quantile(SSB, 0.95),
              p25 = quantile(SSB, 0.25),
              p75 = quantile(SSB, 0.75),
              p5 = quantile(SSB,0.05))

  p1 <- ggplot(data=SSB.plotquant, aes(x= year,y = med)) +
    geom_ribbon(aes(ymin = p5, ymax = p95), fill = alpha("gray", alpha =0.5))+
    geom_ribbon(aes(ymin = p25, ymax = p75), fill = alpha("gray", alpha =0.8))+
    theme_classic()+scale_y_continuous(name = "SSB")+
    geom_line(color="black", size = 1.5)#+geom_line(data = SSB.plot, aes(y = SSB,group = run), color = alpha("black", alpha = 0.2))

  V.ca.plotquant<- V.ca.plot %>%
    group_by(year) %>%
    summarise(med = median(V),
              p95 = quantile(V, 0.95),
              p25 = quantile(V, 0.25),
              p75 = quantile(V, 0.75),
              p5 = quantile(V,0.05))

  Catch.plotquant <- Catch.plot %>%
    group_by(year) %>%
    summarise(med = median(Catch)*1e-6,
              p95 = quantile(Catch, 0.95)*1e-6,
              p25 = quantile(Catch, 0.25)*1e-6,
              p75 = quantile(Catch, 0.75)*1e-6,
              p5 = quantile(Catch,0.05)*1e-6)

  p2 <-  ggplot(Catch.plotquant,aes(x= year,y = med)) +
    geom_ribbon(aes(ymin = p5, ymax = p95), fill = alpha("gray", alpha =0.5))+
    geom_ribbon(aes(ymin = p25, ymax = p75), fill = alpha("gray", alpha =0.8))+
    theme_classic()+scale_y_continuous(name = "Catch (millions)")+
    geom_line(color="black", size = 1.5)#+geom_line(data = Catch.plot, aes(y = Catch,group = run), color = alpha("black", alpha = 0.2))

  AAV.plotquant <- AAV.plot %>%
    group_by(year) %>%
    summarise(med = median(AAV),
              p95 = quantile(AAV, 0.95),
              p25 = quantile(AAV, 0.25),
              p75 = quantile(AAV, 0.75),
              p5 = quantile(AAV,0.05))

  p3 <-  ggplot(AAV.plotquant,aes(x= year,y = med)) +
    geom_ribbon(aes(ymin = p5, ymax = p95), fill = alpha("gray", alpha =0.5))+
    geom_ribbon(aes(ymin = p25, ymax = p75), fill = alpha("gray", alpha =0.8))+
    theme_classic()+scale_y_continuous(name = "Catch\nvariability")+
    geom_line(color="black", size = 1.5)#+geom_line(data = Catch.plot, aes(y = Catch,group = run), color = alpha("black", alpha = 0.2))

  quota.plotquant <- quota.plot[quota.plot$year>2010,] %>%
    group_by(year) %>%
    summarise(med = mean(Quota_frac),
              p95 = quantile(Quota_frac, 0.95),
              p25 = quantile(Quota_frac, 0.25),
              p75 = quantile(Quota_frac, 0.75),
              p5 = quantile(Quota_frac,0.05))

  p4 <- ggplot(data=quota.plotquant, aes(x= year,y = med)) +
    geom_ribbon(aes(ymin = p5, ymax = p95), fill = alpha("gray", alpha =0.5))+
    geom_ribbon(aes(ymin = p25, ymax = p75), fill = alpha("gray", alpha =0.8))+
    theme_classic()+scale_y_continuous(name = "SSB")+coord_cartesian(ylim = c(0.4,1.05))+
    geom_line(color="black", size = 1.5)#+geom_line(data = SSB.plot, aes(y = SSB,group = run), color = alpha("black", alpha = 0.2))
  p4
  #cairo_pdf(filename = "MSE_run.pdf")
  #p.plot <- list(p1,p2,p3)
  #p.export <- plot_grid(plotlist = p.plot, ncol = 1, align ="v")
  #dev.off()
  ###  Plot the performance metrics from Kristins spreadsheet

  ## Probability of S < S10
  SSB.future <- SSB.plot[SSB.plot$year > 2018,]

  #####KM alternative metric calculation October 10, 2019
  ###create Prob S<S10 stat by run and look at med across years
  SSB10.stat <- SSB.future %>%
    group_by(run) %>%
    #filter(year>2018 & year<2028) %>%
    summarise(pcnt = length(which(SSB<0.1))/length(unique(SSB.future$year)))%>%
    summarise(med=median(pcnt),
              p95 = quantile(pcnt, 0.95),
              p25 = quantile(pcnt, 0.25),
              p75 = quantile(pcnt, 0.75),
              p5 = quantile(pcnt,0.05))

  SSB40.stat <- SSB.future %>%
    group_by(run) %>%
    #filter(year>2018 & year<2028) %>%
    summarise(pcnt = length(which(SSB<0.4 & SSB>0.1))/length(unique(SSB.future$year))) %>%
    summarise(med=median(pcnt),
              p95 = quantile(pcnt, 0.95),
              p25 = quantile(pcnt, 0.25),
              p75 = quantile(pcnt, 0.75),
              p5 = quantile(pcnt,0.05))

  if(max(Catch.plot$year) >= min(short_term_yrs)){
    Catch.plotshort <- Catch.plot %>%
      group_by(run) %>%
      filter(year>2018 & year<2028) %>%
      summarise(avg = mean(Catch)*1e-6) %>%
      summarise(med=median(avg),
                p95 = quantile(avg, 0.95),
                p25 = quantile(avg, 0.25),
                p75 = quantile(avg, 0.75),
                p5 = quantile(avg,0.05))
  }else{
    Catch.plotshort <- NA
  }

  if(max(Catch.plot$year) > long_term_yrs){
    Catch.plotlong <- Catch.plot %>%
      group_by(run) %>%
      filter(year>2027) %>%
      summarise(avg = mean(Catch)*1e-6) %>%
      summarise(med=median(avg),
                p95 = quantile(avg, 0.95),
                p25 = quantile(avg, 0.25),
                p75 = quantile(avg, 0.75),
                p5 = quantile(avg,0.05))
  }else{
    Catch.plotlong <- NA
  }

  AAV.plotshort <- AAV.plot %>%
    group_by(run) %>%
    summarise(avg = mean(AAV)) %>%
    summarise(med=median(avg),
              p95 = quantile(avg, 0.95),
              p25 = quantile(avg, 0.25),
              p75 = quantile(avg, 0.75),
              p5 = quantile(avg,0.05))

  V.ca.stat <- V.ca.plot %>%
    group_by(run) %>%
    summarise(avg = mean(V)) %>%
    summarise(med=median(avg),
              p95 = quantile(avg, 0.95),
              p25 = quantile(avg, 0.25),
              p75 = quantile(avg, 0.75),
              p5 = quantile(avg,0.05))

  V.us.stat <- V.us.plot %>%
    group_by(run) %>%
    summarise(avg = mean(V)) %>%
    summarise(med=median(avg),
              p95 = quantile(avg, 0.95),
              p25 = quantile(avg, 0.25),
              p75 = quantile(avg, 0.75),
              p5 = quantile(avg,0.05))

  vtac.ca.stat<- vtac.can %>%
    group_by(run) %>%
    summarise(prop = length(which(V.TAC>(1/0.3)))/length(V.TAC)) %>%
    summarise(med=median(prop),
              p95 = quantile(prop, 0.95),
              p25 = quantile(prop, 0.25),
              p75 = quantile(prop, 0.75),
              p5 = quantile(prop,0.05))

  vtac.us.stat<- vtac.us %>%
    group_by(run) %>%
    summarise(prop = length(which(V.TAC>1))/length(V.TAC)) %>%
    summarise(med=median(prop),
              p95 = quantile(prop, 0.95),
              p25 = quantile(prop, 0.25),
              p75 = quantile(prop, 0.75),
              p5 = quantile(prop,0.05))

  if(max(Catch.plot$year) > long_term_yrs){
    vtac.us.seas.stat <- vtac.us.seas %>%
      group_by(run) %>%
      filter(year>2027) %>%
      summarise(avg.sp = mean(1/V.TAC.sp),
                avg.su = mean(1/V.TAC.su),
                avg.fa= mean(1/V.TAC.fa)) %>%
      summarise(med.sp=median(avg.sp),
                med.su=median(avg.su),
                med.fa=median(avg.fa))
  }else{
    vtac.us.seas.stat <- NA
  }
  if(max(Catch.plot$year) > long_term_yrs){
    vtac.can.seas.stat <- vtac.can.seas %>%
      group_by(run) %>%
      filter(year>2027) %>%
      summarise(avg.sp = mean(1/V.TAC.sp),
                avg.su = mean(1/V.TAC.su),
                avg.fa= mean(1/V.TAC.fa)) %>%
      summarise(med.sp=median(avg.sp),
                med.su=median(avg.su),
                med.fa=median(avg.fa))
  }else{
    vtac.can.seas.stat <- NA
  }
  # vtac.us.seas.stat<- vtac.us.seas %>%
  #   group_by(run) %>%
  #   summarise(prop.sp = (length(which(V.TAC.sp>1))/length(V.TAC.sp)),
  #             prop.su = (length(which(V.TAC.su>1))/length(V.TAC.su)),
  #             prop.fa= (length(which(V.TAC.fa>1))/length(V.TAC.fa))) %>%
  #   summarise(med.sp=median(prop.sp),
  #             med.su=median(prop.su),
  #             med.fa=median(prop.fa))
  #
  # vtac.can.seas.stat<- vtac.can.seas %>%
  #   group_by(run) %>%
  #   summarise(prop.sp = (length(which(V.TAC.sp>1))/length(V.TAC.sp)),
  #             prop.su = (length(which(V.TAC.su>1))/length(V.TAC.su)),
  #             prop.fa= (length(which(V.TAC.fa>1))/length(V.TAC.fa))) %>%
  #   summarise(med.sp=median(prop.sp),
  #             med.su=median(prop.su),
  #             med.fa=median(prop.fa))


  # print(paste("percentage of years where SSB < 0.1ssb0= ",
  #             round(length(which(SSB.future$SSB<0.1))/length(SSB.future$SSB)*100, digits = 2),"%", sep = ""))
  #
  # print(paste("percentage of years where SSB > 0.1ssb0 & SSB < 0.4ssb0 = ",
  #             round(length(which(SSB.future$SSB>0.1 & SSB.future$SSB<0.4))/length(SSB.future$SSB)*100, digits = 2),"%", sep = ""))
  # print(paste("percentage of years where SSB > 0.4ssb0 = ",
  #             round(length(which(SSB.future$SSB>0.4))/length(SSB.future$SSB)*100, digits = 2),"%", sep = ""))
  #
  #
  # Catch.future <- Catch.plot[Catch.plot$year > 2018,]
  #
  # print(paste("percentage of Catch  < 180000= ",
  #             round(length(which(Catch.future$Catch < 180000))/length(Catch.future$Catch)*100, digits = 2),"%", sep = ""))
  #
  # print(paste("percentage of Catch  > 180k and < 350k= ",
  #             round(length(which(Catch.future$Catch > 180000 & Catch.future$Catch < 350000))/length(Catch.future$Catch)*100, digits = 2),"%", sep = ""))
  #
  # print(paste("percentage of Catch > 350k= ",
  #             round(length(which(Catch.future$Catch > 350000))/length(Catch.future$Catch)*100, digits = 2),"%", sep = ""))
  #
  # # Catch variability
  #
  # print(paste("median AAV = ",round(median(AAV.plotquant$med), digits = 2), sep = ""))

  ###
  #p.export <- grid.arrange(p1,p2,p3)


  ##calculate the probability of being between 10 and 40 percent of ssb0 for 3 consecutive years

  rns <- unique(SSB.future$run)
  p.vals <- matrix(0, length(unique(SSB.future$run)))

  for(i in 1:length(unique(SSB.future$run))){
    tmp <- SSB.future[SSB.future$run == rns[i],]
    if(length(tmp$year) >= 4){
      for(j in 1:(length(tmp$year)-3)){
        if(tmp$SSB[j]< 0.4){
          if(tmp$SSB[j+1]<0.4 & tmp$SSB[j+2] <0.4 & tmp$SSB[j+2]<0.4){
            p.vals[i] <- 1+p.vals[i]
          }
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
  indicator <- c("SSB <0.10 ssb0",
                 "0.10 < S < 0.4S0",
                 "S>0.4S0",
                 #    "3 consec yrs S<S40",
                 #   "years closed fishery",
                 "AAV",
                 "Mean SSB/ssb0",
                 #   "median catch",
                 "short term catch",
                 "long term catch",
                 "Canada TAC/V spr",
                 "Canada TAC/V sum",
                 "Canada TAC/V fall",
                 "US TAC/V spr",
                 "US TAC/V sum",
                 "US TAC/V fall")
                # "yrs bio unavailable")
  # Calculate the number of years the quota was met
  if(is.na(vtac.can.seas.stat[1]) | is.na(vtac.us.seas.stat[1])){
    t.export <- NA
  }else{
    t.export <- data.frame(indicator = as.factor(indicator),
                           value = c(
                             round(length(which(SSB.future$SSB <= 0.1))/length(SSB.future$SSB), digits = 2),
                             round(length(which(SSB.future$SSB>0.1 & SSB.future$SSB<0.4))/length(SSB.future$SSB), digits = 2),
                             round(length(which(SSB.future$SSB>0.4))/length(SSB.future$SSB), digits = 2),
                             # round(mean(p.vals), digits = 2),
                             #   mean(nclosed),
                             round(median(AAV.plotquant$med), digits = 2),
                             median(SSB.plotquant$med[SSB.plotquant$year > min(short_term_yrs)]),
                             #   median(1e6*Catch.plotquant$med[Catch.plotquant$year >2017])*1e-6,
                             median(1e6*Catch.plotquant$med[Catch.plotquant$year > min(short_term_yrs) &
                                                              Catch.plotquant$year <= long_term_yrs])*1e-6,
                             median(1e6*Catch.plotquant$med[Catch.plotquant$year > long_term_yrs - 2])*1e-6,
                             vtac.can.seas.stat$med.sp,
                             vtac.can.seas.stat$med.su,
                             vtac.can.seas.stat$med.fa,
                             vtac.us.seas.stat$med.sp,
                             vtac.us.seas.stat$med.su,
                             vtac.us.seas.stat$med.fa))
    # median(quota.plot[quota.plot$year > 2018,]$Quota_frac < 0.95)
  }

                         #lower = c(
                         #   round(length(which(SSB.future$SSB<0.1))/length(SSB.future$SSB)*100, digits = 2),
                         #   round(length(which(SSB.future$SSB>0.1 & SSB.future$SSB<0.4))/length(SSB.future$SSB)*100, digits = 2),
                         #   round(length(which(SSB.future$SSB>0.4))/length(SSB.future$SSB)*100, digits = 2),
                         #   round(mean(p.vals), digits = 2),
                         #   round(length(which(SSB.future$SSB<0.1))/length(SSB.future$SSB)*100, digits = 0),
                         #   round(median(AAV.plotquant$med), digits = 2),
                         #   median(SSB.plotquant$med[SSB.plotquant$year > 2017]),
                         #   median(1e6*Catch.plotquant$med[Catch.plotquant$year >2017])*1e-6,
                         #   median(1e6*Catch.plotquant$med[Catch.plotquant$year > 2018 & Catch.plotquant$year <2030])*1e-6
                         #   )
  print(t.export)
  p.export = NA
  # Add the seasonal stuff
  ls.season <- rbind(vtac.us.seas,  vtac.can.seas)
  ls.season$country <- c(rep("USA",nrow(vtac.us.seas)),rep("CAN",nrow(vtac.can.seas)))
  # Fix the V plots
  list(p.export,t.export,ls.season)
}
