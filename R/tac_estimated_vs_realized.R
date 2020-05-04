#' Calculated the estimated TAC vs the Realized TAC (from data)
#'
#' @details SSB is a vector of equally spaced values between zero and double the
#' Operating model SSB0 value (for a single-space, single-season model). TAC is
#' zero for SSB / SSB0 less than 0.1, 0.4\ * SSB for values greater than 0.4, and
#' for values between 0.1 and 0.4, TAC is 0.4\* SSB \* (SSB - 0.1SSB0) \*
#' the ratio (0.4 \* SBB0 / SSB) / (0.4 \* SSB0 - 0.1SSB0)
#' @return A list of 2 items. (1) A [data.frame] as read in from the TAC.csv data file
#' found in this package, and (2) A [data.frame] as calculated by this function
#' @importFrom readr read_csv
#' @export
calc_tac_est_vs_real <- function(){

  tac_fn <- system.file(file.path("extdata", "TAC.csv"),
              package = "PacifichakeMSE",
              mustWork = TRUE)
  df_tac <- read_csv(tac_fn)
  df <- load_data_seasons(nseason = 1, nspace = 1)
  sim_data <- run.agebased.true.catch(df)

  # Calculate the theoretical TAC
  ssb <- seq(0, sum(sim_data$SSB0) * 2, length.out = nrow(df_tac))
  ssb0 <- sum(sim_data$SSB0)
  tac <- matrix(NA, nrow = nrow(df_tac))
  # add a very low catch (fix later)
  tac[(ssb / ssb0) < 0.1] <- 0
  # For simplicity assume SSB is the same as V (it's close)
  tac[(ssb / ssb0) > 0.4] <- 0.4 * ssb[(ssb / ssb0) > 0.4]
  ix <- (ssb / ssb0) >= 0.1 & (ssb / ssb0) <= 0.4
  # TODO: Check the second-to-last term below. Should it be (0.4 * ssb[ix] / ssb0) ?
  tac[ix] <- 0.4 * ssb[ix] * ((ssb[ix] - 0.1 * ssb0) *
                                ((0.4 * ssb0 / ssb[ix]) /
                                   (0.4 * ssb0 - 0.1 * ssb0)))
  list(tac = df_tac,
       tac_ssb = data.frame(ssb = ssb, tac = tac))
}

# ggplot(data = df.plot, aes(x = SSB*1e-6, y = TAC*1e-6))+geom_line()+
#   scale_y_continuous('TAC (million tonnes)')+scale_x_continuous('SSB (million tonnes)')+theme_classic()
#
# ### Do a regression on the difference between
#
# #lm.JMC <- lm(TAC ~ AssessTac, data = df.tac[df.tac$Year >= 2012,])
# #lm.STAR <- lm(TAC ~ AssessTac, data = df.tac[df.tac$Year < 2012,])
# lm.historical <- lm(TAC ~ AssessTac, data = df.tac)
# lm.realized <- lm(Realized ~ AssessTac, data = df.tac)
#
#
# #### Print the TAC calc adjustment ####
# #
# # df.adjTAC <- data.frame(incpt = c(lm.JMC$coefficients[1],lm.realized$coefficients[1], lm.STAR$coefficients[1]),
# #                         slp = c(lm.JMC$coefficients[2],lm.realized$coefficients[2], lm.STAR$coefficients[2]),
# #                         adj = c('JMC','Realized','STAR'))
#
# df.adjTAC <- data.frame(incpt = c(lm.historical$coefficients[1],lm.realized$coefficients[1]),
#                         slp = c(lm.historical$coefficients[2],lm.realized$coefficients[2]),
#                         adj = c('historical','realized'))
#
# #write.csv(df.adjTAC, 'adjusted_tac_fn.csv', row.names = FALSE)
# #
# # df.plot <- data.frame(TAC = TAC,
# #                       TAC.JMC = predict(lm.JMC, newdata = data.frame(AssessTac = TAC)),
# #                       TAC.realized = predict(lm.realized, newdata = data.frame(AssessTac = TAC)),
# #                       TAC.STAR = predict(lm.STAR, newdata = data.frame(AssessTac = TAC)),
# #                       SSB = SSB)
#
# df.plot <- data.frame(TAC = TAC,
#                       TAC.historical = predict(lm.historical, newdata = data.frame(AssessTac = TAC)),
#                       TAC.realized = predict(lm.realized, newdata = data.frame(AssessTac = TAC)),
#                       SSB = SSB)
#
#
#
# df.plot$TAC.historical[df.plot$TAC.historical > df.plot$TAC] <-df.plot$TAC[df.plot$TAC.historical >TAC]
# df.plot$TAC.realized[df.plot$TAC.realized > df.plot$TAC] <-df.plot$TAC[df.plot$TAC.realized >TAC]
# #df.plot$TAC.STAR[df.plot$TAC.STAR > df.plot$TAC] <-df.plot$TAC[df.plot$TAC.STAR >TAC]
# # Floor data
# df.plot$Floor <- df.plot$TAC*0.5
# df.plot$Floor[df.plot$Floor<= 180000] <- 180000
# df.plot$TAC.HCR <- df.plot$TAC
#
# df.plot.w <- melt(df.plot[,-4], id.vars = 'TAC', value.name = 'Quota', variable.name = 'HCR')
# nhcr <- unique(df.plot.w$HCR)
# cols <- PNWColors::pnw_palette('Starfish',n = 4, type = 'discrete')
#
#
# df.plot.w$HCR <- factor(df.plot.w$HCR, levels = c("TAC.HCR", "TAC.historical", "TAC.realized",
#                                                   'Floor'))
#
# # cols <- LaCroixColoR::lacroix_palette('PassionFruit', n = 4, type = 'discrete')
# # cols <- RColorBrewer::brewer.pal(4, 'Accent')
#
# p1 <- ggplot(df.plot.w, aes(x= TAC*1e-3, y = Quota*1e-3, color = HCR))+geom_line(linetype = 2, size = 0.8)+
#   scale_y_continuous('Catch \n(thousand tonnes)')+scale_color_manual(values = cols,
#                                                                      labels = c('base scenario','historical','realized','floor'))+
#   scale_x_continuous('Harvest control rule')+ coord_cartesian(ylim=c(0, 800), xlim = c(0,1000))+
#   geom_point(data = df.tac,aes(x=AssessTac*1e-3, y = Realized*1e-3), color = cols[3])+
#   geom_point(data = df.tac,aes(x=AssessTac*1e-3,y = TAC*1e-3), color = cols[2])+
#   theme_classic()+
#   #geom_point(data = df.tac[df.tac$Year >= 2012,],aes(x=AssessTac*1e-3,y = TAC*1e-3), color = alpha(cols[4],0.5))+
#   theme(legend.title = element_blank(),
#         legend.key.size =  unit(.2, "cm"),
#         legend.text=element_text(size=7),
#         legend.position = c(0.15,0.8))
# p1
#
# p3 <- ggplot(df.plot.w[df.plot.w$HCR != 'Floor',], aes(x= TAC*1e-3, y = Quota*1e-3, color = HCR))+geom_line(linetype = 2, size = 0.8)+
#   scale_y_continuous('Catch \n(thousand tonnes)')+scale_color_manual(values = cols[1:3],
#                                                                      labels = c('base scenario','historical','realized'))+
#   scale_x_continuous('Harvest control rule')+ coord_cartesian(ylim=c(0, 800), xlim = c(0,1000))+
#   geom_point(data = df.tac,aes(x=AssessTac*1e-3, y = Realized*1e-3), color = cols[3])+
#   geom_point(data = df.tac,aes(x=AssessTac*1e-3,y = TAC*1e-3), color = cols[2])+
#   theme_classic()+
#   #geom_point(data = df.tac[df.tac$Year >= 2012,],aes(x=AssessTac*1e-3,y = TAC*1e-3), color = alpha(cols[4],0.5))+
#   theme(legend.title = element_blank(),
#         legend.key.size =  unit(.2, "cm"),
#         legend.text=element_text(size=7),
#         legend.position = c(0.2,0.8))
# p3
#
# #
# p2 <- ggplot(df.plot, aes(x = SSB*1e-6, y = TAC*1e-6))+geom_line(size = 0.8)+
#   geom_line(aes(y = TAC.JMC*1e-6), color = alpha('red',0.5), linetype = 2, size = 0.8)+
#   geom_line(aes(y = TAC.realized*1e-6), color = alpha('blue',0.5), linetype = 2, size = 0.8)+
#   scale_y_continuous('TAC (million tonnes)')+
#   scale_x_continuous('SSB (million tonnes)')+theme_classic()
# #png('SSB_tac.png', width = 16, height = 12, unit = 'cm', res =400)
# p2
# # dev.off()
#
#
#
# png('Figs/tacplot.png', width = 12, height = 8, unit = 'cm', res =400)
# p1
# dev.off()
#
# # Without the floor thing
# png('results/Climate/tacs.png', width = 8, height = 5, unit = 'cm', res =400)
# p3
# dev.off()
