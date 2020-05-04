#' Plot TAC vs SSB as a simple line
#'
#' @param df A [data.frame] as output by [calc_tac_est_vs_real()]
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_tac_est_vs_real <- function(df){
  ggplot(data = df$tac_ssb, aes(x = ssb * 1e-6, y = tac * 1e-6)) +
    geom_line() +
    scale_y_continuous("TAC (million tonnes)") +
    scale_x_continuous("SSB (million tonnes)") +
    theme_classic()
}

#' Plot estimated vs realized TAC (called Catch in plot) by harvest control rule
#' value
#'
#' @param df A [data.frame] as output by [calc_tac_est_vs_real()]
#'
#' @return A [ggplot2::ggplot()] object
#' @importFrom dplyr rename
#' @importFrom PNWColors pnw_palette
#' @export
plot_tac_vs_hcr <- function(df){
  # Do a regression on the difference between them
  lm_historical <- lm(TAC ~ AssessTac, data = df$tac)
  lm_realized <- lm(Realized ~ AssessTac, data = df$tac)
  df_adjTAC <- data.frame(incpt = c(lm_historical$coefficients[1],
                                    lm_realized$coefficients[1]),
                          slp = c(lm_historical$coefficients[2],
                                  lm_realized$coefficients[2]),
                          adj = c("historical", "realized"))

  df_plot <- data.frame(tac = df$tac_ssb$tac,
                        tac_historical = predict(lm_historical,
                                                 newdata = data.frame(AssessTac = df$tac_ssb$tac)),
                        tac_realized = predict(lm_realized,
                                               newdata = data.frame(AssessTac = df$tac_ssb$tac)),
                        ssb = df$tac_ssb$ssb) %>%
    mutate(tac_historical = ifelse(tac_historical > tac, tac_historical, tac)) %>%
    mutate(tac_realized = ifelse(tac_realized > tac, tac_realized, tac)) %>%
    mutate(floor = tac * 0.5) %>%
    mutate(floor = ifelse(floor <= 180000, 180000, floor)) %>%
    mutate(tac_hcr = tac) %>%
    select(-ssb)

  df_plot <- melt(df_plot, id.vars = "tac", value.name = "Quota", variable.name = "HCR")
  nhcr <- unique(df_plot$HCR)
  cols <- pnw_palette("Starfish", n = 4, type = "discrete")

  df_plot$HCR <- factor(df_plot$HCR,
                        levels = c("tac_hcr",
                                   "tac_historical",
                                   "tac_realized",
                                   "floor"))

  g <- ggplot(df_plot, aes(x = tac * 1e-3, y = Quota * 1e-3, color = HCR)) +
    geom_line(linetype = 2, size = 0.8) +
    scale_y_continuous("Catch \n(thousand tonnes)") +
    scale_color_manual(values = cols,
                       labels = c("Base scenario", "Historical", "Realized", "Floor")) +
    scale_x_continuous("Harvest control rule") +
    coord_cartesian(ylim = c(0, 800), xlim = c(0, 1000)) +
    geom_point(data = df$tac, aes(x = AssessTac * 1e-3, y = Realized * 1e-3), color = cols[3])+
    geom_point(data = df$tac, aes(x = AssessTac * 1e-3, y = TAC * 1e-3), color = cols[2])+
    theme_classic()+
    #geom_point(data = df.tac[df.tac$Year >= 2012,],aes(x=AssessTac*1e-3,y = TAC*1e-3), color = alpha(cols[4],0.5))+
    theme(legend.title = element_blank(),
          legend.text = element_text(),
          legend.justification = "left",
          legend.margin = margin(t = 20, r = 0, b = 0, l = 0, unit = "mm"),
          legend.position = c(0.05, 1))
  g
}