#' Calculate the fishing mortality based on catch
#'
#' @param e_temp Catch
#' @param b_temp Biomass
#' @param m_season natural mortality
#' @param f_sel Fisheries selectivity
#' @param n_temp Numbers at age
#' @param w_catch weight at age
#' @param method 'Pope' or 'Hybrid' method?
#'
#' @return The fishing mortality value
#' @export
get_f <- function(e_temp,
                  b_temp,
                  m_season,
                  f_sel,
                  n_temp,
                  w_catch,
                  method = 'Pope'){
  # This function can be simplified significantly
  if(e_temp > 0){
    temp <- e_temp / (b_temp + 0.1 * e_temp)
    join <- (1 + exp(30 * (temp - 0.95))) ^ -1
    temp2 <- join * temp + 0.95 * (1 - join)
    f_new <- -log(1 - temp2)
    #Fout <- df$F0[yr]
    if(method == "Hybrid"){
      for(i in 1:4){
        z <- m_season + f_new * f_sel
        alpha <- (1 - exp(-z))
        c_temp <- sum((f_new / z) * (n_temp * w_catch * f_sel) * alpha)

        z_adj <- e_temp/(c_temp + 0.0001)

        z_prime <- m_season + z_adj * (z - m_season)
        alpha <- (1 - exp(-z_prime)) / (z_prime)

        temp <- sum(n_temp * w_catch * f_sel * alpha)
        f_temp <-  e_temp / (temp + 0.0001)
        #          print(f_temp)
        j2 <- 1 / (1 + exp(30 * (f_temp - 0.95 * 1)))

        f_new <- j2 * f_temp + (1 - j2)
      }
    }

  }else{
    f_new <- 0
  }
  f_new
}
