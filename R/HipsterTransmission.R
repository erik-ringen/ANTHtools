#' @title HipsterTransmission
#' @description Agent-based model simulation of anticorformity in a quantitative trait, based on Smaldino and Epstein 'Social conformity despite individual preference for distinctiveness'
#' @param N Population size
#' @param d Anticonformity preference, units indicate standard deviations. 0 indicates preference for mean, positive values prefer higher than mean, negative values prefer lower.
#' @param s Variation in anticonformity preference, units indicate standard deviations; defaults to 0.
#' @param k Adjustment rate, bounded between 0 and 1. Controls how fast agents update their trait values.
#' @param t_max How many generations to run the sim.
#' @export

HipsterTransmission <- function( N, d, s=0, k, t_max ) {
  output <- matrix(NA, nrow=N, ncol=t_max)

  # Initial
  output[,1] <- 2*rbeta(N, 2, 2) - 1
  # Conformity preferences
  D <- rep(NA, N)
  for (i in 1:N) D[i] <- d + s*rnorm(1, 0, 1)

  for (t in 2:t_max) {
  # Current distribution
    mu <- mean(output[,t-1])
    sigma <- sd(output[,t-1])

  for (i in 1:N) {
    optima <- mu + D[i]*sigma
    output[i,t] <- output[i,t-1] + k*(optima - output[i,t-1])
  }
  }

  plot(NULL, xlim=c(0,t_max), ylim=c(min(output), max(output)), xlab="Time", ylab="Trait Value")
  for (i in 1:N) lines(x=seq(from=1,to=t_max), y=output[i,], col=makeTransparent("cornflowerblue", 0.6))
}

