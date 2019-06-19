#' @title moth_sim
#' @description Simulates the evolution of peppered moths in a polluted environment, based on Wilensky, U. (1997). NetLogo Peppered Moths model. http://ccl.northwestern.edu/netlogo/models/PepperedMoths. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.
#' @param pollution Environmental pollution levels, MUST BE BETWEEN 0 and 1.
#' @param speed Rate of environmental (pollution) change, either 'fixed' (no change), 'slow', or 'fast'.
#' @param predation Intensity of predation, MUST BE BETWEEN 0 and 1.
#' @param drift magnitude of drift, must be either 'none', 'small', or 'large'.
#' @return Returns a list of moth phenotypes over time, plus pollution levels and input parameter values
#' @export

moth_sim <- function( pollution, speed, predation, drift ) {

  parms = c( pollution, speed, predation, drift ) # saving initial values

  # Building support functions
  '%ni%' <- Negate('%in%')

  inv_logit <- function (x)
  {
    p <- 1/(1 + exp(-x))
    p <- ifelse(x == Inf, 1, p)
    p
  }

  logistic <- function (x)
  {
    p <- 1/(1 + exp(-x))
    p <- ifelse(x == Inf, 1, p)
    p
  }

  # Checking for bad parameter values
  if (pollution < 0 || pollution > 1) {
    print("Error: pollution parameter must be between 0 and 1")
  }

  else if (predation < 0 || predation > 1) {
    print("Error: predation parameter must be between 0 and 1")
  }

  else if (drift %ni% c("none", "small", "large")) {
    print("Error: drift parameter must be not be 'none', 'small', or 'large'")
  }

  else if (speed %ni% c("fixed", "slow", "fast")) {
    print("Error: spped parameter must be 'fixed', 'slow', or 'fast'")
  }

  else {

N_init <- 1000
N_gen <- 100

# Initialize the population, sampling each moth color evenly
init_moths <- sample( c("light", "medium", "dark"), size=N_init, replace=TRUE )

# Each generation, the life-cycle is:
# 1) Face predators; survival is a function of pollution levels and moth color
# Pr(Predation|Color,N_predators) = predation intensity * (|pollution color - moth color|)

# 2) IF moths survived and they are mature, lay 1 moth eggs that will mature at time T+1 (asexual reproduction)

moth_df <- data.frame(
  light = rep(NA, N_gen+1),
  medium = rep(NA, N_gen+1),
  dark = rep(NA, N_gen+1),
  pol = rep(NA, N_gen+1)
)

#### Running the simulation ####################
for (t in 0:N_gen) {

  if (t == 0) { # initializing the sim for gen 0
    light_survive <- length(init_moths[init_moths == "light"])
    medium_survive <- length(init_moths[init_moths == "medium"])
    dark_survive <- length(init_moths[init_moths == "dark"])
  }

  else {

  # Pollution fluctuations
  if (speed == "slow") pollution = pollution + rnorm(1, 0, 0.02)
  if (speed == "fast") pollution = pollution + rnorm(1, 0, 0.1)

  # Constraint
  if (pollution < 0) pollution = 0
  if (pollution > 1) pollution = 1

  # Predation
  light_survive <- light_survive * (1 - (predation/2) * abs(pollution - 0))
  medium_survive <- medium_survive * (1 - (predation/2) * abs(pollution - 0.5))
  dark_survive <- dark_survive * (1 - (predation/2) * abs(pollution - 1))

  # Drift (random fluctuation in pop size)
  if (drift == "small") {
  light_survive <- light_survive + rnorm(1, mean=0, sd=0.05*light_survive)
  medium_survive <- medium_survive + rnorm(1, mean=0, sd=0.05*medium_survive)
  dark_survive <- dark_survive + rnorm(1, mean=0, sd=0.05*dark_survive)
  }

  if (drift == "large") {
    light_survive <- light_survive + rnorm(1, mean=0, sd=0.2*light_survive)
    medium_survive <- medium_survive + rnorm(1, mean=0, sd=0.2*medium_survive)
    dark_survive <- dark_survive + rnorm(1, mean=0, sd=0.2*dark_survive)
  }

  # Reproduction, with exponential growth and asexual reproduction
  light_survive <- round(exp(0.15) * light_survive)
  medium_survive <- round(exp(0.15) * medium_survive)
  dark_survive <- round(exp(0.15) * dark_survive)
  }

  # Recording each pop. size (virtual ecology!)
  moth_df[t+1,1] = light_survive
  moth_df[t+1,2] = medium_survive
  moth_df[t+1,3] = dark_survive
  moth_df[t+1,4] = pollution
} # end simulation
################################################

moth_list <- list(moth_df=moth_df, parms=parms)

return(moth_list)
}
}

#' @title plot_moth
#' @description Plots the results of peppered moths simulation
#' @param sim moth simulation data, created by moth_sim()
#' @return three-panel plot
#' @export

plot_moth <- function(x) {

  sim <- x$moth_df

  if (colnames(sim)[1] == "light") {
    par(mfrow=c(3,1))

    # Population time-series
    plot(NA, xlim=c(0,100), ylim=c(0,max(sim)), xlab="Time", ylab="Moth Population Size")

    # Put the legend in a good place, depending on sim result
    if (sum(sim[1,1:3]) > sum(sim[101,1:3])) legend(legend=c("Light", "Medium", "Dark"), x=85, y=max(sim), lwd=2, col=c("orange", "red", "darkred"))
    else legend(legend=c("Light", "Medium", "Dark"), x=0, y=max(sim), lwd=2, col=c("orange", "red", "darkred"))
    lines(x=seq(from=0, to=100), y=sim[,1], col="orange", lwd=2)
    lines(x=seq(from=0, to=100), y=sim[,2], col="red", lwd=2)
    lines(x=seq(from=0, to=100), y=sim[,3], col="darkred", lwd=2)
    mtext(paste0("pollution = ", x$parms[1], "    speed = ", x$parms[2], "   predation = ", x$parms[3], "   drift = ", x$parms[4]))

    # Pollution time-series
    plot(NA, xlim=c(0,100), ylim=c(0,1), xlab="Time", ylab="Pollution Level")
    lines(x=seq(from=0, to=100), y=sim[,4], lwd=2)

    # Pop. proportion
    plot(NA, xlim=c(0,100), ylim=c(0,1), xlab="Time", ylab="Moth Color Proportion")
    lines(x=seq(from=0, to=100), y=sim[,1] / rowSums(sim[,-4]), col="orange", lwd=2)
    lines(x=seq(from=0, to=100), y=sim[,2] / rowSums(sim[,-4]), col="red", lwd=2)
    lines(x=seq(from=0, to=100), y=sim[,3] / rowSums(sim[,-4]), col="darkred", lwd=2)
  }

  else {
    print("Error: object not created with moth_sim() function")
  }
}
