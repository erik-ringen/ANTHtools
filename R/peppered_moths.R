#' @title moth_sim
#' @description Simulates the evolution of peppered moths in a polluted environment, based on Wilensky, U. (1997). NetLogo Peppered Moths model. http://ccl.northwestern.edu/netlogo/models/PepperedMoths. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.
#' @param pop_size Initial moth population size, defaults to 1000.
#' @param pollution Environmental pollution levels, MUST BE BETWEEN 0 and 1. Defaults to 0.
#' @param speed Rate of environmental (pollution) change, either 'fixed' (no change), 'slow', or 'fast'. Defaults to "fixed".
#' @param predation Intensity of predation, MUST BE BETWEEN 0 and 1. Defaults to 0.
#' @param drift magnitude of drift, must be either 'none', 'small', or 'large'. Defaults to "small".
#' @param migration Logical argument. If FALSE (F), the default, then no moths enter the population from outside. If TRUE (T), then migrant moths will reintroduce variation into the population even if it had reached fixation.
#' @param N_gen Number of generations to run the simulation. Defaults to 2000.
#' @return Returns a list of moth phenotypes over time, plus pollution levels and input parameter values
#' @export

moth_sim <- function(pop_size = 1000, pollution = 0, speed = "fixed", predation = 0, drift = "small", migration = F, N_gen = 2000) {

  parms = c(pop_size, pollution, speed, predation, drift) # saving initial values

  # Sanitize inputs
  N_gen <- ceiling(N_gen)

  # Building support functions
  '%ni%' <- Negate('%in%')

  inv_logit <- function(x)
  {
    p <- 1/(1 + exp(-x))
    p <- ifelse(x == Inf, 1, p)
    p
  }

  logistic <- function(x)
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

  else if (N_gen < 1) {
    print("Error: number of generations must be greater than 0")
  }

  else {

# Initialize the population, sampling each moth color evenly
init_moths <- sample( c("light", "medium", "dark"), size = ceiling(pop_size), replace = TRUE )

# Each generation, the life-cycle is:
# 1) Face predators; survival is a function of pollution levels and moth color
# Pr(Predation|Color,N_predators) = predation intensity * (|pollution color - moth color|)

# 2) IF moths survived and they are mature, lay 1 moth eggs that will mature at time T+1 (asexual reproduction)

moth_df <- data.frame(
  light = rep(NA, N_gen + 1),
  medium = rep(NA, N_gen + 1),
  dark = rep(NA, N_gen + 1),
  pol = rep(NA, N_gen + 1)
)

# Moth death at carrying capacity
b <- 5
k <- 4
K <- pop_size * 4

pr_death_K <- function(total_pop) {
  return(
    (1 - exp(-k * (total_pop / K)))^b
  )
}

fixation <- NA

light_survive <- length(init_moths[init_moths == "light"])
medium_survive <- length(init_moths[init_moths == "medium"])
dark_survive <- length(init_moths[init_moths == "dark"])

#### Running the simulation ####################
for (t in 0:N_gen) {

  total_pop <- light_survive + medium_survive + dark_survive
  pr_survive <- 1 - pr_death_K(total_pop)

  # Pollution fluctuations
  if (speed == "slow") pollution = pollution + rnorm(1, 0, 0.02)
  if (speed == "fast") pollution = pollution + rnorm(1, 0, 0.1)

  # Constraint
  if (pollution < 0) pollution = 0
  if (pollution > 1) pollution = 1

  # Predation
  # Calculate change in proportion on the log scale to prevent rounding errors
  light_survive <- exp(
    log(light_survive) + log(1 - (predation / 2) * abs(pollution - 0)) + log(pr_survive)
  )

  medium_survive <- exp(
    log(medium_survive) + log(1 - (predation / 2) * abs(pollution - 0.5)) + log(pr_survive)
  )

  dark_survive <- exp(
    log(dark_survive) + log(1 - (predation / 2) * abs(pollution - 1)) + log(pr_survive)
  )

  # Drift (random fluctuation in pop size)
  if (drift == "small") {
  light_survive <- light_survive + rnorm(1, mean = 0, sd = 0.05 * light_survive)
  medium_survive <- medium_survive + rnorm(1, mean = 0, sd = 0.05 * medium_survive)
  dark_survive <- dark_survive + rnorm(1, mean = 0, sd = 0.05 * dark_survive)
  }

  if (drift == "large") {
    light_survive <- light_survive + rnorm(1, mean = 0, sd = 0.2 * light_survive)
    medium_survive <- medium_survive + rnorm(1, mean = 0, sd = 0.2 * medium_survive)
    dark_survive <- dark_survive + rnorm(1, mean = 0, sd = 0.2 * dark_survive)
  }

  # Prevent pop from becoming negative
  light_survive <- max(c(0, light_survive))
  medium_survive <- max(c(0, medium_survive))
  dark_survive <- max(c(0, dark_survive))

  # "Migration" Rate, a hacky solution to reintroduce variation in the population
  if (migration == T) {

    n_migrants <- pop_size * 0.005 # half percent of initial pop size

    # which color are they?
    moth_color <- sample(c("light", "medium", "dark"), size = n_migrants, replace = T)

    light_survive <- light_survive + sum(moth_color == "light")
    medium_survive <- medium_survive + sum(moth_color == "medium")
    dark_survive <- dark_survive + sum(moth_color == "dark")
  }

  # Reproduction, with exponential growth and asexual reproduction
  light_survive <- round(exp(0.15) * light_survive)
  medium_survive <- round(exp(0.15) * medium_survive)
  dark_survive <- round(exp(0.15) * dark_survive)

  # survey total pop size, record fixation
  pops <- c(light_survive, medium_survive, dark_survive)
  if (is.na(fixation) & sum(pops != 0) == 1) fixation <- t

  # Recording each pop. size (virtual ecology!)
  moth_df[t + 1, 1] = light_survive
  moth_df[t + 1, 2] = medium_survive
  moth_df[t + 1, 3] = dark_survive
  moth_df[t + 1, 4] = pollution
} # end simulation
################################################
fixation <- ifelse(is.na(fixation), t + 1, fixation) # record time + 1 if fixation not yet reached
moth_list <- list(moth_df = moth_df, parms = parms, fixation = fixation)

return(moth_list)
}
}


#' @title plot_moth
#' @description Plots the results of peppered moths simulation
#' @param sim moth simulation data, created by moth_sim()
#' @param plot_fixation Logical, if TRUE (T), plots a vertical line for the generation that the population hit fixation. If FALSE (F), the default, then don't plot the line.
#' @return three-panel plot
#' @export

plot_moth <- function(x, plot_fixation = F) {
  require(magrittr)
  require(ggplot2)
  require(dplyr)
  require(patchwork)
  require(tidyr)

  moth_df <- x$moth_df %>%
    mutate(Time = 0:(n() - 1))

  d_fix <- data.frame(fix_time = x$fixation)

  pop_df <- moth_df %>%
    group_by(Time) %>%
    pivot_longer(names_to = "color", values_to = "n", -c(Time, pol)) %>%
    mutate(color = factor(color, levels = c("light", "medium", "dark")))

  prop_df <- pop_df %>%
    group_by(Time) %>%
    mutate(total_pop = sum(n)) %>%
    ungroup() %>%
    mutate(prop = n / total_pop)

  # Raw count of moths over time
  p_moth_pop <- pop_df %>%
    ggplot(aes(x = Time, y = n, color = color)) +
    geom_line(alpha = 0.7) +
    ggtitle("Moth Pop Size") +
    ylab("") +
    xlab("") +
    scale_color_manual(values = c("orange", "red", "darkred")) +
    theme_bw(base_size = 16) +
    theme(legend.title = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.title = element_text(size = 16)) +
    guides(colour = guide_legend(override.aes = list(size = 2)))

  # Pollution over time
  p_pol <- moth_df %>%
    ggplot(aes(x = Time, y = pol, alpha = pol)) +
    geom_line(lwd = 0.5) +
    ggtitle("Pollution Level") +
    ylab("") +
    scale_y_continuous(limits = c(0, 1), breaks = c(0, 1), labels = c("Min", "Max")) +
    theme_bw(base_size = 16) +
    theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.title = element_text(size = 16)) +
    guides(colour = guide_legend(override.aes = list(size = 2)))

  # Proportion of moths over time
  p_moth_prop <- prop_df %>%
    ggplot(aes(x = Time, y = prop, color = color)) +
    geom_line(alpha = 0.7) +
    ggtitle("Moth Color Proportion") +
    ylab("") +
    xlab("") +
    scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", "0.5", "1")) +
    scale_color_manual(values = c("orange", "red", "darkred")) +
    theme_bw(base_size = 16) +
    theme(legend.title = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.title = element_text(size = 16)) +
    guides(colour = guide_legend(override.aes = list(size = 2)))

  # Patch together ###

  if (plot_fixation == T) {
    p_moth_pop <- p_moth_pop + geom_vline(data = d_fix, aes(xintercept = fix_time), linetype = "dashed")

    p_moth_prop <- p_moth_prop + geom_vline(data = d_fix, aes(xintercept = fix_time), linetype = "dashed")

    p_moth_pop / p_moth_prop / p_pol +
      plot_layout(guides = 'collect')
  }

  else p_moth_pop / p_moth_prop / p_pol +
    plot_layout(guides = 'collect')

}


