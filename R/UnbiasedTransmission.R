#' @title UnbiasedTransmission
#' @description Agent-based model simulation of unbiased tranmission, only slightly modified from Alex Mesoudi's code here: https://github.com/amesoudi/cultural_evolution_ABM_tutorial/blob/master/ABMmodels_model1.Rmd
#' @param N Population size
#' @param t_max How many generations to run the sim.
#' @param add Whether or not the new sim should be added to an existing plot; defaults to FALSE.
#' @param col Color of lines for plotting; defaults to black.
#' @export

UnbiasedTransmission <- function (N, t_max, add=FALSE, col='black') {

  agent <- data.frame(trait = sample(c("A","B"), N, replace = TRUE), stringsAsFactors = FALSE)

  output <- data.frame(p = rep(NA, t_max))

  output$p[1] <- sum(agent$trait == "A") / N

  for (t in 2:t_max) {

    previous_agent <- agent  # copy agent to previous_agent dataframe

    agent <- data.frame(trait = sample(previous_agent$trait, N, replace = TRUE))  # randomly copy from previous generation

    output$p[t] <- sum(agent$trait == "A") / N  # get p and put it into output slot for this generation t

  }

  if (add == FALSE) {
  plot(output$p, type = 'l', ylab = "p, proportion of agents with trait A", xlab = "generation", ylim = c(0,1), main = paste("N =", N), col=col, lwd=1.5)
  }

  if (add == TRUE) {
    lines(x=seq(from=0,to=t_max-1), y=output$p, col=col, lwd=1.5)
  }

}
