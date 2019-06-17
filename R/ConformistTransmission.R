#' @title ConformistTransmission
#' @description Agent-based model simulation of conformist tranmission, only slightly modified from Alex Mesoudi's code here: https://github.com/amesoudi/cultural_evolution_ABM_tutorial/blob/master/ABMmodels_model3.Rmd
#' @param N Population size
#' @param p_0 Proportion of agents with trait A at the start of the simulation.
#' @param D Strength of conformity
#' @param t_max How many generations to run the sim.
#' @param add Whether or not the new sim should be added to an existing plot; defaults to FALSE.
#' @param col Color of lines for plotting; defaults to black.
#' @export

ConformistTransmission <- function (N, p_0, D, t_max, add=FALSE, col="black" ) {

  output <- rep(NA,t_max)  # create a matrix with t_max rows and r_max columns, filled with NAs, then convert to data.frame

    agent <- data.frame(trait = sample(c("A","B"), N, replace = TRUE, prob = c(p_0,1-p_0)), stringsAsFactors = FALSE)  # create first generation

    output[1] <- sum(agent$trait == "A") / N  # add first generation's p to first row of column r

    for (t in 2:t_max) {

      # create dataframe with a set of 3 randomly-picked demonstrators for each agent
      demonstrators <- data.frame(dem1 = sample(agent$trait, N, replace = TRUE), dem2 = sample(agent$trait, N, replace = TRUE), dem3 = sample(agent$trait, N, replace = TRUE), stringsAsFactors = F)
      # get the number of As in each 3-dem combo
      numAs <- rowSums(demonstrators == "A")
      agent$trait[numAs == 3] <- "A"  # for dem combos with all As, set to A
      agent$trait[numAs == 0] <- "B"  # for dem combos with no As, set to B
      prob <- runif(N)

      # when A is a majority, 2/3
      agent$trait[numAs == 2 & prob < (2/3 + D/3)] <- "A"
      agent$trait[numAs == 2 & prob >= (2/3 + D/3)] <- "B"
      # when A is a minority, 1/3
      agent$trait[numAs == 1 & prob < (1/3 - D/3)] <- "A"
      agent$trait[numAs == 1 & prob >= (1/3 - D/3)] <- "B"
      output[t] <- sum(agent$trait == "A") / N  # get p and put it into output slot for this generation t and run r

    }

  # Plotting sim results
if (add == FALSE) {
  plot(output ~ seq(from=0,to=t_max-1), xlab='generation', ylab='p, proportion of agents with trait A', type='l', ylim=c(0,1), col=col)
    abline(h=0.5, lty='dashed')
}
    else {
      lines(x=seq(from=0,to=t_max-1), y=output, col=col)
    }
}

