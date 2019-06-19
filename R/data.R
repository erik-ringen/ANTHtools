#' @title AgtaForaging
#' @description Thomas Headland's Agta foraging data, exported from Koster et al.'s THE LIFE HISTORY OF HUMAN FORAGING:
#' CROSS-CULTURAL AND INDIVIDUAL VARIATION
#' @format A data frame with 179 rows and 6 variables:
#' \describe{
#'   \item{\code{trip_id}}{integer index for hunting trip}
#'   \item{\code{harvest}}{double kg of meat harvested}
#'   \item{\code{group_size}}{integer number of foragers on trip}
#'   \item{\code{pooled}}{double binary indicator of whether the harvest was shared across foragers, 1 = yes}
#'   \item{\code{zero_return}}{double binary indicator of whether the hunt failed, 1 = yes}
#'   \item{\code{trip_duration}}{length of foraging trip, in hours}
#'}
#' @source \url{https://www.biorxiv.org/content/biorxiv/early/2019/03/12/574483.full.pdf}
"AgtaForaging"

#' @title Buss1989
#' @description Summary statistics from David Buss's 1989 study "Sex differences in human mate preferences: Evolutionary hypotheses tested in 37 cultures". Caution: these data were scraped directly from an old pdf and have not been vetted for errors!
#' @format A data frame with 148 rows and 6 variables:
#' \describe{
#'   \item{\code{Sample}}{integer Sample population}
#'   \item{\code{Region}}{integer World region for sample}
#'   \item{\code{question}}{character Which study item}
#'   \item{\code{Mean_F}}{double Mean value for female respondents}
#'   \item{\code{Mean_M}}{double Mean value for male respondents}
#'   \item{\code{SD_F}}{double Standard deviation for female respondents}
#'   \item{\code{SD_M}}{double Standard deviation for male respondents}
#'}
"Buss1989"
