#' @title makeTransparent
#' @description Convienience function for applying transparency to a color, taken from Ricardo Oliveros-Ramos' stackoverflow post: https://stackoverflow.com/questions/8047668/transparent-equivalent-of-given-color
#' @param alpha How transparent should the color be? Between 0 and 1
#' @export

makeTransparent <- function(..., alpha=0.5) {

  if(alpha<0 | alpha>1) stop("alpha must be between 0 and 1")

  alpha = floor(255*alpha)
  newColor = col2rgb(col=unlist(list(...)), alpha=FALSE)

  .makeTransparent = function(col, alpha) {
    rgb(red=col[1], green=col[2], blue=col[3], alpha=alpha, maxColorValue=255)
  }

  newColor = apply(newColor, 2, .makeTransparent, alpha=alpha)

  return(newColor)
}
