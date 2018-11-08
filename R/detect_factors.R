#' find if a factor is actually character
#'
#' In order to check if a factor is a character,this function checks
#'
#' whether there are repeated value in the factor
#' @param x a factor
#'
#' @usage
#' detect_factors(f)
#'
#' @return boolean; \code{TRUE} if input is factor,
#'         \code{FALSE} if input is character
#' @export
#'
#' @examples
#' detect_factors(factor(c("a","b","a"))) # True
#' detect_factors(factor(c("a","b","c"))) # False
detect_factors <- function(x){
  # check if the input is factor
  if(!is.factor(x)){
    stop('Please enter a factor!\n',
         'The input is: ', class(x)[1])
  }
  # If the length of input is not equal to unique ones return true
  return(length(unique(x)) != length(x))
}
