#' @title descend a factor
#'
#' @description A new version of reorder that uses desc
#'
#' @usage newreorder(f)
#'
#' This function give you the ability to sort your factors in descending order.
#'
#' @param x a factor or a vector
#' @return a factor
#' @export
#'
#' @examples new_reorder(factor(c("c","b","a")))

newreorder <- function(x){
  # check if the input is factor
  if(!is.factor(x)){
    stop('Please enter a factor!\n',
         'You input a: ', class(x)[1])
  }
  # decend the factor
  return(stats::reorder(x, dplyr::desc(x)))
}
