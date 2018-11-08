#' @title Set the levels "as is"
#'
#' @description Set levels of a factor to the order in which they appear in the data.
#'
#' @usage
#' newfactor(f)
#'
#' @param f a factor.
#'
#' @return
#' a factor with levels in the order of f.
#' If the input is not a factor, the function will raise an error
#'
#' @examples
#' newfactor(factor(c("s", "t", "a"))) # Levels: s t a
#'
#' @export
newfactor <- function(x){
  if(!is.factor(x)){
    stop('Please enter a factor!\n',
         'You entered: ', class(x)[1])
  }

  return(factor(x, as.character(unique(x))))
}

#' @title Set the levels reversed
#'
#' @description
#' Set levels of a factor to the reversed order in which they appear in the data.
#'
#' @param f a factor
#'
#' @return a factor with levels in the reverse order appeared in x.
#' @export
#'
#' @examples newrev(factor(c("s","t","a","t")))
newrev <- function(x){
  if(!is.factor(x)){
    stop('Please enter a factor!\n',
         'You entered: ', class(x)[1])
  }
  return(factor(x, rev(as.character(unique(x)))))
}
