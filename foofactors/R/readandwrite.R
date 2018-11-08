#' @title write data frames to plain text delimited
#'
#' @description Write data frame to plain text delimited files
#'
#' @usage writedataframe(dataframe, file, level)
#'
#' @param dataframe is dataframe
#'
#' @param file indicates where is the dataframe
#'
#' @param level is where to store the file
#'
#' @export

dfwrite <- function(df, dffilename, lvfilename = NA) {
  # check if the input is a factor or not
  dfcheck <- function(x) {
    # check if the input is a data frame or not
    if (!is.data.frame(x)) {
      stop("Function requires a data frame instead of a ", class(x)[1])
    }
  }
  # write data frame using write_csv()
  readr::write_csv(df, dffilename)
  # get names of columns of factors
  factor_cols <- names(Filter(is.factor, df))
  # check filename for levels
  if (is.na(lvfilename)) {
    # use dirname() to get path of filename
    lvfilename <- paste0(dirname(dffilename), "/", "levels.txt")
  }
  # write levels to companion file
  # use lapply() and levels() to get levels of factor columns
  dput(lapply(df[factor_cols], levels), lvfilename)
}

#' @title  read data frames from plain text delimited
#'
#' @description read data frame from plain text delimited files
#'
#' @usage readdataframe(file, level)
#'
#' @param dataframe is dataframe
#'
#' @param level is where to store the file
#'
#' @export

dfread <- function(file, level) {
  # write data frame using read_csv()
  ret <- readr::read_csv(dffilename)
  # check filename for levels
  if (is.na(lvfilename)) {
    # use dirname() to get path of filename
    lvfilename <- paste0(dirname(dffilename), "/", "levels.txt")
  }
  # get levels from companion file
  lvs <- dget(lvfilename)
  # set levels of data frame
  for (i in seq_along(lvs)) {
    # first convert columns to factor
    ret[[names(lvs[i])]] <- as.factor(ret[[names(lvs[i])]])
    # set levels
    levels(ret[[names(lvs[i])]]) <- lvs[[i]]
  }
  # return data frame
  return(ret)
}
