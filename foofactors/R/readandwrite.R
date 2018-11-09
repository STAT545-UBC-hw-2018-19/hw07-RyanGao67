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

dfwrite <- function(dataframe, file, level=NA) {
  # make sure the input is a factor
  if (!is.data.frame(dataframe)) stop("Not a data frame ", class(dataframe)[1])
  readr::write_csv(dataframe, file)
  # if the level is not given in the parameter
  # we need a default one
  if (is.na(level)) {
    level <- paste0(dirname(file), "/", "levels.txt")
  }
  dput(lapply(dataframe[names(Filter(is.factor, dataframe))], levels), level)
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

dfread <- function(file, level = NA) {
  # write data frame using read_csv()
  ret <- readr::read_csv(file)
  # check filename for levels
  if (is.na(level)) {
    # use dirname() to get path of filename
    level <- paste0(dirname(file), "/", "levels.txt")
  }
  # get levels from companion file
  lvs <- dget(level)
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
