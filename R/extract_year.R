#' \code{extract_year()}
#'
#' @description Extracts four-digit years from dates.
#'
#' @details \code{extract_year()} extracts four-digit years from dates stored in a vector.
#' Dates in the vector can be character or Date class, so long as a four-digit year is present somewhere in them.
#' If multiple candidate years are identified for a given date, the \code{if_multiple} argument governs which is returned,
#' the first or the last (the last is the default). The \code{expect_modern} argument determines whether to extract only
#' years in the 20th and 21st centuries.
#'
#' @param dates A vector containing one or more dates from which to extract years. Date or character class.
#' @param expect_modern Only extract 19XY and 20XY years? \code{TRUE} or \code{FALSE}; \code{TRUE} is default.
#' @param if_multiple If multiple year candidates are detected in a string, which should be returned, the first or the last?
#' \code{'first'} or \code{'last'}, case-insensitive; \code{'last'} is default.
#'
#' @return A character vector.
#'
#' @examples
#' dates <- c('01/01/1950', '01-01-1950',
#'            '2020-01-01', '2020/01/01',
#'            'Date: 2000-11-11', 'ABC 12-12-1999 XYZ')
#' extract_year(dates, expect_modern = TRUE, if_multiple = 'last')
#'
#' @export
extract_year <- function(dates, expect_modern = TRUE, if_multiple = 'last') {
  if ((is.character(dates) | inherits(dates, what = 'Date')) == F) {
    dates <- as.character(dates)
  }
  if ((expect_modern %in% c(TRUE, FALSE)) == F) {
    warning("expect_modern is not TRUE or FALSE; defaulting to TRUE", call. = F)
    expect_modern <- TRUE
  }
  if (grepl(x = if_multiple, pattern = '^(?i)(first|last)$') == F) {
    warning("if_multiple is not one of 'first' or 'last'; defaulting to 'last'", call. = F)
    if_multiple <- 'last'
  }
  pattern <- ifelse(expect_modern == TRUE, '(19|20)\\d{2}', '\\d{4}')
  stringi::stri_extract(str = dates, regex = pattern, mode = tolower(if_multiple))
}
