#' \code{assign_quarter()}
#'
#' @description Determines the quarter of the year associated with each date in a vector.
#'
#' @details \code{assign_quarter()} determines the quarter of the year (Q1/Jan-Mar, Q2/Apr-Jun, etc.) associated with each date in a vector.
#' The vector can be character or date class. If the vector is date class, the function can simply be run as \code{assign_quarter(dates)}.
#' If the vector is character class, a format (e.g., "\%Y-\%m-\%d") must be supplied via the \code{format} argument.
#'
#' The function is designed to handle some common scenarios where \code{quarters()} would error. For example, in the following example,
#' \code{quarters()} errors because the vector isn't Date classed:
#'
#' \code{dates <- c('1999/01/01', 'ABC'); quarters(dates)}
#'
#' \code{assign_quarter()} can handle that case all in one step. For a vector that isn't Date classed, \code{assign_quarter()} uses a
#' \code{format} argument to convert strings to dates, and quarters are correspondingly returned:
#'
#'\code{dates <- c('1999/01/01', 'ABC'); assign_quarter(dates, format = '\%Y/\%m/\%d')}
#'
#' Further, when a quarter can't be determined because an element of the Date-classed vector is \code{NA} (either because it was NA in the original
#' dates vector or because it was converted to NA during date conversion), \code{assign_quarter()} returns \code{NA}; \code{quarters()} would return "QNA".
#'
#' @param dates A vector containing one or more dates for which to assign quarters. Date or character class.
#' @param format The format of the dates; used if the dates vector is in character format. The format is specified as described in \code{?strptime()}. E.g., "\%Y-\%m-\%d".
#' @param return_QX Should the function return quarters as "Jan-Mar", "Apr-Jun", etc., or as "Q1", "Q2", etc.? \code{TRUE} or \code{FALSE}; \code{TRUE} is default.
#'
#' @return A character vector.
#'
#' @examples
#' dates <- c('2022/01/01', '1900/12/01', '1950/06/01', 'ABC')
#' assign_quarter(dates, format = '%Y/%m/%d', return_QX = TRUE)
#'
#' @export
assign_quarter <- function(dates, format = NA, return_QX = TRUE) {
  if ((return_QX %in% c(TRUE, FALSE)) == FALSE) {
    warning("return_QX is not TRUE or FALSE; defaulting to TRUE",
            call. = FALSE, immediate. = TRUE)
    return_QX <- TRUE
  }

  if (inherits(dates, what = 'Date') == FALSE) {
    if (is.na(format)) {
      stop('If the dates argument is not "Date" class, a format argument must be provided.', call. = FALSE)
    } else {
      orig_NA_count <- sum(is.na(dates))
      dates <- as.Date(dates, format = format)
      if ((sum(is.na(dates)) - orig_NA_count) > 0) {cat((sum(is.na(dates)) - orig_NA_count), 'element(s) of dates vector unable to be converted to Date format.\n')}
    }
  }

  years <- ECtools::extract_year(dates, expect_modern = FALSE)
  quarter_vals <- dplyr::case_when(dates < as.Date(paste0(years, '-04-01')) ~ ifelse(return_QX == T, 'Q1', 'Jan-Mar'),
                                   dates >= as.Date(paste0(years, '-04-01')) & dates < as.Date(paste0(years, '-07-01')) ~ ifelse(return_QX == T, 'Q2', 'Apr-Jun'),
                                   dates >= as.Date(paste0(years, '-07-01')) & dates < as.Date(paste0(years, '-10-01')) ~ ifelse(return_QX == T, 'Q3', 'Jul-Sep'),
                                   dates >= as.Date(paste0(years, '-10-01')) ~ ifelse(return_QX == T, 'Q4', 'Oct-Dec'),
                                   TRUE ~ as.character(NA))

  if (sum(is.na(quarter_vals)) > 0) {cat('Quarters not successfully determined for', sum(is.na(quarter_vals)), 'element(s) of the dates vector.\n')}

  quarter_vals
}

