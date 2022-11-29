#' \code{extract_zip()}
#'
#' @description Extracts five-digit ZIP Codes from addresses.
#'
#' @details \code{extract_zip()} extracts five-digit ZIP Codes from addresses stored in a vector.
#' ZIP Codes in the address vector can either be five-digit or nine-digit; only five-digit ZIP Codes will be returned.
#' If multiple candidate ZIP Codes are identified for a given address string, the \code{if_multiple} argument governs which is returned,
#' the first or the last (the last is the default).
#'
#' @param address A vector containing one or more address strings from which to extract ZIP Codes. Character or character-coercible.
#' @param if_multiple If multiple ZIP Code candidates are detected in a string, which should be returned, the first or the last?
#' \code{'first'} or \code{'last'}, case-insensitive; \code{'last'} is default.
#'
#' @return A vector.
#'
#' @examples
#' addresses <- c('NORFOLK, VA 23501',
#'                '100 MAIN ST, ALEXANDRIA, VA 22301',
#'                '99999 FIRST ST, ROANOKE, VA 24001-9999')
#' extract_zip(addresses, if_multiple = 'last')
#'
#' @export
extract_zip <- function(address, if_multiple = 'last') {
  if (is.character(address) == F) {
    address <- as.character(address)
  }
  if (grepl(x = if_multiple, pattern = '^(?i)(first|last)$') == F) {
    warning("if_multiple is not one of 'first' or 'last'; defaulting to 'last'", call. = F)
    if_multiple <- 'last'
  }
  stringi::stri_extract(str = address, regex = '\\d{5}(?<=(\\-\\d{4})?)', mode = tolower(if_multiple))
}
