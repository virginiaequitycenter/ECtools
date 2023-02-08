#' \code{extract_zip()}
#'
#' @description Extracts five-digit ZIP Codes from addresses.
#'
#' @details \code{extract_zip()} extracts five-digit ZIP Codes from addresses in a vector.
#' ZIP Codes in the address vector can be either five-digit or nine-digit (hyphenated); only five-digit ZIP Codes will be returned.
#' If multiple candidate ZIP Codes are identified for a given address string, the \code{if_multiple} argument governs which is returned
#' (\code{'first'}, \code{'last'}, or \code{'all'}; \code{'first'} is default; if \code{'all'}, multiple candidate ZIPs for a given string are returned separated by \code{' | '}).
#' Specified ZIP Codes can be optionally converted to \code{NA} if extracted (by default, the common placeholders \code{'00000'} and \code{'99999'} are converted to \code{NA}).
#' By default, the function only identifies ZIP Codes that follow US state names or abbreviations (including DC).
#'
#' @param address A vector containing one or more address strings from which to extract ZIP Codes. Character or character-coercible.
#' @param if_multiple If multiple candidate ZIP Codes are detected in a string, which should be returned, \code{'first'},
#' \code{'last'}, or \code{'all'}? Case-insensitive; \code{'first'} is default.
#' @param must_follow_state If \code{TRUE}, only extract five-digit ZIP Codes that follow (case-insensitive) US state names or abbreviations (including DC). \code{TRUE} is default.
#' @param auto_NA A vector containing ZIP Codes that should be converted to \code{NA} if extracted.
#' The common placeholders \code{'00000'} and \code{'99999'} are the default. Set to \code{NA} to convert no ZIPs to \code{NA}.
#'
#' @return A vector.
#'
#' @examples
#' addresses <- c('NORFOLK, VA 23501',
#'                '100 MAIN ST, ALEXANDRIA, VA 22301-9999',
#'                '11111A 1ST ST, ROANOKE, VA 24001 & 22222B 2ND ST, ROANOKE, VA 24001',
#'                'RICHMOND, VA 00000', 'RICHMOND, VA 99999')
#' # Extract the last ZIP Code in each string:
#' extract_zip(addresses, if_multiple = 'last')
#' # Extract all ZIP Codes in each string:
#' extract_zip(addresses, if_multiple = 'all')
#' # With must_follow_state set to FALSE, 11111 and 22222 are
#' # errantly picked up in the last string:
#' extract_zip(addresses, if_multiple = 'all', must_follow_state = FALSE)
#' # With auto_NA set to NA, 00000 and 99999 are left as is:
#' extract_zip(addresses, if_multiple = 'last', auto_NA = NA)
#'
#' @export
extract_zip <- function(address, if_multiple = 'first', must_follow_state = TRUE, auto_NA = c('00000', '99999')) {
  if (is.character(address) == FALSE) {
    address <- as.character(address)
  }
  if ((must_follow_state %in% c(TRUE, FALSE)) == FALSE) {
    warning("must_follow_state is not TRUE or FALSE; defaulting to TRUE", call. = FALSE)
    must_follow_state <- TRUE
  }
  if (grepl(x = if_multiple, pattern = '^(?i)(first|last|all)$') == FALSE) {
    warning("if_multiple is not 'first', 'last', or 'all'; defaulting to 'first'", call. = FALSE)
    if_multiple <- 'first'
  }
  if ((all(nchar(auto_NA) == 5) | all(is.na(auto_NA))) == FALSE) {
    warning("auto_NA should contain only either (a) five-digit ZIP strings or (b) NA; defaulting to c('00000', '99999')", call. = FALSE)
    auto_NA = c('00000', '99999')
  }

  pattern <- ifelse(must_follow_state == FALSE,
                    '\\d{5}',
                    paste0('(?<=(?i)(',
                           paste0(paste0(datasets::state.abb, collapse = '|'), '|', paste0(datasets::state.name, collapse = '|'), '|D\\.?C|District of Columbia'),
                           ')\\.? )\\d{5}'))

  zips <- stringi::stri_extract(str = address, regex = pattern, mode = tolower(if_multiple))

  if (all(is.na(auto_NA)) == FALSE) {
    zips <- lapply(zips, function(x) ifelse(x %in% auto_NA, NA, x))
  }

  zips <- sapply(zips, function(x) paste0(x, collapse = ' | '))

  ifelse(zips == 'NA', NA, zips)

}
