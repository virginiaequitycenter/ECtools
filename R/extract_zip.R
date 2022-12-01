#' \code{extract_zip()}
#'
#' @description Extracts five-digit ZIP Codes from addresses.
#'
#' @details \code{extract_zip()} extracts five-digit ZIP Codes from addresses in a vector.
#' ZIP Codes in the address vector can either be five-digit or nine-digit; only five-digit ZIP Codes will be returned.
#' If multiple candidate ZIP Codes are identified for a given address string, the \code{if_multiple} argument governs which is returned
#' ('first', 'last', or 'all'; 'last' is default; if 'all', multiple candidate ZIPs for a given string are returned separated by ' | ').
#' By default, the function only identifies ZIP Codes following US state names and abbreviations.
#'
#' @param address A vector containing one or more address strings from which to extract ZIP Codes. Character or character-coercible.
#' @param if_multiple If multiple ZIP Code candidates are detected in a string, which should be returned, \code{'first'},
#' \code{'last'}, or \code{'all'}? Case-insensitive; \code{'last'} is default.
#' @param must_follow_state If \code{TRUE}, only match five-digit ZIP Codes that follow US state names or abbreviations
#' (case-insensitive). \code{TRUE} is default.
#'
#' @return A vector.
#'
#' @examples
#' addresses <- c('NORFOLK, VA 23501',
#'                '100 MAIN ST, ALEXANDRIA, VA 22301-9999',
#'                '11111A 1ST ST, ROANOKE, VA 24001 & 22222B 2ND ST, ROANOKE, VA 24001')
#' # Extract the last ZIP Code in each string:
#' extract_zip(addresses, if_multiple = 'last', must_follow_state = TRUE)
#' # Extract all ZIP Codes in each string:
#' extract_zip(addresses, if_multiple = 'all', must_follow_state = TRUE)
#' # With must_follow_state set to FALSE, 11111 and 22222 are
#' # errantly picked up in the last string:
#' extract_zip(addresses, if_multiple = 'all', must_follow_state = FALSE)
#'
#' @export
extract_zip <- function(address, if_multiple = 'last', must_follow_state = TRUE) {
  if (is.character(address) == F) {
    address <- as.character(address)
  }
  if ((must_follow_state %in% c(TRUE, FALSE)) == F) {
    warning("must_follow_state is not TRUE or FALSE; defaulting to TRUE", call. = F)
    must_follow_state <- TRUE
  }
  if (grepl(x = if_multiple, pattern = '^(?i)(first|last|all)$') == F) {
    warning("if_multiple is not 'first', 'last', or 'all'; defaulting to 'last'", call. = F)
    if_multiple <- 'last'
  }

  pattern <- ifelse(must_follow_state == F,
                    '\\d{5}',
                    paste0('(?<=(?i)(',
                           paste0(paste0(datasets::state.abb, collapse = '|'), '|', paste0(datasets::state.name, collapse = '|')),
                           ')\\s{1,})\\d{5}'))

  zips <- stringi::stri_extract(str = address, regex = pattern, mode = tolower(if_multiple))

  if (is.list(zips)) {
    zips <- sapply(zips, function(x) paste0(x, collapse = ' | '))
  }

  zips
}
