#' \code{expand_shorthand()}
#'
#' @description Expands shorthands and acronyms in a vector of strings based on a table(s) included in the package.
#'
#' @details \code{expand_shorthand()} expands shorthands and acronyms in a vector of strings according to table(s) included in the package. The function currently
#' offers expansions for housing-related terms, with more to come.
#'
#' @param to_expand A vector containing one or more strings in which to expand shorthands/acronyms. Character or character-coercible.
#' @param type The "genre" of expansions to perform; currently, only housing-related expansions are provided.
#' @param case_out Should the \code{to_expand} vector, with expansions, be returned in uppercase or lowercase? Uppercase is default.
#'
#' @return A character vector.
#'
#' @examples
#' example_names <- c('GREENE APTS', 'BLU MGMT', 'REDD PROPERTY MGT')
#' expand_shorthand(to_expand = example_names, type = 'housing', case_out = 'upper')
#'
#' @export
expand_shorthand <- function(to_expand, type = 'housing', case_out = 'upper') {
  available_types <- dir(system.file('extdata', package = 'ECtools')) |> gsub(x = _, pattern = '\\.csv', replacement = '')
  if ((type %in% available_types) == F) {
    stop(paste0('type must be one of: ', paste0(available_types, collapse = ', ')), call. = F)
  }

  if ((case_out %in% c('upper', 'lower')) == F) {
    warning("case_out argument is not one of 'upper' or 'lower'; defaulting to 'upper'",
            call. = FALSE, immediate. = TRUE)
    case_out <- 'upper'
  }

  expansions <- utils::read.csv(system.file('extdata', paste0(type, '.csv'), package = 'ECtools', mustWork = TRUE))
  for (i in 1:nrow(expansions)) {
    pattern <- paste0('(?i)(\\b', expansions$shorthand[i], '\\b)')
    to_expand <- stringi::stri_replace_all(to_expand, regex = pattern, replacement = expansions$expansion[i])
  }

  to_expand <- if (case_out == 'upper') {
    to_expand <- toupper(to_expand)
  } else if (case_out == 'lower') {
    to_expand <- tolower(to_expand)
  }

  to_expand
}
