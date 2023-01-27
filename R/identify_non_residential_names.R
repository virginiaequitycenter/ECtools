#' \code{identify_non_residential_names()}
#'
#' @description Identifies non-residential names ("La Sucre Bakery" vs. "Jan La Sucre") based on an extensive regex pattern.
#'
#' @details \code{identify_non_residential_names()} applies a regex pattern to a character vector containing one or more names, returning
#' \code{TRUE} and \code{FALSE} for each element. The regex pattern used by default is taken from the GitHub repo jacob-gg/non-person-regex.
#' A user can, optionally, use a different regex by setting the \code{use_alt_regex} argument to \code{TRUE} and declaring
#' a connection; this, however, is usually not desired.
#'
#' @param name A vector containing one or more strings to identify as non-residential or residential. Character or character-coercible.
#' @param use_alt_regex Default (safe) is \code{FALSE}; if \code{TRUE}, the function will request a \code{source()}-able connection
#' to a file that, when executed, will return the regex to use to determine whether each element of \code{name} is non-residential or residential.
#'
#' @return A logical vector.
#'
#' @examples
#' example_names <- c('JANE SMITH', 'JANE SMITH APARTMENTS', 'AMERICANO CAFE')
#' identify_non_residential_names(example_names)
#'
#' @export
identify_non_residential_names <- function(name, use_alt_regex = FALSE) {
  if (is.character(name) == F) {
    name <- as.character(name)
  }
  if (use_alt_regex != FALSE) {
    con <- readline(prompt = 'Please enter a connection to the script from which you want to source the regex to use: ')
    pattern <- source(con, local = TRUE)[[1]]
  } else {
    pattern <- source('https://raw.githubusercontent.com/jacob-gg/non-person-regex/main/non_person_regex.R', local = TRUE)[[1]]
  }

  stringi::stri_detect(name, regex = pattern)
}
