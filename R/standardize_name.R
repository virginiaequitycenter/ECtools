#' \code{standardize_name()}
#'
#' @description Standardizes names by removing or transforming various special characters and by eliminating excess white space.
#'
#' @details \code{standardize_name()} performs several regular-expression standardization procedures to reduce variability in the formatting of names.
#' The function:
#'
#' \itemize{
#'   \item Removes leading and trailing white space
#'   \item Converts dashes and forward slashes to spaces
#'   \item Removes the following characters: . ; ( ) [ ] { } # : _
#'   \item Removes errant commas at the ends of strings
#'   \item Converts at signs and ampersands to "at" and "and" (and forces surrounding spaces)
#'   \item Eliminates excess white spaces (e.g., errant double-spaces)
#' }
#'
#' @param name A character vector containing one or more strings to standardize.
#' @param case_out The desired case of the output vector ("upper" or "lower"; "upper" is default).
#'
#' @return A character vector.
#'
#' @examples
#' standardize_name(c(' _MAGNOLIA-&-FIR_ #COMPANY#     L.L.C.,',
#'                    '{MAGNOLIA} /AND/ (F)(I)(R) [COMPANY]... LLC, ',
#'                    'SMITH, LILY-MAY A.',
#'                    'SMITH, LILY MAY A,'),
#'                  case_out = 'upper')
#'
#' @export
standardize_name <- function(name, case_out = 'upper') {
  if (is.character(name) == F) {
    name <- as.character(name)
  }
  if ((case_out %in% c('upper', 'lower')) == F) {
    warning("case_out argument is not 'upper' or 'lower'; defaulting to 'upper'",
            call. = FALSE, immediate. = TRUE)
    case_out <- 'upper'
  }

  cat(paste0('Names are standardized by:\n',
             '\tRemoving leading and trailing spaces (" SMITH, MARY " \u2192 "SMITH, MARY")\n',
             '\tConverting dashes and forward slashes to single spaces ("-" and "/" \u2192 " ")\n',
             '\tRemoving the following characters: . ; ( ) [ ] { } # : _\n',
             '\tRemoving errant commas at the ends of strings ("ABC," \u2192 "ABC")\n',
             '\tConverting @ signs to "at" (and forcing surrounding spaces)\n',
             '\tConverting ampersands (&) to "and" (and forcing surrounding spaces)\n',
             '\tConverting all instances of >1 space to single spaces ("  " \u2192 " ")\n'))

  name <- gsub(x = name, pattern = '^\\s{1,}|\\s{1,}$', replacement = '') |>
    gsub(x = _, pattern = '\\-|\\/', replacement = ' ') |>
    gsub(x = _, pattern = '\\.|\\;|\\(|\\)|\\[|\\]|\\{|\\}|\\#|\\:|\\_', replacement = '') |>
    gsub(x = _, pattern = '\\,$', replacement = '') |>
    gsub(x = _, pattern = '\\@', replacement = ' at ') |>
    gsub(x = _, pattern = '\\&', replacement = ' and ') |>
    gsub(x = _, pattern = '\\s{2,}', replacement = ' ')

  if (case_out == 'upper') {
    toupper(name)
  } else {
    tolower(name)
  }

}
