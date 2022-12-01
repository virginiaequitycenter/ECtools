#' \code{correct_punctuation_spacing()}
#'
#' @description Eliminate spaces before and ensures spaces after designated punctuation marks.
#'
#' @details \code{correct_punctuation_spacing()} eliminates spaces before and ensures spaces after punctuation marks declared in
#' the \code{punctuation} argument. For example, "SMITH ,JANE" would be corrected to "SMITH, JANE" (assuming that a comma
#' is included among the designated punctuation marks via the \code{punctuation} argument. The \code{punctuation} argument has
#' a default value of \code{c(',', ';', ':')}. Before correcting spacing around the designated marks, the function checks whether
#' each entry in the \code{punctuation} argument is indeed a punctuation mark, and it will remove any that are not identified as such (and alert the user via a warning).
#'
#' @param name A character vector containing one or more strings in which to correct spacing around punctuation marks.
#' @param punctuation A character vector containing punctuation marks to correct spacing around; \code{c(',', ';', ':')} is default.
#'
#' @return A character vector.
#'
#' @examples
#' correct_punctuation_spacing(c('SMITH ,JANE', 'SMITH,JANE', 'SMITH , JANE'),
#'                             punctuation = c(',', ';', ':', 'ABC'))
#'
#' @export
correct_punctuation_spacing <- function(name, punctuation = c(',', ';', ':')) {
  if (is.character(name) == FALSE) {
    name <- as.character(name)
  }
  if (is.character(punctuation) == FALSE) {
    warning("punctuation argument must be a character vector; defaulting to c(',', ';', ':')",
            call. = FALSE, immediate. = TRUE)
    punctuation <- c(',', ';', ':')
  }

  is_punctuation <- grepl(x = punctuation, pattern = '^[[:punct:]]$')
  if (all(is_punctuation) == FALSE) {
    warning(paste0('All entries in punctuation argument must be punctuation marks\n',
                   '\tRemoving from punctuation argument: ', paste0(punctuation[is_punctuation == F], collapse = ' '), '\n',
                   '\tKeeping in punctuation argument: ', paste0(punctuation[is_punctuation], collapse = ' ')),
            call. = FALSE, immediate. = TRUE)
    punctuation <- punctuation[is_punctuation]
  }

  punctuation_pattern <- paste0('(\\', paste0(punctuation, collapse = '|\\'), ')')

  cat(paste0('Spacing errors around punctuation are corrected by:\n',
             '\tCollapsing spaces before the designated punctuation marks (e.g., "SMITH , JANE" --> "SMITH, JANE")\n',
             '\tEnsuring spaces after the designated punctuation marks (e.g., "SMITH,JANE" --> "SMITH, JANE")\n'))

  name <- gsub(x = name, pattern = paste0(punctuation_pattern, '(\\S)'), replacement = '\\1 \\2') |>
    gsub(x = _, pattern =   paste0('(\\S)(\\s{1,})', punctuation_pattern), replacement = '\\1\\3')

  name
}
