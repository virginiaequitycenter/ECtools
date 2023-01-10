#' \code{remove_duplicates_df()}
#'
#' @description Removes duplicate rows from a data frame, determined based on a user-declared set of columns.
#'
#' @details \code{remove_duplicates_df()} uses a user-declared set of variables to determine which rows in a data frame
#' are duplicates of others, and it removes all but the first instance of each duplicated row.
#' In effect, it's \code{dplyr::distinct(dat, var1, var2, ...)} with a bit more information in the output.
#'
#' @param dat A data frame.
#' @param column_names A character vector containing column names on which to base the assessment of row duplication.
#' If \code{NULL}, \code{column_names} is set to \code{colnames(dat)}.
#'
#' @return A data frame.
#'
#' @examples
#' d <- data.frame(year = c(1999, 1999, 1999, 1999),
#'                 name = c('jane', 'jane', 'mark', 'erin'),
#'                 misc = c(1, 2, 3, 4))
#' remove_duplicates_df(d, c('year', 'name'))
#'
#' @export
remove_duplicates_df <- function(dat, column_names) {
  if (is.null(column_names)) { column_names <- colnames(dat) }
  if (!is.data.frame(dat)) {
    stop('dat must be a data frame', call. = FALSE)
  }
  if (is.vector(column_names, mode = 'character') == FALSE) {
    stop('column_names argument must be a character vector', call. = FALSE)
  }
  if (all(column_names %in% colnames(dat)) == FALSE) {
    stop('all elements of column_names must be column names in the dat object', call. = FALSE)
  }
  init_nrow <- nrow(dat)
  dat <- dat[!duplicated(dat[, column_names]), ]
  post_nrow <- nrow(dat)
  cat(paste0('Removed ', init_nrow - post_nrow, ' duplicate',
             ifelse(init_nrow - post_nrow == 1, '', 's'),
             ', as determined by the following variable',
             ifelse(length(column_names) == 1, ':', 's:'),
             ' ', paste0(column_names, collapse = ', '), '\n'))
  dat
}
