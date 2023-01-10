#' \code{remove_duplicates_df()}
#'
#' @description Removes duplicate rows from a data frame (determined based on a user-declared set of columns) and, optionally,
#' saves the removed rows to a data frame.
#'
#' @details \code{remove_duplicates_df()} uses a user-declared set of variables to determine which rows in a data frame
#' are duplicates of others, and it removes all but the first instance of each duplicated row. The removed rows can be
#' saved to a data frame (determined by the \code{save_removed_rows_as} argument), which is saved to the global environment.
#' If the \code{save_removed_rows_as} argument is \code{NULL}, the removed rows are not saved. The function checks if the name
#' given in \code{save_removed_rows_as} is the name of an object in the global environment already, and if so, it
#' appends five random numbers to the end of the data frame's name.
#' In effect, \code{remove_duplicates_df()}  is \code{dplyr::distinct(dat, var1, var2, ...)} with a bit more information in
#' the output and the ability to save removed rows to a data frame.
#'
#' @param dat A data frame.
#' @param column_names A character vector containing column names on which to base the assessment of row duplication.
#' If \code{NULL}, \code{column_names} is set to \code{colnames(dat)}.
#' @param save_removed_rows_as The name of the data frame in which to save removed (duplicate) rows. \code{NULL} or a character vector with a length of one.
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
remove_duplicates_df <- function(dat, column_names, save_removed_rows_as = 'removed') {
  if (is.null(column_names)) { column_names <- colnames(dat) }
  if (!is.data.frame(dat)) {
    stop('dat must be a data frame', call. = FALSE)
  }
  if (!is.vector(column_names, mode = 'character')) {
    stop('column_names argument must be a character vector', call. = FALSE)
  }
  if (!all(column_names %in% colnames(dat))) {
    stop('All elements of column_names must be column names in the dat object', call. = FALSE)
  }

  if (!is.null(save_removed_rows_as)) {
    if (is.vector(save_removed_rows_as, mode = 'character') == FALSE | length(save_removed_rows_as) != 1) {
      stop('If not NULL, save_removed_rows_as argument must be a character vector with a length of 1', call. = FALSE)
    }
    if (save_removed_rows_as %in% ls(name = .GlobalEnv)) {
      while (save_removed_rows_as %in% ls(name = .GlobalEnv)) {
        orig_save_removed_rows_as <- save_removed_rows_as
        save_removed_rows_as <- paste0(orig_save_removed_rows_as, paste0(sample(0:9, 5, replace = TRUE), collapse = ''))
      }
      warning(paste0('save_removed_rows_as argument matches an object in the global environment;\n',
                     'removed rows will be saved as ', save_removed_rows_as, ' instead'), call. = FALSE)
    }
    assign(save_removed_rows_as, dat[duplicated(dat[, column_names]), ], envir = .GlobalEnv)
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
