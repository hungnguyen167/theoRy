# --- theoRy engine JSON-to-data-frame helpers -------------------------------

`%||%` <- function(x, y) if (is.null(x)) y else x

#' Convert a list of backend JSON records to a data frame
#'
#' Handles \code{NULL} values from JSON \code{null} by replacing them with
#' \code{NA} (of appropriate type) so that \code{as.data.frame()} and
#' \code{rbind()} work correctly.
#'
#' @param records A list of record lists, each a named list.
#' @param col_types Named character vector of R types for required columns
#'   (e.g. \code{c(target = "character", direction = "character")}).
#' @return A data frame with one row per record.
#'
#' @keywords internal
records_to_df <- function(records, col_types = NULL) {
  if (length(records) == 0) {
    return(data.frame())
  }

  # Replace NULL with NA in every record
  records <- lapply(records, function(r) {
    lapply(r, function(v) if (is.null(v)) NA else v)
  })

  df <- do.call(rbind, lapply(records, as.data.frame,
                               stringsAsFactors = FALSE))
  rownames(df) <- NULL

  if (!is.null(col_types)) {
    for (col in names(col_types)) {
      if (col %in% names(df)) {
        if (col_types[col] == "character") {
          df[[col]] <- as.character(df[[col]])
        } else if (col_types[col] == "integer") {
          df[[col]] <- as.integer(df[[col]])
        } else if (col_types[col] == "numeric") {
          df[[col]] <- as.numeric(df[[col]])
        } else if (col_types[col] == "logical") {
          df[[col]] <- as.logical(df[[col]])
        }
      }
    }
  }

  df
}
