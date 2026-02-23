#' Load a file from inst/extdata/
#' @param filename Character filename (e.g. "nuts2_sea_basin.csv")
#' @return Data frame or object from .rds
#' @noRd
load_extdata <- function(filename) {
  path <- app_sys("extdata", filename)
  if (!file.exists(path)) {
    stop("extdata file not found: ", filename, call. = FALSE)
  }
  ext <- tolower(tools::file_ext(filename))
  if (ext == "rds") {
    readRDS(path)
  } else if (ext == "csv") {
    utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  } else if (ext == "json") {
    jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  } else {
    stop("Unsupported extdata format: ", ext, call. = FALSE)
  }
}

#' Ensure a data frame has required columns, filling missing ones with NA
#' @param df Data frame
#' @param required_cols Character vector of column names
#' @return Data frame with all required columns present
#' @noRd
ensure_columns <- function(df, required_cols) {
  missing <- setdiff(required_cols, names(df))
  for (col in missing) {
    df[[col]] <- NA
  }
  df
}
