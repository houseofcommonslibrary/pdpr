### Package errors

#' Report an error with an http request
#'
#' @param response The text of the server reponse.
#' @keywords internal

request_error <- function(response) {
    stringr::str_glue(
        "The server responded with the following message: {response}")
}

#' Report an error parsing a date string
#'
#' @param date_str The date string that could not be parsed.
#' @keywords internal

date_format_error <- function(date_str) {
    stringr::str_glue("{date_str} is not a valid Date or date string")
}

#' Report an error handling dataframes with missing columms
#'
#' @param colname The name of the column that could not be found.
#' @keywords internal

missing_column_error <- function(colname) {
    stringr::str_glue("Could not find a column called {colname}")
}
