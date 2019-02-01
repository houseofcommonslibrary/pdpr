### Package utility functions

# API Functions ---------------------------------------------------------------

#' Check if R can reach the api and return a boolean
#'
#' @keywords internal

check_api <- function() {

    api_url <- stringr::str_c(
        "https://api.parliament.uk/sparql",
        "?query=SELECT+*+WHERE+%7B+%3Fs+%",
        "3Fp+%3Fo+.+%7D+LIMIT+1%0D%0A")

    tryCatch({
        response <- httr::GET(api_url)
        response$status_code == 200
    }, error = function(e) {
        FALSE
    })
}

# Date handling functions -----------------------------------------------------

#' Convert a datetype variable returned from the data platorm to an R Date
#'
#' @keywords internal

convert_pdp_date <- function(datetime_str) {
    datetime <- as.POSIXct(strptime(datetime_str, format = "%Y-%m-%d+%H:%M"))
    as.Date(datetime, origin = "1970-01-01", tz = "Europe/London")
}

#' Parse an ISO 8601 date from a string
#'
#' @keywords internal

parse_date <- function(date_str) {

    valid_pattern <- "^\\d{4}\\-(0[1-9]|1[012])\\-(0[1-9]|[12][0-9]|3[01])$"
    if (! grepl(valid_pattern, date_str)) stop(date_format_error(date_str))

    tryCatch(
        as.Date(date_str, origin = "1970-01-01"),
        error = function(e) stop(date_format_error(date_str)))
}

#' Cast a numeric value to a Date
#'
#' @keywords internal

cast_date <- function(date_num) {
    tryCatch(
        as.Date(date_num, origin = "1970-01-01"),
        error = function(e) stop("Could not cast numeric to date"))
}

# Data presentation functions -------------------------------------------------

#' Take a tibble and remove all columns that end in the suffix "_id"
#'
#' The intended purpose of this function is to display a tibble on the console
#' showing only the readable columns i.e. not the identifiers.
#'
#' @param df A tibble.
#' @return  A tibble with the same structure as the input df with any columns
#' ending in the suffix "_id" removed.
#' @export

readable <- function(df) {
    readable_cols <- Filter(function(c) ! endsWith(c, "_id"), colnames(df))
    df[readable_cols]
}
