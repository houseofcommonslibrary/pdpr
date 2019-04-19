### User configurable package settings

# Settings environment --------------------------------------------------------

#' Package settings environment
#' @keywords internal
settings <- new.env(parent = emptyenv())

# Settings: api url -----------------------------------------------------------

#' Get the api url
#'
#' \code{get_api_key} gets the url that the package is currently configured to
#' use for the SPARQL endpoint to a data platform instance.
#'
#' @return The currently set api url as a string.
#' @keywords internal

get_api_url <- function() {
    if (! exists(SETTINGS_API_URL, envir = settings)) {
        set_api_url(SETTINGS_API_URL_DEFAULT)
    }
    get(SETTINGS_API_URL, envir = settings)
}

#' Set the api url
#'
#' \code{set_api_url} sets the url that the package uses for the api endpoint.
#' By default the package uses the main live endpoint for the data platform's
#' SPARQL api. If you wish to run a local version of the api you can use this
#' function to tell the package to use that endpoint instead.
#'
#' @param api_url The url of an available data platform SPARQL endpoint.
#' @return NULL
#' @export

set_api_url <- function(api_url) {
    assign(SETTINGS_API_URL, api_url, envir = settings)
}

#' Reset the api url to the default
#'
#' \code{reset_api_url} resets the url that the package uses for the api
#' endpoint to the live api url.
#'
#' @return NULL
#' @export

reset_api_url <- function() {
    set_api_url(SETTINGS_API_URL_DEFAULT)
}
