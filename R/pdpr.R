#' pdpr: A package for downloading data from the Parliamentary Data Platform
#'
#' The pdpr package provides a suite of functions for downloading data from
#' the data platform for the UK Parliament.
#'
#' @docType package
#' @name pdpr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
NULL

# Tell R CMD check about the dot operator in pipelines
if(getRversion() >= "2.15.1") {
    utils::globalVariables(c("."))
}
