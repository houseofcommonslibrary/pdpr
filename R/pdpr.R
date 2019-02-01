#' pdpr: A package for downloading data from the Parliamentary Data Platform
#'
#' The pdpr package provides a suite of functions for downloading data from
#' the data platform for the UK Parliament.
#'
#' @docType package
#' @name pdpr
NULL

# Import pipe
#' @importFrom magrittr %>%
NULL

# Tell R CMD check about variables in pipelines
if(getRversion() >= "2.15.1") {
    utils::globalVariables(c(
        "person_id",
        "mnis_id",
        "given_name",
        "family_name",
        "display_name",
        "party_id",
        "party_mnis_id",
        "party_name",
        "per_par_mem_id",
        "party_membership_start_date",
        "party_membership_end_date",
        "seat_incumbency_start_date",
        "seat_incumbency_end_date",
        "government_incumbency_start_date",
        "government_incumbency_end_date",
        "opposition_incumbency_start_date",
        "opposition_incumbency_end_date",
        "seat_incumbency_end_date",
        "committee_membership_start_date",
        "committee_membership_end_date",
        "in_membership",
        "dissolution",
        "election"))
}
