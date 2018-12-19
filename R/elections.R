### Elections data functions

#' Return the dates of UK general elections since 1929 as a tibble
#'
#' \code{get_general_elections} returns the dates of UK general elections since
#' 1929 as a tibble with three columns:
#'
#'  name        -- The name of each general election as a string
#'  dissolution -- The date of dissolution as a date
#'  election    -- The date of the election as a date
#'
#' @return A tibble with data on general elections.
#' @export

get_general_elections <- function () {

    election_dates <- "
        name,        dissolution,       election
        1929,        1929-05-10,        1929-05-30
        1931,        1931-10-07,        1931-10-27
        1935,        1935-10-25,        1935-11-14
        1945,        1945-06-15,        1945-07-05
        1950,        1950-02-03,        1950-02-23
        1951,        1951-10-05,        1951-10-25
        1955,        1955-05-06,        1955-05-26
        1959,        1959-09-18,        1959-10-08
        1964,        1964-09-25,        1964-10-15
        1966,        1966-03-10,        1966-03-31
        1970,        1970-05-29,        1970-06-18
        1974 (Feb),  1974-02-08,        1974-02-28
        1974 (Oct),  1974-09-20,        1974-10-10
        1979,        1979-04-07,        1979-05-03
        1983,        1983-05-13,        1983-06-09
        1987,        1987-05-18,        1987-06-11
        1992,        1992-03-16,        1992-04-09
        1997,        1997-04-08,        1997-05-01
        2001,        2001-05-14,        2001-06-07
        2005,        2005-04-11,        2005-05-05
        2010,        2010-04-12,        2010-05-06
        2015,        2015-03-30,        2015-05-07
        2017,        2017-05-03,        2017-06-08"

    readr::read_csv(election_dates,
                    trim_ws = TRUE,
                    col_types = readr::cols(
                        name = readr::col_character(),
                        dissolution = readr::col_date(),
                        election = readr::col_date()))
}

#' Return the dates of UK general elections since 1929 as a list
#'
#' \code{get_general_elections_list} returns a list containing the dissolution
#' and election dates for each general election since 1929 as date objects.
#' Each item in the list is named with the election name and contains a list of
#' two values: one named "dissolution" containing the dissolution date and the
#' other named "election" containing the election date.
#'
#' @return A list containing the dissolution and election dates for each
#'   general election.
#' @export

get_general_elections_list <- function() {
    elections <- get_general_elections()
    election_list <- purrr::pmap(
        as.list(dplyr::select(elections, dissolution, election)),
        function(dissolution, election) {
            list(
                dissolution = cast_date(dissolution),
                election = cast_date(election)
            )
        }
    )
    names(election_list) <- elections$name
    election_list
}
