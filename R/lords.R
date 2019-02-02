### Functions for downloading and analysing data on Lords

# Raw Lords queries -----------------------------------------------------------

#' Fetch key details for all Lords
#'
#' @keywords internal

fetch_lords_raw <- function() {
    fetch_members_raw(house = PDP_ID_HOUSE_OF_LORDS)
}

#' Fetch Lords memberships for all Lords
#'
#' @keywords internal

fetch_lords_memberships_raw <- function() {

    house <- PDP_ID_HOUSE_OF_LORDS

    lords_membership_query <- stringr::str_glue("
        PREFIX : <https://id.parliament.uk/schema/>
        PREFIX d: <https://id.parliament.uk/>
        SELECT DISTINCT

            ?person_id
            ?mnis_id
            ?given_name
            ?family_name
            ?display_name
            ?seat_type_id
            ?seat_type_name
            ?seat_incumbency_id
            ?seat_incumbency_start_date
            ?seat_incumbency_end_date

        WHERE {{

            # House constraint for the House of Lords
            BIND(d:{house} AS ?house)

            ?person_id :memberMnisId ?mnis_id;
                :personGivenName ?given_name ;
                :personFamilyName ?family_name ;
                <http://example.com/F31CBD81AD8343898B49DC65743F0BDF> ?display_name ;
                :memberHasParliamentaryIncumbency ?seat_incumbency_id .
            ?seat_incumbency_id a :SeatIncumbency ;
                :seatIncumbencyHasHouseSeat ?seat ;
                :parliamentaryIncumbencyStartDate ?seat_incumbency_start_date .
            OPTIONAL {{ ?seat_incumbency_id :parliamentaryIncumbencyEndDate ?seat_incumbency_end_date . }}
            ?seat :houseSeatHasHouse ?house ;
                :houseSeatHasHouseSeatType ?seat_type_id .
            ?seat_type_id :houseSeatTypeName ?seat_type_name .
        }}
    ")

    sparql_select(lords_membership_query)
}

#' Fetch party memberships for all Lords
#'
#' @keywords internal

fetch_lords_party_memberships_raw <- function() {
    fetch_party_memberships_raw(house = PDP_ID_HOUSE_OF_LORDS)
}

#' Fetch government roles for all Lords
#'
#' @keywords internal

fetch_lords_government_roles_raw <- function() {
    fetch_government_roles_raw(house = PDP_ID_HOUSE_OF_LORDS)
}

#' Fetch opposition roles for all Lords
#'
#' @keywords internal

fetch_lords_opposition_roles_raw <- function() {
    fetch_opposition_roles_raw(house = PDP_ID_HOUSE_OF_LORDS)
}

#' Fetch committee memberships for all Lords
#'
#' @keywords internal

fetch_lords_committee_memberships_raw <- function() {
    fetch_committee_memberships_raw(house = PDP_ID_HOUSE_OF_LORDS)
}

# Main Lords API --------------------------------------------------------------

#' Fetch key details for all Lords
#'
#' \code{fetch_lords} fetches data from the data platform showing key details
#' about each Lord, with one row per Lord.
#'
#' The from_date and to_date arguments can be used to filter the Lords returned
#' based on the dates of their Lords memberships. The on_date argument is a
#' convenience that sets the from_date and to_date to the same given date. The
#' on_date has priority: if the on_date is set, the from_date and to_date are
#' ignored.
#'
#' The filtering is inclusive: a Lord is returned if any part of one of their
#' Lords memberships falls within the period specified with the from and to
#' dates.
#'
#' @param from_date A string or Date representing a date. If a string is used
#'   it should specify the date in ISO 8601 date format e.g. '2000-12-31'. The
#'   default value is NA, which means no records are excluded on the basis of
#'   the from_date.
#' @param to_date A string or Date representing a date. If a string is used
#'   it should specify the date in ISO 8601 date format e.g. 2000-12-31'. The
#'   default value is NA, which means no records are excluded on the basis
#'   of the to_date.
#' @param on_date A string or Date representing a date. If a string is used
#'   it should specify the date in ISO 8601 date format e.g. 2000-12-31'. The
#'   default value is NA, which means no records are excluded on the basis
#'   of the on_date.
#' @return A tibble of key details for each Lord, with one row per Lord.
#' @export

fetch_lords <- function(from_date = NA,
                        to_date = NA,
                        on_date = NA) {

    # Set from_date and to_date to on_date if set
    if (! is.na(on_date)) {
        from_date <- on_date
        to_date <- on_date
    }

    # Fetch key details
    lords <- fetch_lords_raw()

    # Filter on dates if requested
    if (! is.na(from_date) || ! is.na(to_date)) {
        lords_memberships <- fetch_lords_memberships()
        matching_memberships <- filter_dates(
            lords_memberships,
            start_col = "seat_incumbency_start_date",
            end_col = "seat_incumbency_end_date",
            from_date = from_date,
            to_date = to_date)
        lords <- lords %>%
            dplyr::filter(.data$person_id %in% matching_memberships$person_id)
    }

    # Tidy up and return
    lords %>% dplyr::arrange(.data$family_name)
}

#' Fetch Lords memberships for all Lords
#'
#' \code{fetch_lords_memberships} fetches data from the data platform showing
#' Lords memberships for each Lord.
#'
#' The from_date and to_date arguments can be used to filter the memberships
#' returned. The on_date argument is a convenience that sets the from_date and
#' to_date to the same given date. The on_date has priority: if the on_date is
#' set, the from_date and to_date are ignored.
#'
#' The filtering is inclusive: a membership is returned if any part of it falls
#' within the period specified with the from and to dates.
#'
#' Note that a membership with an NA end date is still open.
#'
#' @param from_date A string or Date representing a date. If a string is used
#'   it should specify the date in ISO 8601 date format e.g. '2000-12-31'. The
#'   default value is NA, which means no records are excluded on the basis of
#'   the from_date.
#' @param to_date A string or Date representing a date. If a string is used
#'   it should specify the date in ISO 8601 date format e.g. 2000-12-31'. The
#'   default value is NA, which means no records are excluded on the basis
#'   of the to_date.
#' @param on_date A string or Date representing a date. If a string is used
#'   it should specify the date in ISO 8601 date format e.g. 2000-12-31'. The
#'   default value is NA, which means no records are excluded on the basis
#'   of the on_date.
#' @return A tibble of Lords memberships for each Lord, with one row per
#'   Lords membership.
#' @export

fetch_lords_memberships <- function(from_date = NA,
                                    to_date = NA,
                                    on_date = NA) {

    # Set from_date and to_date to on_date if set
    if (! is.na(on_date)) {
        from_date <- on_date
        to_date <- on_date
    }

    # Fetch the Lords memberships
    lords_memberships <- fetch_lords_memberships_raw()

    # Filter on dates if requested
    if (! is.na(from_date) || ! is.na(to_date)) {
        lords_memberships <- filter_dates(
            lords_memberships,
            start_col = "seat_incumbency_start_date",
            end_col = "seat_incumbency_end_date",
            from_date = from_date,
            to_date = to_date)
    }

    # Tidy up and return
    lords_memberships %>%
        dplyr::arrange(.data$family_name)
}

#' Fetch party memberships for all Lords
#'
#' \code{fetch_lords_party_memberships} fetches data from the data platform
#' showing party memberships for each Lord. The memberships are processed and
#' merged so that there is only one row for each period of continuous
#' membership within the same party. A membership with an NA end date is still
#' open.
#'
#' The from_date and to_date arguments can be used to filter the memberships
#' returned. The on_date argument is a convenience that sets the from_date and
#' to_date to the same given date. The on_date has priority: if the on_date is
#' set, the from_date and to_date are ignored.
#'
#' The while_lord argument can be used to filter the memberships to include
#' only those that occurred during the period when each individual was a Lord.
#'
#' The filtering is inclusive: a membership is returned if any part of it falls
#' within the period specified with the from and to dates.
#'
#' The collapse argument controls whether memberships are combined so that
#' there is only one row for each period of continuous membership within the
#' same party. Combining the memberships in this way means that party
#' membership ids from the data platform are not included in the tibble
#' returned.
#'
#' Note that a membership with an NA end date is still open.
#'
#' @param from_date A string or Date representing a date. If a string is used
#'   it should specify the date in ISO 8601 date format e.g. '2000-12-31'. The
#'   default value is NA, which means no records are excluded on the basis of
#'   the from_date.
#' @param to_date A string or Date representing a date. If a string is used
#'   it should specify the date in ISO 8601 date format e.g. 2000-12-31'. The
#'   default value is NA, which means no records are excluded on the basis
#'   of the to_date.
#' @param on_date A string or Date representing a date. If a string is used
#'   it should specify the date in ISO 8601 date format e.g. 2000-12-31'. The
#'   default value is NA, which means no records are excluded on the basis
#'   of the on_date.
#' @param while_lord A boolean indicating whether to filter the party
#'   membership to include only those memberships that were held while each
#'   individual was serving as a Lord. The default value is TRUE.
#' @param collapse A boolean which determines whether to collapse consecutive
#'   memberships within the same party into a single period of continuous party
#'   membership. Setting this to TRUE means that party membership ids are not
#'   returned in the dataframe. The default value is FALSE.
#' @return A tibble of party memberships for each MP, with one row per party
#'   membership.
#' @export

fetch_lords_party_memberships <- function(from_date = NA,
                                          to_date = NA,
                                          on_date = NA,
                                          while_lord = TRUE,
                                          collapse = FALSE) {

    # Set from_date and to_date to on_date if set
    if (! is.na(on_date)) {
        from_date <- on_date
        to_date <- on_date
    }

    # Fetch the party memberships
    party_memberships <- fetch_lords_party_memberships_raw()

    # Filter on dates if requested
    if (! is.na(from_date) || ! is.na(to_date)) {
        party_memberships <- filter_dates(
            party_memberships,
            start_col = "party_membership_start_date",
            end_col = "party_membership_end_date",
            from_date = from_date,
            to_date = to_date)
    }

    # Filter on Lords memberships if requested
    if (while_lord) {
        lords_memberships <- fetch_lords_memberships()
        party_memberships <- filter_memberships(
            tm = party_memberships,
            fm = lords_memberships,
            tm_id_col = "party_membership_id",
            tm_start_col = "party_membership_start_date",
            tm_end_col = "party_membership_end_date",
            fm_start_col = "seat_incumbency_start_date",
            fm_end_col = "seat_incumbency_end_date",
            join_col = "person_id")
    }

    # Collapse consecutive memberships and return if requested
    if (collapse) {
        return(combine_party_memberships(party_memberships))
    }

    # Otherwise tidy up and return
    party_memberships %>%
        dplyr::arrange(
            .data$family_name,
            .data$party_membership_start_date)
}

#' Fetch government roles for all Lords
#'
#' \code{fetch_lords_government_roles} fetches data from the data platform
#' showing government roles for each Lord.
#'
#' The from_date and to_date arguments can be used to filter the roles
#' returned. The on_date argument is a convenience that sets the from_date and
#' to_date to the same given date. The on_date has priority: if the on_date is
#' set, the from_date and to_date are ignored.
#'
#' The while_lord argument can be used to filter the roles to include only
#' those that occurred during the period when each individual was a Lord.
#'
#' The filtering is inclusive: a role is returned if any part of it falls
#' within the period specified with the from and to dates.
#'
#' Note that a role with an NA end date is still open.
#'
#' @param from_date A string or Date representing a date. If a string is used
#'   it should specify the date in ISO 8601 date format e.g. '2000-12-31'. The
#'   default value is NA, which means no records are excluded on the basis of
#'   the from_date.
#' @param to_date A string or Date representing a date. If a string is used
#'   it should specify the date in ISO 8601 date format e.g. 2000-12-31'. The
#'   default value is NA, which means no records are excluded on the basis
#'   of the to_date.
#' @param on_date A string or Date representing a date. If a string is used
#'   it should specify the date in ISO 8601 date format e.g. 2000-12-31'. The
#'   default value is NA, which means no records are excluded on the basis
#'   of the on_date.
#' @param while_lord A boolean indicating whether to filter the government roles
#'   to include only those roles that were held while each individual was
#'   serving as an MP. The default value is TRUE.
#' @return A tibble of government roles for each Lord, with one row per role.
#' @export

fetch_lords_government_roles <- function(from_date = NA,
                                         to_date = NA,
                                         on_date = NA,
                                         while_lord = TRUE) {

    # Set from_date and to_date to on_date if set
    if (! is.na(on_date)) {
        from_date <- on_date
        to_date <- on_date
    }

    # Fetch the government roles
    government_roles <- fetch_lords_government_roles_raw()

    # Filter on dates if requested
    if (! is.na(from_date) || ! is.na(to_date)) {
        government_roles <- filter_dates(
            government_roles,
            start_col = "government_incumbency_start_date",
            end_col = "government_incumbency_end_date",
            from_date = from_date,
            to_date = to_date)
    }

    # Filter on Lords memberships if requested
    if (while_lord) {
        lords_memberships <- fetch_lords_memberships()
        government_roles <- filter_memberships(
            tm = government_roles,
            fm = lords_memberships,
            tm_id_col = "government_incumbency_id",
            tm_start_col = "government_incumbency_start_date",
            tm_end_col = "government_incumbency_end_date",
            fm_start_col = "seat_incumbency_start_date",
            fm_end_col = "seat_incumbency_end_date",
            join_col = "person_id")
    }

    # Tidy up and return
    government_roles %>% dplyr::arrange(
        .data$family_name,
        .data$government_incumbency_start_date)
}


#' Fetch opposition roles for all Lords
#'
#' \code{fetch_lords_oppositon_roles} fetches data from the data platform
#' showing opposition roles for each Lord.
#'
#' The from_date and to_date arguments can be used to filter the roles
#' returned. The on_date argument is a convenience that sets the from_date and
#' to_date to the same given date. The on_date has priority: if the on_date is
#' set, the from_date and to_date are ignored.
#'
#' The filtering is inclusive: a role is returned if any part of it falls
#' within the period specified with the from and to dates.
#'
#' The while_lord argument can be used to filter the roles to include only
#' those that occurred during the period when each individual was a Lord.
#'
#' Note that a role with an NA end date is still open.
#'
#' @param from_date A string or Date representing a date. If a string is used
#'   it should specify the date in ISO 8601 date format e.g. '2000-12-31'. The
#'   default value is NA, which means no records are excluded on the basis of
#'   the from_date.
#' @param to_date A string or Date representing a date. If a string is used
#'   it should specify the date in ISO 8601 date format e.g. 2000-12-31'. The
#'   default value is NA, which means no records are excluded on the basis
#'   of the to_date.
#' @param on_date A string or Date representing a date. If a string is used
#'   it should specify the date in ISO 8601 date format e.g. 2000-12-31'. The
#'   default value is NA, which means no records are excluded on the basis
#'   of the on_date.
#' @param while_lord A boolean indicating whether to filter the opposition
#'   roles to include only those roles that were held while each individual was
#'   serving as an MP. The default value is TRUE.
#' @return A tibble of opposition roles for each MP, with one row per role.
#' @export

fetch_lords_opposition_roles <- function(from_date = NA,
                                         to_date = NA,
                                         on_date = NA,
                                         while_lord = TRUE) {

    # Set from_date and to_date to on_date if set
    if (! is.na(on_date)) {
        from_date <- on_date
        to_date <- on_date
    }

    # Fetch the opposition roles
    opposition_roles <- fetch_lords_opposition_roles_raw()

    # Filter on dates if requested
    if (! is.na(from_date) || ! is.na(to_date)) {
        opposition_roles <- filter_dates(
            opposition_roles,
            start_col = "opposition_incumbency_start_date",
            end_col = "opposition_incumbency_end_date",
            from_date = from_date,
            to_date = to_date)
    }

    # Filter on Lords memberships if requested
    if (while_lord) {
        lords_memberships <- fetch_commons_memberships()
        opposition_roles <- filter_memberships(
            tm = opposition_roles,
            fm = lords_memberships,
            tm_id_col = "opposition_incumbency_id",
            tm_start_col = "opposition_incumbency_start_date",
            tm_end_col = "opposition_incumbency_end_date",
            fm_start_col = "seat_incumbency_start_date",
            fm_end_col = "seat_incumbency_end_date",
            join_col = "person_id")
    }

    # Tidy up and return
    opposition_roles %>% dplyr::arrange(
        .data$family_name,
        .data$opposition_incumbency_start_date)
}

#' Fetch committee memberships for all Lords
#'
#' \code{fetch_lords_committee_memberships} fetches data from the data platform
#' showing committee memberships for each Lord.
#'
#' The from_date and to_date arguments can be used to filter the memberships
#' returned. The on_date argument is a convenience that sets the from_date and
#' to_date to the same given date. The on_date has priority: if the on_date is
#' set, the from_date and to_date are ignored.
#'
#' The while_lord argument can be used to filter the memberships to include
#' only those that occurred during the period when each individual was a Lord.
#'
#' The filtering is inclusive: a membership is returned if any part of it falls
#' within the period specified with the from and to dates.
#'
#' Note that a membership with an NA end date is still open.
#'
#' @param from_date A string or Date representing a date. If a string is used
#'   it should specify the date in ISO 8601 date format e.g. '2000-12-31'. The
#'   default value is NA, which means no records are excluded on the basis of
#'   the from_date.
#' @param to_date A string or Date representing a date. If a string is used
#'   it should specify the date in ISO 8601 date format e.g. 2000-12-31'. The
#'   default value is NA, which means no records are excluded on the basis
#'   of the to_date.
#' @param on_date A string or Date representing a date. If a string is used
#'   it should specify the date in ISO 8601 date format e.g. 2000-12-31'. The
#'   default value is NA, which means no records are excluded on the basis
#'   of the on_date.
#' @param while_lord A boolean indicating whether to filter the committee
#'   memberships to include only those memberships that were held while each
#'   individual was serving as a Lord. The default value is TRUE.
#' @return A tibble of committee memberships for each Lord, with one row per
#'   membership.
#' @export

fetch_lords_committee_memberships <- function(from_date = NA,
                                              to_date = NA,
                                              on_date = NA,
                                              while_lord = TRUE) {

    # Set from_date and to_date to on_date if set
    if (! is.na(on_date)) {
        from_date <- on_date
        to_date <- on_date
    }

    # Fetch the committee roles
    committee_memberships <- fetch_lords_committee_memberships_raw()

    # Filter on dates if requested
    if (! is.na(from_date) || ! is.na(to_date)) {
        committee_memberships <- filter_dates(
            committee_memberships,
            start_col = "committee_membership_start_date",
            end_col = "committee_membership_end_date",
            from_date = from_date,
            to_date = to_date)
    }

    # Filter on Lord memberships if requested
    if (while_lord) {
        lords_memberships <- fetch_lords_memberships()
        committee_memberships <- filter_memberships(
            tm = committee_memberships,
            fm = lords_memberships,
            tm_id_col = "committee_membership_id",
            tm_start_col = "committee_membership_start_date",
            tm_end_col = "committee_membership_end_date",
            fm_start_col = "seat_incumbency_start_date",
            fm_end_col = "seat_incumbency_end_date",
            join_col = "person_id")
    }

    # Tidy up and return
    committee_memberships %>% dplyr::arrange(
        .data$family_name,
        .data$committee_membership_start_date)
}
