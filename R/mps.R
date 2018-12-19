### Functions for downloading and analysing data on MPs

# Raw MPs queries -------------------------------------------------------------

#' Fetch key details for all MPs
#'
#' @keywords internal

fetch_mps_raw <- function() {
    fetch_members_raw(house = PDP_ID_HOUSE_OF_COMMONS)
}

#' Fetch Commons memberships for all MPs
#'
#' @keywords internal

fetch_commons_memberships_raw <- function() {

    house <- PDP_ID_HOUSE_OF_COMMONS

    commons_membership_query <- stringr::str_glue("
        PREFIX : <https://id.parliament.uk/schema/>
        PREFIX d: <https://id.parliament.uk/>
        SELECT DISTINCT

            ?person_id
            ?mnis_id
            ?given_name
            ?family_name
            ?display_name
            ?constituency_id
            ?constituency_name
            ?constituency_ons_id
            ?seat_incumbency_id
            ?seat_incumbency_start_date
            ?seat_incumbency_end_date

        WHERE {{

            # Entity id for the House of Commons
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
            ?seat :houseSeatHasConstituencyGroup ?constituency_id;
                :houseSeatHasHouse ?house .
            ?constituency_id :constituencyGroupName ?constituency_name ;
                :constituencyGroupStartDate ?constituencyStartDate .
            OPTIONAL {{ ?constituency_id :constituencyGroupOnsCode ?constituency_ons_id . }}
        }}
    ")

    sparql_select(commons_membership_query)
}

#' Fetch party memberships for all MPs
#'
#' @keywords internal

fetch_mps_party_memberships_raw <- function() {
    fetch_party_memberships_raw(house = PDP_ID_HOUSE_OF_COMMONS)
}

#' Fetch government roles for all MPs
#'
#' @keywords internal

fetch_mps_government_roles_raw <- function() {
    fetch_government_roles_raw(house = PDP_ID_HOUSE_OF_COMMONS)
}

#' Fetch opposition roles for all MPs
#'
#' @keywords internal

fetch_mps_opposition_roles_raw <- function() {
    fetch_opposition_roles_raw(house = PDP_ID_HOUSE_OF_COMMONS)
}

#' Fetch committee memberships for all MPs
#'
#' @keywords internal

fetch_mps_committee_memberships_raw <- function() {
    fetch_committee_memberships_raw(house = PDP_ID_HOUSE_OF_COMMONS)
}

# Main MPs API ----------------------------------------------------------------

#' Fetch key details for all MPs
#'
#' \code{fetch_mps} fetches data from the data platform showing key details
#' about each MP, with one row per MP.
#'
#' The from_date and to_date arguments can be used to filter the MPs returned
#' based on the dates of their Commons memberships. The on_date argument is a
#' convenience that sets the from_date and to_date to the same given date. The
#' on_date has priority: if the on_date is set, the from_date and to_date are
#' ignored.
#'
#' The filtering is inclusive: an MP is returned if any part of one of their
#' Commons memberships falls within the period specified with the from and to
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
#' @return A tibble of key details for each MP, with one row per MP.
#' @export

fetch_mps <- function(from_date = NA,
                      to_date = NA,
                      on_date = NA) {

    # Set from_date and to_date to on_date if set
    if (! is.na(on_date)) {
        from_date <- on_date
        to_date <- on_date
    }

    # Fetch key details
    mps <- fetch_mps_raw()

    # Filter on dates if requested
    if (! is.na(from_date) || ! is.na(to_date)) {
        commons_memberships <- fetch_commons_memberships()
        matching_memberships <- filter_dates(
            commons_memberships,
            start_col = 'seat_incumbency_start_date',
            end_col = 'seat_incumbency_end_date',
            from_date = from_date,
            to_date = to_date)
        mps <- mps %>%
            dplyr::filter(person_id %in% matching_memberships$person_id)
    }

    # Tidy up and return
    mps %>% dplyr::arrange(family_name)
}

#' Fetch Commons memberships for all MPs
#'
#' \code{fetch_commons_memberships} fetches data from the data platform
#' showing Commons memberships for each MP. The memberships are processed to
#' impose consistent rules on the start and end dates for memberships. A
#' membership with an NA end date is still open.
#'
#' The from_date and to_date arguments can be used to filter the memberships
#' returned. The on_date argument is a convenience that sets the from_date and
#' to_date to the same given date. The on_date has priority: if the on_date is
#' set, the from_date and to_date are ignored.
#'
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
#' @return A tibble of Commons memberships for each MP, with one row per
#'   Commons membership.
#' @export

fetch_commons_memberships <- function(from_date = NA,
                                      to_date = NA,
                                      on_date = NA) {

    # Set from_date and to_date to on_date if set
    if (! is.na(on_date)) {
        from_date <- on_date
        to_date <- on_date
    }

    # Fetch the Commons memberships
    commons_memberships <- fetch_commons_memberships_raw()

    # Filter on dates if requested
    if (! is.na(from_date) || ! is.na(to_date)) {
        commons_memberships <- filter_dates(
            commons_memberships,
            start_col = 'seat_incumbency_start_date',
            end_col = 'seat_incumbency_end_date',
            from_date = from_date,
            to_date = to_date)
    }

    # Define a function to adjust end dates if they are incorrect
    adjust_date <- function(date, elections) {
        after_dissolution <- date > elections$dissolution
        to_election <- date <= elections$election
        match <- after_dissolution & to_election
        ifelse (sum(match), elections[match,]$dissolution, date)
    }

    # Get the elections
    general_elections <- get_general_elections()

    # Calculate the adjusted end dates
    adj_ends <- purrr::map_dbl(
        commons_memberships$seat_incumbency_end_date,
        adjust_date,
        general_elections)

    # Cast back to dates and reassign
    commons_memberships$seat_incumbency_end_date <- cast_date(adj_ends)

    # Tidy up and return
    commons_memberships %>%
      dplyr::arrange(
          family_name,
          seat_incumbency_start_date) %>%
      dplyr::ungroup()
}

#' Fetch party memberships for all MPs
#'
#' \code{fetch_mps_party_memberships} fetches data from the data platform
#' showing party memberships for each MP. The memberships are processed and
#' merged so that there is only one row for each period of continuous
#' membership within the same party. A membership with an NA end date is still
#' open.
#'
#' The from_date and to_date arguments can be used to filter the memberships
#' returned. The on_date argument is a convenience that sets the from_date and
#' to_date to the same given date. The on_date has priority: if the on_date is
#' set, the from_date and to_date are ignored.
#'
#' The while_mp argument can be used to filter the memberships to include only
#' those that occurred during the period when each individual was an MP.
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
#' @param while_mp A boolean indicating whether to filter the party membership
#'   to include only those memberships that were held while each individual was
#'   serving as an MP. The default value is TRUE.
#' @param collapse A boolean which determines whether to collapse consecutive
#'   memberships within the same party into a single period of continuous party
#'   membership. Setting this to TRUE means that party membership ids are not
#'   returned in the dataframe. The default value is FALSE.
#' @return A tibble of party memberships for each MP, with one row per party
#'   membership.
#' @export

fetch_mps_party_memberships <- function(from_date = NA,
                                        to_date = NA,
                                        on_date = NA,
                                        while_mp = TRUE,
                                        collapse = FALSE) {

    # Set from_date and to_date to on_date if set
    if (! is.na(on_date)) {
        from_date <- on_date
        to_date <- on_date
    }

    # Fetch the party memberships
    party_memberships <- fetch_mps_party_memberships_raw()

    # Filter on dates if requested
    if (! is.na(from_date) || ! is.na(to_date)) {
        party_memberships <- filter_dates(
            party_memberships,
            start_col = 'party_membership_start_date',
            end_col = 'party_membership_end_date',
            from_date = from_date,
            to_date = to_date)
    }

    # Filter on Commons memberships if requested
    if (while_mp) {
        commons_memberships <- fetch_commons_memberships()
        party_memberships <- filter_memberships(
            tm = party_memberships,
            fm = commons_memberships,
            tm_id_col = 'party_membership_id',
            tm_start_col = 'party_membership_start_date',
            tm_end_col = 'party_membership_end_date',
            fm_start_col = 'seat_incumbency_start_date',
            fm_end_col = 'seat_incumbency_end_date',
            join_col = 'person_id')
    }

    # Collapse consecutive memberships and return if requested
    if (collapse) {
        return(combine_party_memberships(party_memberships))
    }

    # Otherwise tidy up and return
    party_memberships %>%
        dplyr::arrange(
            family_name,
            party_membership_start_date)
}

#' Fetch government roles for all MPs
#'
#' \code{fetch_mps_government_roles} fetches data from the data platform
#' showing government roles for each MP.
#'
#' The from_date and to_date arguments can be used to filter the roles
#' returned. The on_date argument is a convenience that sets the from_date and
#' to_date to the same given date. The on_date has priority: if the on_date is
#' set, the from_date and to_date are ignored.
#'
#' The while_mp argument can be used to filter the roles to include only those
#' that occurred during the period when each individual was an MP.
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
#' @param while_mp A boolean indicating whether to filter the government roles
#'   to include only those roles that were held while each individual was
#'   serving as an MP. The default value is TRUE.
#' @return A tibble of government roles for each MP, with one row per role.
#' @export

fetch_mps_government_roles <- function(from_date = NA,
                                       to_date = NA,
                                       on_date = NA,
                                       while_mp = TRUE) {

    # Set from_date and to_date to on_date if set
    if (! is.na(on_date)) {
        from_date <- on_date
        to_date <- on_date
    }

    # Fetch the government roles
    government_roles <- fetch_mps_government_roles_raw()

    # Filter on dates if requested
    if (! is.na(from_date) || ! is.na(to_date)) {
        government_roles <- filter_dates(
            government_roles,
            start_col = 'government_incumbency_start_date',
            end_col = 'government_incumbency_end_date',
            from_date = from_date,
            to_date = to_date)
    }

    # Filter on Commons memberships if requested
    if (while_mp) {
        commons_memberships <- fetch_commons_memberships()
        government_roles <- filter_memberships(
            tm = government_roles,
            fm = commons_memberships,
            tm_id_col = 'government_incumbency_id',
            tm_start_col = 'government_incumbency_start_date',
            tm_end_col = 'government_incumbency_end_date',
            fm_start_col = 'seat_incumbency_start_date',
            fm_end_col = 'seat_incumbency_end_date',
            join_col = 'person_id')
    }

    # Tidy up and return
    government_roles %>% dplyr::arrange(
        family_name,
        government_incumbency_start_date)
}

#' Fetch opposition roles for all MPs
#'
#' \code{fetch_mps_oppositon_roles} fetches data from the data platform
#' showing opposition roles for each MP.
#'
#' The from_date and to_date arguments can be used to filter the roles
#' returned. The on_date argument is a convenience that sets the from_date and
#' to_date to the same given date. The on_date has priority: if the on_date is
#' set, the from_date and to_date are ignored.
#'
#' The filtering is inclusive: a role is returned if any part of it falls
#' within the period specified with the from and to dates.
#'
#' The while_mp argument can be used to filter the roles to include only those
#' that occurred during the period when each individual was an MP.
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
#' @param while_mp A boolean indicating whether to filter the opposition roles
#'   to include only those roles that were held while each individual was
#'   serving as an MP. The default value is TRUE.
#' @return A tibble of opposition roles for each MP, with one row per role.
#' @export

fetch_mps_opposition_roles <- function(from_date = NA,
                                       to_date = NA,
                                       on_date = NA,
                                       while_mp = TRUE) {

    # Set from_date and to_date to on_date if set
    if (! is.na(on_date)) {
        from_date <- on_date
        to_date <- on_date
    }

    # Fetch the opposition roles
    opposition_roles <- fetch_mps_opposition_roles_raw()

    # Filter on dates if requested
    if (! is.na(from_date) || ! is.na(to_date)) {
        opposition_roles <- filter_dates(
            opposition_roles,
            start_col = 'opposition_incumbency_start_date',
            end_col = 'opposition_incumbency_end_date',
            from_date = from_date,
            to_date = to_date)
    }

    # Filter on Commons memberships if requested
    if (while_mp) {
        commons_memberships <- fetch_commons_memberships()
        opposition_roles <- filter_memberships(
            tm = opposition_roles,
            fm = commons_memberships,
            tm_id_col = 'opposition_incumbency_id',
            tm_start_col = 'opposition_incumbency_start_date',
            tm_end_col = 'opposition_incumbency_end_date',
            fm_start_col = 'seat_incumbency_start_date',
            fm_end_col = 'seat_incumbency_end_date',
            join_col = 'person_id')
    }

    # Tidy up and return
    opposition_roles %>% dplyr::arrange(
        family_name,
        opposition_incumbency_start_date)
}

#' Fetch committee memberships for all MPs
#'
#' \code{fetch_mps_committee_memberships} fetches data from the data platform
#' showing committee memberships for each MP.
#'
#' The from_date and to_date arguments can be used to filter the memberships
#' returned. The on_date argument is a convenience that sets the from_date and
#' to_date to the same given date. The on_date has priority: if the on_date is
#' set, the from_date and to_date are ignored.
#'
#' The while_mp argument can be used to filter the memberships to include only
#' those that occurred during the period when each individual was an MP.
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
#' @param while_mp A boolean indicating whether to filter the committee
#'   memberships to include only those memberships that were held while each
#'   individual was serving as an MP. The default value is TRUE.
#' @return A tibble of committee memberships for each MP, with one row per
#'   membership.
#' @export

fetch_mps_committee_memberships <- function(from_date = NA,
                                            to_date = NA,
                                            on_date = NA,
                                            while_mp = TRUE) {

    # Set from_date and to_date to on_date if set
    if (! is.na(on_date)) {
        from_date <- on_date
        to_date <- on_date
    }

    # Fetch the committee roles
    committee_memberships <- fetch_mps_committee_memberships_raw()

    # Filter on dates if requested
    if (! is.na(from_date) || ! is.na(to_date)) {
        committee_memberships <- filter_dates(
            committee_memberships,
            start_col = 'committee_membership_start_date',
            end_col = 'committee_membership_end_date',
            from_date = from_date,
            to_date = to_date)
    }

    # Filter on Commons memberships if requested
    if (while_mp) {
        commons_memberships <- fetch_commons_memberships()
        committee_memberships <- filter_memberships(
            tm = committee_memberships,
            fm = commons_memberships,
            tm_id_col = 'committee_membership_id',
            tm_start_col = 'committee_membership_start_date',
            tm_end_col = 'committee_membership_end_date',
            fm_start_col = 'seat_incumbency_start_date',
            fm_end_col = 'seat_incumbency_end_date',
            join_col = 'person_id')
    }

    # Tidy up and return
    committee_memberships %>% dplyr::arrange(
        family_name,
        committee_membership_start_date)
}
