### Functions for combining related records in a dataframe

#' Combine consecutive records in a tibble of party memberships
#'
#' \code{combine_party_memberships} takes a tibble of party memberships and
#' combines historically consecutive memberships of the same party into a
#' single continuous memberships with the start date of thre first membership
#' and the end date of the last. Combining the memberships in this way means
#' that party membership ids from the data platform are not included in the
#' dataframe returned.
#'
#' @param collapse A tibble containing party memberships as returned by one of
#'   the fetch party membership functions.
#' @return A tibble of party memberships, with one row per party membership.
#' The memberships are processed and combined so that there is only one party
#' membership for a period of continuous membership within the same party.
#' @keywords internal

combine_party_memberships <- function(pm) {

    # Check the party memberships dataframe has the expected structure
    required_columns <- c(
        "person_id",
        "mnis_id",
        "given_name",
        "family_name",
        "display_name",
        "party_id",
        "party_mnis_id",
        "party_name",
        "party_membership_id",
        "party_membership_start_date",
        "party_membership_end_date")

    if (length(colnames(pm)) != length(required_columns) ||
        ! all(colnames(pm) == required_columns)) {

        stop("pm does not have the expected columns")
    }

    # Function to identify consecutive memberships of the same party
    get_map_party_changes <- function() {

        previous_per_par_id <- ""
        group_id <- 0

        function(per_par_id) {
            if (per_par_id != previous_per_par_id) {
                previous_per_par_id <<- per_par_id
                group_id <<- group_id + 1
            }
            stringr::str_c(per_par_id, group_id, sep = "-")
        }
    }

    # Sort by person id and membership start date
    pm <- pm %>% dplyr::arrange(
        .data$person_id,
        .data$party_membership_start_date)

    # Create unique combination of person_id and party_id
    per_par_id <- stringr::str_c(
        pm$person_id,
        pm$party_id,
        sep = "-")

    # Build an id for consecutive memberships of the same party
    pm$per_par_mem_id <- purrr::map_chr(
        per_par_id,
        get_map_party_changes())

    # Group by person, party and consecutive membership, then take the
    # earliest start date and latest end date
    pm %>%
        dplyr::group_by(
            .data$person_id,
            .data$mnis_id,
            .data$given_name,
            .data$family_name,
            .data$display_name,
            .data$party_id,
            .data$party_mnis_id,
            .data$party_name,
            .data$per_par_mem_id) %>%
        dplyr::summarise(
            party_membership_start_date =
                min(.data$party_membership_start_date),
            party_membership_end_date =
                max(.data$party_membership_end_date)) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(
            .data$family_name,
            .data$party_membership_start_date) %>%
        dplyr::select(-.data$per_par_mem_id)
}
