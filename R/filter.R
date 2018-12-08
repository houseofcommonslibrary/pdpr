### Filter functions

# Filter dates ----------------------------------------------------------------

#' Filter a tibble of data based on the given from and to dates.
#'
#' \code{filter_dates} takes a tibble which contains data on a time bound
#' activity and returns the subset of rows where that activity took place
#' within a given period. The tibble must contain two columns of Date objects,
#' which record the start and end dates of an activity. The from and to dates
#' provided are used to find all rows where some part of the period
#' of activity took place within the period of filtering. The filtering
#' process is inclusive: as long as at least one day of activity falls within
#' the filtering period, the row is returned.
#'
#' @param df A tibble containing data on a time bound activity.
#' @param start_col The name of the column that contains the start date for
#'   the activity.
#' @param end_col The name of the column that contains the end date for the
#'   activity.
#' @param from_date A string or Date representing a date. If a string is used
#'   it should specify the date in ISO 8601 date format e.g. '2000-12-31'. The
#'   default value is NA, which means no records are excluded on the basis of
#'   the from_date.
#' @param to_date A string or Date representing a date. If a string is used
#'   it should specify the date in ISO 8601 date format e.g. 2000-12-31'. The
#'   default value is NA, which means no records are excluded on the basis
#'   of the to_date.
#' @return  A tibble with the same structure as the input df containing
#'   the rows that meet the filtering criteria.
#' @keywords internal

filter_dates <- function(df,
                         start_col,
                         end_col,
                         from_date = NA,
                         to_date = NA) {

    # Check the start and end columns exist
    if (! start_col %in% colnames(df)) {
        stop(missing_column_error(start_col))
    }

    if (! end_col %in% colnames(df)) {
        stop(missing_column_error(end_col))
    }

    # Check the dataframe has rows
    if (nrow(df) == 0) return(df)

    # Check there are dates to filter
    if (is.na(from_date) && is.na(to_date)) return(df)

    # Handle from and to dates
    from_date <- handle_date(from_date)
    to_date <- handle_date(to_date)

    # Check from date is before to date
    if ((! is.na(from_date)) && (! is.na(to_date)) && (from_date > to_date)) {
        stop("to_date is before from_date")
    }
    # Set default values
    from_after_end <- FALSE
    to_before_start = FALSE

    # Get matching rows
    if (! is.na(from_date)) {
        from_after_end <- purrr::map_lgl(df[[end_col]], function(d) {
            ifelse(is.na(d), FALSE, from_date > d)
        })
    }

    if (! is.na(to_date)) {
        to_before_start <- purrr::map_lgl(df[[start_col]], function(d) {
            ifelse(is.na(d), FALSE, to_date < d)
        })
    }

    df[!(from_after_end | to_before_start), ]
}

#' Take a date which may be a string or a date and returns a date.
#'
#' \code{handle_date} takes a date which may be a Date or an ISO 8601 date
#' string, checks it is valid, and returns the date as a Date. NA values are
#' returned unmodified. This function raises an error if it is unable to
#' handle the date.
#'
#' @keywords internal

handle_date <- function(d) {
    if (is.na(d)) {
        return(d)
    } else if (class(d) == "Date") {
        return(d)
    } else if(class(d) == "character") {
        return(parse_date(d))
    } else {
        stop(date_format_error(d))
    }
}

# Filter memberships ----------------------------------------------------------

#' Filter a dataframe of memberships to include only the rows whose period
#' of membership intersects with those in another dataframe of memberships
#'
#' \code{filter_memberships} is a function to find all memberships in one
#' dataframe that intersect with those in another data frame for each person,
#' or other entity. This function lets you find things like all committee
#' memberships for Commons Members during the period they have served as an MP,
#' or all government roles held by Members of the House Lords while they have
#' served in the Lords.
#'
#' @param tm A tibble containing the target memberships. These are the
#'   memberships to be filtered.
#' @param fm A tibble containing the filter memberships. These are the
#'   memberships that are used to filter the target memberships.
#' @param tm_id_col The name of the column in the target memberships that
#'   contains the target membership id.
#' @param tm_start_col The name of the column in target memberships that
#'   contains the start date for the membership.
#' @param tm_end_col The name of the column in target memberships that contains
#'   the end date for the membership.
#' @param fm_start_col The name of the column in filter memberships that
#'   contains the start date for the membership.
#' @param fm_end_col The name of the column in filter memberships that contains
#'   the end date for the membership.
#' @param join_col The name of the column in both the target and filter
#'   memberships that contains the id of the entity that is common to both
#'   tables. Where the entity is a person this will be the person id.
#' @return  A tibble with the same structure as the input tm containing the
#'   rows that meet the filtering criteria.
#' @keywords internal

filter_memberships <- function(tm,
                               fm,
                               tm_id_col,
                               tm_start_col,
                               tm_end_col,
                               fm_start_col,
                               fm_end_col,
                               join_col) {

    # Check the target dataframe has rows
    if (nrow(tm) == 0) return(tm)

    # Check the columns exist in each dataframe
    if (! tm_id_col %in% colnames(tm)) {
        stop(missing_column_error(tm_id_col))
    }

    if (! tm_start_col %in% colnames(tm)) {
        stop(missing_column_error(tm_start_col))
    }

    if (! tm_end_col %in% colnames(tm)) {
        stop(missing_column_error(tm_end_col))
    }

    if (! fm_start_col %in% colnames(fm)) {
        stop(missing_column_error(fm_start_col))
    }

    if (! fm_end_col %in% colnames(fm)) {
        stop(missing_column_error(fm_end_col))
    }

    if (! join_col %in% colnames(fm)) {
        stop(missing_column_error(join_col))
    }

    # Create abstract copies of tm and fm
    tma <- tm %>% dplyr::select_(join_col, tm_id_col, tm_start_col, tm_end_col)
    colnames(tma) <- c("join_col", "tm_id_col", "tm_start_col", "tm_end_col")

    fma <- fm %>% dplyr::select_(join_col, fm_start_col, fm_end_col)
    colnames(fma) <- c("join_col", "fm_start_col", "fm_end_col")

    # Join the target memberships with the filter membership dates on join_col
    tm_fm <- dplyr::left_join(
        tma,
        fma,
        by = "join_col")

    # Function to test if a target membership and filter membership intersect
    in_fm_func <- function(row) {

        # Handle dates
        tm_start_date <- row["tm_start_col"]
        tm_end_date <- row["tm_end_col"]
        fm_start_date <- row["fm_start_col"]
        fm_end_date <- row["fm_end_col"]
        tm_start_after_fm_end <- FALSE
        tm_end_before_fm_start <- FALSE

        # Get the match status of the rows
        if (! is.na(tm_start_date)) {
            tm_start_after_fm_end <- ifelse(
                is.na(fm_end_date), FALSE, tm_start_date > fm_end_date)
        }

        if (! is.na(tm_end_date)) {
            tm_end_before_fm_start <- ifelse(
                is.na(fm_start_date), FALSE,  tm_end_date < fm_start_date)
        }

        # Return if the memberships instersect
        ! (tm_start_after_fm_end || tm_end_before_fm_start)
    }

    # Apply the function to each combination of target and filter memberships
    tm_fm["in_membership"] <- apply(tm_fm, 1, in_fm_func)

    match_status <- tm_fm %>%
        dplyr::group_by_("tm_id_col") %>%
        dplyr::summarise(in_membership = any(in_membership))

    # Restore the actual target membership id column name for joining
    colnames(match_status) <- c(tm_id_col, "in_membership")

    # Join the match status with the original target memberships data
    tm_fm_status <- dplyr::left_join(
        tm,
        match_status,
        by = tm_id_col)

    # Return the target memberships after filtering
    tm_fm_status %>%
        dplyr::filter(in_membership) %>%
        dplyr::select(-in_membership) %>%
        dplyr::ungroup()
}
