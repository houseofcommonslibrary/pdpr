### Download data for unit testing MPs

# Imports ---------------------------------------------------------------------

source(file.path("tests", "testthat", "validate.R"))

# Mocks data ------------------------------------------------------------------

#' Fetch mocks data for unit tests of MPs
#'
#' @keywords internal

fetch_mps_mocks_data <- function() {

    # Download MPs
    m <- fetch_mps_raw()
    write(m, "mps_raw")
    Sys.sleep(API_PAUSE_TIME)

    # Download Commons memberships
    cm <- fetch_commons_memberships_raw()
    write(cm, "mps_commons_memberships_raw")
    Sys.sleep(API_PAUSE_TIME)

    # Download MP party memberships
    pm <- fetch_mps_party_memberships_raw()
    write(pm, "mps_party_memberships_raw")
    Sys.sleep(API_PAUSE_TIME)

    # Download MP government roles
    gor <- fetch_mps_government_roles_raw()
    write(gor, "mps_government_roles_raw")
    Sys.sleep(API_PAUSE_TIME)

    # Download MP opposition roles
    opr <- fetch_mps_opposition_roles_raw()
    write(opr, "mps_opposition_roles_raw")
    Sys.sleep(API_PAUSE_TIME)

    # Download MP committee memberships
    cmt <- fetch_mps_committee_memberships_raw()
    write(cmt, "mps_committee_memberships_raw")
    Sys.sleep(API_PAUSE_TIME)
}

# Validation data -------------------------------------------------------------

#' Fetch vaidation data for unit tests of MPs
#'
#' @keywords internal

fetch_mps_validation_data <- function() {

    # Fetch MPs
    m <- fetch_mps()
    write(m, "fetch_mps")
    Sys.sleep(API_PAUSE_TIME)

    # Fetch MPs with from and to dates
    m <- fetch_mps(from_date = "2017-06-08", to_date = "2017-06-08")
    write(m, "fetch_mps_from_to")
    Sys.sleep(API_PAUSE_TIME)

    # Fetch Commons memberships
    cm <- fetch_commons_memberships()
    write(cm, "fetch_commons_memberships")
    Sys.sleep(API_PAUSE_TIME)

    # Fetch Commons memberships with from and to dates
    cm <- fetch_commons_memberships(
        from_date = "2017-06-08", to_date = "2017-06-08")
    write(cm, "fetch_commons_memberships_from_to")
    Sys.sleep(API_PAUSE_TIME)

    # Fetch MPs party memberships
    pm <- fetch_mps_party_memberships()
    write(pm, "fetch_mps_party_memberships")
    Sys.sleep(API_PAUSE_TIME)

    # Fetch MPs party memberships with from and to dates
    pm <- fetch_mps_party_memberships(
        from_date = "2017-06-08", to_date = "2017-06-08")
    write(pm, "fetch_mps_party_memberships_from_to")
    Sys.sleep(API_PAUSE_TIME)

    # Fetch MPs party memberships with while_mp
    pm <- fetch_mps_party_memberships(while_mp = FALSE)
    write(pm, "fetch_mps_party_memberships_while_mp")
    Sys.sleep(API_PAUSE_TIME)

    # Fetch MPs party memberships with collapse
    pm <- fetch_mps_party_memberships(collapse = TRUE)
    write(pm, "fetch_mps_party_memberships_collapse")
    Sys.sleep(API_PAUSE_TIME)

    # Fetch MPs government roles
    gor <- fetch_mps_government_roles()
    write(gor, "fetch_mps_government_roles")
    Sys.sleep(API_PAUSE_TIME)

    # Fetch MPs government roles with from and to dates
    gor <- fetch_mps_government_roles(
        from_date = "2017-06-08", to_date = "2017-06-08")
    write(gor, "fetch_mps_government_roles_from_to")
    Sys.sleep(API_PAUSE_TIME)

    # Fetch MPs government roles with while_mp
    gor <- fetch_mps_government_roles(while_mp = FALSE)
    write(gor, "fetch_mps_government_roles_while_mp")
    Sys.sleep(API_PAUSE_TIME)

    # Fetch MPs opposition roles
    opr <- fetch_mps_opposition_roles()
    write(opr, "fetch_mps_opposition_roles")
    Sys.sleep(API_PAUSE_TIME)

    # Fetch MPs opposition roles with from and to dates
    opr <- fetch_mps_opposition_roles(
        from_date = "2017-06-08", to_date = "2017-06-08")
    write(opr, "fetch_mps_opposition_roles_from_to")
    Sys.sleep(API_PAUSE_TIME)

    # Fetch MPs opposition roles with while_mp
    opr <- fetch_mps_opposition_roles(while_mp = FALSE)
    write(opr, "fetch_mps_opposition_roles_while_mp")
    Sys.sleep(API_PAUSE_TIME)

    # Fetch MPs committee memberships
    cmt <- fetch_mps_committee_memberships()
    write(cmt, "fetch_mps_committee_memberships")
    Sys.sleep(API_PAUSE_TIME)

    # Fetch MPs committee memberships with from and to dates
    cmt <- fetch_mps_committee_memberships(
        from_date = "2017-06-08", to_date = "2017-06-08")
    write(cmt, "fetch_mps_committee_memberships_from_to")
    Sys.sleep(API_PAUSE_TIME)

    # Fetch MPs committee memberships with while_mp
    cmt <- fetch_mps_committee_memberships(while_mp = FALSE)
    write(cmt, "fetch_mps_committee_memberships_while_mp")
    Sys.sleep(API_PAUSE_TIME)
}

# Fetch all data --------------------------------------------------------------

#' Fetch mocks and validation data for unit tests of MPs
#'
#' @keywords internal

fetch_mps_test_data <- function() {
    fetch_mps_mocks_data()
    fetch_mps_validation_data()
}
