### Download data for unit testing Lords

# Imports -------------------------------------------------------------------

source(file.path("tests", "testthat", "validate.R"))

# Functions -------------------------------------------------------------------

#' Fetch mocks data for unit tests of Lords
#'
#' @keywords internal

fetch_lords_mocks_data <- function() {

    # Download Lords
    l <- fetch_lords_raw()
    write(l, "lords_raw")
    Sys.sleep(API_PAUSE_TIME)

    # Download Commons memberships
    cm <- fetch_lords_memberships_raw()
    write(cm, "lords_memberships_raw")
    Sys.sleep(API_PAUSE_TIME)

    # Download MP party memberships
    pm <- fetch_lords_party_memberships_raw()
    write(pm, "lords_party_memberships_raw")
    Sys.sleep(API_PAUSE_TIME)

    # Download MP government roles
    gor <- fetch_lords_government_roles_raw()
    write(gor, "lords_government_roles_raw")
    Sys.sleep(API_PAUSE_TIME)

    # Download MP opposition roles
    opr <- fetch_lords_opposition_roles_raw()
    write(opr, "lords_opposition_roles_raw")
    Sys.sleep(API_PAUSE_TIME)

    # Download MP committee memberships
    cmt <- fetch_lords_committee_memberships_raw()
    write(cmt, "lords_committee_memberships_raw")
    Sys.sleep(API_PAUSE_TIME)
}

#' Fetch vaidation data for unit tests of Lords
#'
#' @keywords internal

fetch_lords_validation_data <- function() {

    # Fetch MPs
    m <- fetch_lords()
    write(m, "fetch_lords")
    Sys.sleep(API_PAUSE_TIME)

    # Fetch Lords with from and to dates
    l <- fetch_lords(from_date = "2017-06-08", to_date = "2017-06-08")
    write(l, "fetch_lords_from_to")
    Sys.sleep(API_PAUSE_TIME)

    # Fetch Lords memberships
    lm <- fetch_lords_memberships()
    write(lm, "fetch_lords_memberships")
    Sys.sleep(API_PAUSE_TIME)

    # Fetch Lords memberships with from and to dates
    lm <- fetch_lords_memberships(
        from_date = "2017-06-08", to_date = "2017-06-08")
    write(lm, "fetch_lords_memberships_from_to")
    Sys.sleep(API_PAUSE_TIME)

    # Fetch Lords party memberships
    pm <- fetch_lords_party_memberships()
    write(pm, "fetch_lords_party_memberships")
    Sys.sleep(API_PAUSE_TIME)

    # Fetch Lords party memberships with from and to dates
    pm <- fetch_lords_party_memberships(
        from_date = "2017-06-08", to_date = "2017-06-08")
    write(pm, "fetch_lords_party_memberships_from_to")
    Sys.sleep(API_PAUSE_TIME)

    # Fetch Lords party memberships with while_lord
    pm <- fetch_lords_party_memberships(while_lord = FALSE)
    write(pm, "fetch_lords_party_memberships_while_lord")
    Sys.sleep(API_PAUSE_TIME)

    # Fetch Lords party memberships with collapse
    pm <- fetch_lords_party_memberships(collapse = TRUE)
    write(pm, "fetch_lords_party_memberships_collapse")
    Sys.sleep(API_PAUSE_TIME)

    # Fetch Lords government roles
    gor <- fetch_lords_government_roles()
    write(gor, "fetch_lords_government_roles")
    Sys.sleep(API_PAUSE_TIME)

    # Fetch Lords government roles with from and to dates
    gor <- fetch_lords_government_roles(
        from_date = "2017-06-08", to_date = "2017-06-08")
    write(gor, "fetch_lords_government_roles_from_to")
    Sys.sleep(API_PAUSE_TIME)

    # Fetch Lords government roles with while_lord
    gor <- fetch_lords_government_roles(while_lord = FALSE)
    write(gor, "fetch_lords_government_roles_while_lord")
    Sys.sleep(API_PAUSE_TIME)

    # Fetch Lords opposition roles
    opr <- fetch_lords_opposition_roles()
    write(opr, "fetch_lords_opposition_roles")
    Sys.sleep(API_PAUSE_TIME)

    # Fetch Lords opposition roles with from and to dates
    opr <- fetch_lords_opposition_roles(
        from_date = "2017-06-08", to_date = "2017-06-08")
    write(opr, "fetch_lords_opposition_roles_from_to")
    Sys.sleep(API_PAUSE_TIME)

    # Fetch Lords opposition roles with while_lord
    opr <- fetch_lords_opposition_roles(while_lord = FALSE)
    write(opr, "fetch_lords_opposition_roles_while_lord")
    Sys.sleep(API_PAUSE_TIME)

    # Fetch Lords committee memberships
    cmt <- fetch_lords_committee_memberships()
    write(cmt, "fetch_lords_committee_memberships")
    Sys.sleep(API_PAUSE_TIME)

    # Fetch Lords committee memberships with from and to dates
    cmt <- fetch_lords_committee_memberships(
        from_date = "2017-06-08", to_date = "2017-06-08")
    write(cmt, "fetch_lords_committee_memberships_from_to")
    Sys.sleep(API_PAUSE_TIME)

    # Fetch Lords committee memberships with while_lord
    cmt <- fetch_lords_committee_memberships(while_lord = FALSE)
    write(cmt, "fetch_lords_committee_memberships_while_lord")
    Sys.sleep(API_PAUSE_TIME)
}

# Fetch all data --------------------------------------------------------------

#' Fetch mocks and validation data for unit tests of Lords
#'
#' @keywords internal

fetch_lords_test_data <- function() {
    fetch_lords_mocks_data()
    fetch_lords_validation_data()
}
