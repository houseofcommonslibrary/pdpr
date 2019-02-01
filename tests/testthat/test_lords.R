### Test Lords functions
context("Lords functions")

# Imports ---------------------------------------------------------------------

source("validate.R")

# Mocks -----------------------------------------------------------------------

mock_fetch_lords_raw <- function() {
    read("lords_raw")
}

mock_fetch_lords_memberships_raw <- function() {
    read("lords_memberships_raw")
}

mock_fetch_lords_party_memberships_raw <- function() {
    read("lords_party_memberships_raw")
}

mock_fetch_lords_government_roles_raw <- function() {
    read("lords_government_roles_raw")
}

mock_fetch_lords_opposition_roles_raw <- function() {
    read("lords_opposition_roles_raw")
}

mock_fetch_lords_committee_memberships_raw <- function() {
    read("lords_committee_memberships_raw")
}

# Tests -----------------------------------------------------------------------

test_that("fetch_lords processes results correctly.", {
    with_mock(
        "pdpr::fetch_lords_raw" = mock_fetch_lords_raw,
        "pdpr::fetch_lords_memberships" = mock_fetch_lords_memberships_raw, {

            cols <- c(
                "person_id",
                "mnis_id",
                "given_name",
                "family_name",
                "display_name",
                "full_title",
                "gender")

            obs <- fetch_lords()
            exp <- read("fetch_lords")
            compare_obs_exp(obs, exp, cols, "person_id")

            obs <- fetch_lords(from_date = "2017-06-08", to_date = "2017-06-08")
            exp <- read("fetch_lords_from_to")
            compare_obs_exp(obs, exp, cols, "person_id")
        })
})

test_that("fetch_lords_memberships processes results correctly.", {
    with_mock(
        "pdpr::fetch_lords_memberships_raw" =
            mock_fetch_lords_memberships_raw, {

            cols <- c(
                "person_id",
                "mnis_id",
                "given_name",
                "family_name",
                "display_name",
                "seat_type_id",
                "seat_type_name",
                "seat_incumbency_id",
                "seat_incumbency_start_date")

            obs <- fetch_lords_memberships()
            exp <- read("fetch_lords_memberships")
            compare_obs_exp(obs, exp, cols, "person_id")

            obs <- fetch_lords_memberships(from_date = "2017-06-08",
                                             to_date = "2017-06-08")
            exp <- read("fetch_lords_memberships_from_to")
            compare_obs_exp(obs, exp, cols, "person_id")
        })
})

test_that("fetch_lords_party_memberships processes results correctly.", {
    with_mock("pdpr::fetch_lords_party_memberships_raw" =
                  mock_fetch_lords_party_memberships_raw, {

                      cols <- c(
                          "person_id",
                          "mnis_id",
                          "given_name",
                          "family_name",
                          "display_name",
                          "party_id",
                          "party_mnis_id",
                          "party_name",
                          "party_membership_start_date")

                      obs <- fetch_lords_party_memberships()
                      exp <- read("fetch_lords_party_memberships")
                      compare_obs_exp(obs, exp, cols, "person_id")

                      obs <- fetch_lords_party_memberships(from_date = "2017-06-08",
                                                         to_date = "2017-06-08")
                      exp <- read("fetch_lords_party_memberships_from_to")
                      compare_obs_exp(obs, exp, cols, "person_id")

                      obs <- fetch_lords_party_memberships(while_lord = FALSE)
                      exp <- read("fetch_lords_party_memberships_while_lord")
                      compare_obs_exp(obs, exp, cols, "person_id")

                      obs <- fetch_lords_party_memberships(collapse = TRUE)
                      exp <- read("fetch_lords_party_memberships_collapse")
                      compare_obs_exp(obs, exp, cols, "person_id")
                  })
})

test_that("fetch_lords_government_roles processes results correctly.", {
    with_mock(
        "pdpr::fetch_lords_government_roles_raw" =
            mock_fetch_lords_government_roles_raw, {

            cols <- c(
                "person_id",
                "mnis_id",
                "given_name",
                "family_name",
                "display_name",
                "position_id",
                "position_name",
                "government_incumbency_id",
                "government_incumbency_start_date")

            obs <- fetch_lords_government_roles()
            exp <- read("fetch_lords_government_roles")
            compare_obs_exp(obs, exp, cols, "person_id")

            obs <- fetch_lords_government_roles(from_date = "2017-06-08",
                                              to_date = "2017-06-08")
            exp <- read("fetch_lords_government_roles_from_to")
            compare_obs_exp(obs, exp, cols, "person_id")

            obs <- fetch_lords_government_roles(while_lord = FALSE)
            exp <- read("fetch_lords_government_roles_while_lord")
            compare_obs_exp(obs, exp, cols, "person_id")
        })
})

test_that("fetch_lords_opposition_roles processes results correctly.", {
    with_mock(
        "pdpr::fetch_lords_opposition_roles_raw" =
            mock_fetch_lords_opposition_roles_raw, {

            cols <- c(
                "person_id",
                "mnis_id",
                "given_name",
                "family_name",
                "display_name",
                "position_id",
                "position_name",
                "opposition_incumbency_id",
                "opposition_incumbency_start_date")

            obs <- fetch_lords_opposition_roles()
            exp <- read("fetch_lords_opposition_roles")
            compare_obs_exp(obs, exp, cols, "person_id")

            obs <- fetch_lords_opposition_roles(from_date = "2017-06-08",
                                              to_date = "2017-06-08")
            exp <- read("fetch_lords_opposition_roles_from_to")
            compare_obs_exp(obs, exp, cols, "person_id")

            obs <- fetch_lords_opposition_roles(while_lord = FALSE)
            exp <- read("fetch_lords_opposition_roles_while_lord")
            compare_obs_exp(obs, exp, cols, "person_id")
        })
})

test_that("fetch_lords_committee_memberships processes results correctly.", {
    with_mock(
        "pdpr::fetch_lords_committee_memberships_raw" =
            mock_fetch_lords_committee_memberships_raw, {

            cols <- c(
                "person_id",
                "mnis_id",
                "given_name",
                "family_name",
                "display_name",
                "committee_id",
                "committee_name",
                "committee_membership_id",
                "committee_membership_start_date")

            obs <- fetch_lords_committee_memberships()
            exp <- read("fetch_lords_committee_memberships")
            compare_obs_exp(obs, exp, cols, "person_id")

            obs <- fetch_lords_committee_memberships(from_date = "2017-06-08",
                                                     to_date = "2017-06-08")
            exp <- read("fetch_lords_committee_memberships_from_to")
            compare_obs_exp(obs, exp, cols, "person_id")

            obs <- fetch_lords_committee_memberships(while_lord = FALSE)
            exp <- read("fetch_lords_committee_memberships_while_lord")
            compare_obs_exp(obs, exp, cols, "person_id")
        })
})
