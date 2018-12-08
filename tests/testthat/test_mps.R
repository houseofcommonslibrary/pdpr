### Test MPs functions
context("MPs functions")

# Imports ---------------------------------------------------------------------

source("validate.R")

# Mocks -----------------------------------------------------------------------

mock_fetch_mps_raw <- function() {
    read("mps_raw")
}

mock_fetch_commons_memberships_raw <- function() {
    read("mps_commons_memberships_raw")
}

mock_fetch_mps_party_memberships_raw <- function() {
    read("mps_party_memberships_raw")
}

mock_fetch_mps_government_roles_raw <- function() {
    read("mps_government_roles_raw")
}

mock_fetch_mps_opposition_roles_raw <- function() {
    read("mps_opposition_roles_raw")
}

mock_fetch_mps_committee_memberships_raw <- function() {
    read("mps_committee_memberships_raw")
}

# Tests -----------------------------------------------------------------------

test_that("fetch_mps processes results correctly.", {
    with_mock(
        fetch_mps_raw = mock_fetch_mps_raw,
        fetch_commons_memberships = mock_fetch_commons_memberships_raw, {

        cols <- c(
            "person_id",
            "mnis_id",
            "given_name",
            "family_name",
            "display_name",
            "full_title",
            "gender")

        obs <- fetch_mps()
        exp <- read("fetch_mps")
        compare_obs_exp(obs, exp, cols)

        obs <- fetch_mps(from_date = "2017-06-08", to_date = "2017-06-08")
        exp <- read("fetch_mps_from_to")
        compare_obs_exp(obs, exp, cols)

        obs <- fetch_mps(on_date = "2017-06-08")
        exp <- read("fetch_mps_from_to")
        compare_obs_exp(obs, exp, cols)
    })
})

test_that("fetch_commons_memberships processes results correctly.", {
    with_mock(
        fetch_commons_memberships_raw = mock_fetch_commons_memberships_raw, {

            cols <- c(
                "person_id",
                "mnis_id",
                "given_name",
                "family_name",
                "display_name",
                "constituency_id",
                "constituency_name",
                "seat_incumbency_id",
                "seat_incumbency_start_date")

            obs <- fetch_commons_memberships()
            exp <- read("fetch_commons_memberships")
            compare_obs_exp(obs, exp, cols)

            obs <- fetch_commons_memberships(from_date = "2017-06-08",
                                             to_date = "2017-06-08")
            exp <- read("fetch_commons_memberships_from_to")
            compare_obs_exp(obs, exp, cols)

            obs <- fetch_commons_memberships(on_date = "2017-06-08")
            exp <- read("fetch_commons_memberships_from_to")
            compare_obs_exp(obs, exp, cols)
        })
})

test_that("fetch_mps_party_memberships processes results correctly.", {
    with_mock(fetch_mps_party_memberships_raw =
                  mock_fetch_mps_party_memberships_raw, {

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

            obs <- fetch_mps_party_memberships()
            exp <- read("fetch_mps_party_memberships")
            compare_obs_exp(obs, exp, cols)

            obs <- fetch_mps_party_memberships(from_date = "2017-06-08",
                                               to_date = "2017-06-08")
            exp <- read("fetch_mps_party_memberships_from_to")
            compare_obs_exp(obs, exp, cols)

            obs <- fetch_mps_party_memberships(on_date = "2017-06-08")
            exp <- read("fetch_mps_party_memberships_from_to")
            compare_obs_exp(obs, exp, cols)

            obs <- fetch_mps_party_memberships(while_mp = FALSE)
            exp <- read("fetch_mps_party_memberships_while_mp")
            compare_obs_exp(obs, exp, cols)

            obs <- fetch_mps_party_memberships(collapse = TRUE)
            exp <- read("fetch_mps_party_memberships_collapse")
            compare_obs_exp(obs, exp, cols)
        })
})

test_that("fetch_mps_government_roles processes results correctly.", {
    with_mock(
        fetch_mps_government_roles_raw = mock_fetch_mps_government_roles_raw, {

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

            obs <- fetch_mps_government_roles()
            exp <- read("fetch_mps_government_roles")
            compare_obs_exp(obs, exp, cols)

            obs <- fetch_mps_government_roles(from_date = "2017-06-08",
                                              to_date = "2017-06-08")
            exp <- read("fetch_mps_government_roles_from_to")
            compare_obs_exp(obs, exp, cols)

            obs <- fetch_mps_government_roles(on_date = "2017-06-08")
            exp <- read("fetch_mps_government_roles_from_to")
            compare_obs_exp(obs, exp, cols)

            obs <- fetch_mps_government_roles(while_mp = FALSE)
            exp <- read("fetch_mps_government_roles_while_mp")
            compare_obs_exp(obs, exp, cols)
        })
})

test_that("fetch_mps_opposition_roles processes results correctly.", {
    with_mock(
        fetch_mps_opposition_roles_raw = mock_fetch_mps_opposition_roles_raw, {

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

            obs <- fetch_mps_opposition_roles()
            exp <- read("fetch_mps_opposition_roles")
            compare_obs_exp(obs, exp, cols)

            obs <- fetch_mps_opposition_roles(from_date = "2017-06-08",
                                              to_date = "2017-06-08")
            exp <- read("fetch_mps_opposition_roles_from_to")
            compare_obs_exp(obs, exp, cols)

            obs <- fetch_mps_opposition_roles(on_date = "2017-06-08")
            exp <- read("fetch_mps_opposition_roles_from_to")
            compare_obs_exp(obs, exp, cols)

            obs <- fetch_mps_opposition_roles(while_mp = FALSE)
            exp <- read("fetch_mps_opposition_roles_while_mp")
            compare_obs_exp(obs, exp, cols)
        })
})

test_that("fetch_mps_committee_memberships processes results correctly.", {
    with_mock(
        fetch_mps_committee_memberships_raw =
            mock_fetch_mps_committee_memberships_raw, {

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

            obs <- fetch_mps_committee_memberships()
            exp <- read("fetch_mps_committee_memberships")
            compare_obs_exp(obs, exp, cols)

            obs <- fetch_mps_committee_memberships(from_date = "2017-06-08",
                                                   to_date = "2017-06-08")
            exp <- read("fetch_mps_committee_memberships_from_to")
            compare_obs_exp(obs, exp, cols)

            obs <- fetch_mps_committee_memberships(on_date = "2017-06-08")
            exp <- read("fetch_mps_committee_memberships_from_to")
            compare_obs_exp(obs, exp, cols)

            obs <- fetch_mps_committee_memberships(while_mp = FALSE)
            exp <- read("fetch_mps_committee_memberships_while_mp")
            compare_obs_exp(obs, exp, cols)
        })
})
