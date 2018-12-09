### Test combine functions
context("Combine functions")

# Test data -------------------------------------------------------------------

pm_csv <- "
    person_id,  membership_id,  party_id,   start_date, end_date
    p1,         m1,             pa1,        2001-01-01, 2001-12-31
    p1,         m2,             pa2,        2002-01-01, 2002-12-31
    p1,         m3,             pa1,        2003-01-01, 2003-12-31
    p1,         m4,             pa1,        2004-01-01, 2004-12-31
    p2,         m5,             pa1,        2002-01-01, 2002-12-31
    p2,         m6,             pa2,        2004-01-01, NA
    p2,         m7,             pa2,        2003-01-01, 2003-12-31
    p2,         m8,             pa1,        2001-01-01, 2001-12-31
"

pm <- readr::read_csv(pm_csv,
                      trim_ws = TRUE,
                      col_types = readr::cols(
                          person_id = readr::col_character(),
                          membership_id = readr::col_character(),
                          party_id = readr::col_character(),
                          start_date = readr::col_date(),
                          end_date = readr::col_date()))

pm$party_membership_start_date = pm$start_date
pm$party_membership_end_date = pm$end_date
pm$mnis_id = ""
pm$given_name = pm$person_id
pm$family_name = pm$person_id
pm$display_name = pm$person_id
pm$party_membership_id = ""
pm$party_mnis_id = ""
pm$party_name = ""
pm <- pm %>% dplyr::select(
    person_id,
    mnis_id,
    given_name,
    family_name,
    display_name,
    party_id,
    party_mnis_id,
    party_name,
    party_membership_id,
    party_membership_start_date,
    party_membership_end_date)

# Test combine_party_memberships ----------------------------------------------

test_that("combine_party_memberships raises an error for incorrect columns.", {

    pm_missing_column <- pm %>% dplyr::select(-person_id)

    expect_error(
        combine_party_memberships(pm_missing_column),
        "pm does not have the expected columns")

    pm_wrong_column_names <- pm %>% dplyr::select(-person_id)
    pm_wrong_column_names$pid <- pm$person_id

    expect_error(
        combine_party_memberships(pm_wrong_column_names),
        "pm does not have the expected columns")
})

test_that("combine filters the correct memberships.", {

    cpm <- combine_party_memberships(pm)

    expect_equal(nrow(cpm), 5)
    expect_equal(ncol(cpm), ncol(pm) - 1)
    expect_equal(colnames(cpm),
                 colnames(pm %>% dplyr::select(-party_membership_id)))
    expect_equal(cpm$person_id, c("p1", "p1", "p1", "p2", "p2"))
    expect_equal(cpm$party_membership_start_date, c(
        as.Date("2001-01-01"),
        as.Date("2002-01-01"),
        as.Date("2003-01-01"),
        as.Date("2001-01-01"),
        as.Date("2003-01-01")))
    expect_equal(cpm$party_membership_end_date, c(
        as.Date("2001-12-31"),
        as.Date("2002-12-31"),
        as.Date("2004-12-31"),
        as.Date("2002-12-31"),
        NA))
})
