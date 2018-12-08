### Test elections data functions
context("Elections data functions")

# Tests -----------------------------------------------------------------------

test_that("get_general_elections returns the expected elections data.", {

    ge <- get_general_elections()

    expect_equal(names(ge), c("name", "dissolution", "election"))
    expect_is(ge$name, "character")
    expect_is(ge$dissolution, "Date")
    expect_is(ge$election, "Date")

    # Test that dissolutions always precede elections
    expect_true(all(ge$dissolution < ge$election))

    # Test that elections always precede the following dissolution
    expect_true(all(
        ge$election[-length(ge$election)] < ge$dissolution[-1]
    ))

    # Test that election names are unique
    expect_true(length(ge$name) == length(unique(ge$name)))
})

test_that("get_general_elections_list returns the expected elections data.", {

    data <- get_general_elections_list()

    for (d in data) {
        expect_is(d, "list")
        expect_equal(length(names(d)), 2)
        expect_true("election" %in% names(d))
        expect_true("dissolution" %in% names(d))
        expect_is(d$dissolution, "Date")
        expect_is(d$election, "Date")
        expect_true(d$dissolution < d$election)
    }
})
