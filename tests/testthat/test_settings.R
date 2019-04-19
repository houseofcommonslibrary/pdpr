### Test core download functions
context("Settings functions")

# Test get_api_url ------------------------------------------------------------

test_that("get_api_url returns default url when a url has not been set.", {
    expect_equal(get_api_url(), SETTINGS_API_URL_DEFAULT)
})

# Test set_api_url ------------------------------------------------------------

test_that("set_api_url sets the api url returned by get_api_url.", {
    api_url <- "http://localhost:8000/sparql"
    set_api_url(api_url)
    expect_equal(get_api_url(), api_url)
    set_api_url(SETTINGS_API_URL_DEFAULT)
    expect_equal(get_api_url(), SETTINGS_API_URL_DEFAULT)
})

# Test reset_api_url ----------------------------------------------------------

test_that("reset_api_url resets the api url returned by get_api_url.", {
    api_url <- "http://localhost:8001/sparql"
    set_api_url(api_url)
    expect_equal(get_api_url(), api_url)
    reset_api_url()
    expect_equal(get_api_url(), SETTINGS_API_URL_DEFAULT)
})
