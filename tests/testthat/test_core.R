### Test core download functions
context("Core download functions")

# Setup -----------------------------------------------------------------------

# Check api is available
api_available <- check_api()

# Queries ---------------------------------------------------------------------

query_basic <- "
    PREFIX : <https://id.parliament.uk/schema/>
    SELECT *
    WHERE {
        ?p ?s ?o .
    }
    LIMIT 1
"

query_person <- "
    PREFIX : <https://id.parliament.uk/schema/>
    PREFIX d: <https://id.parliament.uk/>
    SELECT DISTINCT

        ?person
        ?given_name
        ?family_name
        ?gender
        ?dob

    WHERE {

        # Entity id for Shirley Williams
        BIND(d:URDlhhkg AS ?person)

        ?person :personGivenName ?given_name ;
            :personFamilyName ?family_name ;
            :personHasGenderIdentity/:genderIdentityHasGender/:genderName ?gender .
        OPTIONAL { ?person :personDateOfBirth ?dob . }
    }
"

query_broken <- "
    PREFIX : <https://id.parliament.uk/schema/>
    # PREFIX d: <https://id.parliament.uk/> Commented out to break query
    SELECT DISTINCT

        ?person
        ?given_name
        ?family_name
        ?gender
        ?dob

    WHERE {

        # Entity id for Shirley Williams
        BIND(d:URDlhhkg AS ?person)

        ?person :personGivenName ?given_name ;
            :personFamilyName ?family_name ;
            :personHasGenderIdentity/:genderIdentityHasGender/:genderName ?gender .
        OPTIONAL { ?person :personDateOfBirth ?dob . }
    }
"

query_broken_error <- stringr::str_c(
    "MALFORMED QUERY: org.eclipse.rdf4j.query.parser.sparql.ast.",
    "VisitorException: QName 'd:URDlhhkg' uses an undefined prefix")

# Tests -----------------------------------------------------------------------

test_that("request sends and receives the most basic SPARQL query", {

    if (!api_available) skip("skipped as api could not be reached")

    response <- request(query_basic)
    text <- httr::content(response, "text")
    json <- jsonlite::fromJSON(text, simplifyVector = FALSE)
    headers <- unlist(json$head$vars)
    records <- json$results$bindings

    expect_equal(response$status_code, 200)
    expect_equal(headers, c("p", "s", "o"))
    expect_equal(length(records), 1)
    expect_equal(records[[1]]$p$value,
        "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
    expect_equal(records[[1]]$s$value,
        "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
    expect_equal(records[[1]]$o$value,
        "http://www.w3.org/1999/02/22-rdf-syntax-ns#Property")

    Sys.sleep(API_PAUSE_TIME)
})

test_that("request sends and receives a Parliamentary query", {

    if (!api_available) skip("skipped as api could not be reached")

    response <- request(query_person)
    text <- httr::content(response, "text")
    json <- jsonlite::fromJSON(text, simplifyVector = FALSE)
    headers <- unlist(json$head$vars)
    records <- json$results$bindings

    expect_equal(response$status_code, 200)
    expect_equal(headers,
        c("person", "given_name", "family_name", "gender", "dob"))
    expect_equal(length(records), 1)
    expect_equal(records[[1]]$person$value,
        "https://id.parliament.uk/URDlhhkg")
    expect_equal(records[[1]]$given_name$value, "Shirley")
    expect_equal(records[[1]]$family_name$value, "Williams")
    expect_equal(records[[1]]$gender$value, "Female")
    expect_equal(records[[1]]$dob$value, "1930-07-27+01:00")
    expect_equal(records[[1]]$dob$datatype, XML_DATE)

    Sys.sleep(API_PAUSE_TIME)
})

test_that("select returns data for the most basic SPARQL query", {

    if (!api_available) skip("skipped as api could not be reached")

    data <- sparql_select(query_basic)

    expect_equal(names(data), c("p", "s", "o"))
    expect_is(data$p, "character")
    expect_is(data$s, "character")
    expect_is(data$o, "character")
    expect_equal(nrow(data), 1)
    expect_equal(data$p,
        "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
    expect_equal(data$s,
       "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
    expect_equal(data$o,
       "http://www.w3.org/1999/02/22-rdf-syntax-ns#Property")

    Sys.sleep(API_PAUSE_TIME)
})

test_that("select returns data for a Parliamentary query", {

    if (!api_available) skip("skipped as api could not be reached")

    data <- sparql_select(query_person)

    expect_equal(names(data),
        c("person", "given_name", "family_name", "gender", "dob"))
    expect_is(data$person, "character")
    expect_is(data$given_name, "character")
    expect_is(data$family_name, "character")
    expect_is(data$gender, "character")
    expect_is(data$dob, "Date")
    expect_equal(nrow(data), 1)
    expect_equal(data$person,
        "https://id.parliament.uk/URDlhhkg")
    expect_equal(data$given_name, "Shirley")
    expect_equal(data$family_name, "Williams")
    expect_equal(data$gender, "Female")
    expect_equal(data$dob, as.Date("1930-07-27"))

    Sys.sleep(API_PAUSE_TIME)
})

test_that("select raises an error for a broken query", {

    if (!api_available) skip("skipped as api could not be reached")
    expect_error(sparql_select(query_broken), query_broken_error)

    Sys.sleep(API_PAUSE_TIME)
})
