### Core download functions

#' Send an http request with a query and return the response
#'
#' \code{request} sends a SPARQL query to the api endpoint and returns the
#' response object. It is a simple wrapper around httr::POST. It sets the
#' appropriate headers and sends the query as the request body. It does not
#' validate the query or handle the response in any way. The response format
#' is JSON.
#'
#' @param query A SPARQL query as a string.
#' @return The http response object from httr.
#' @keywords internal

request <- function(query) {

    url <- get_api_url()

    headers <- httr::add_headers(
        "content-type" = "application/sparql-query",
        "accept" = "application/sparql-results+json"
    )

    httr::POST(url, headers, body = query, encode = "form")
}

#' Send a select query and return the response as a tibble
#'
#' \code{sparql_select} sends a SPARQL query to the api endpoint and returns
#' the response as a tibble. The SPARQL should be a SELECT query as the
#' response is processed as tabular data. The function will convert datatypes
#' that it recognises. It currently recognises date types. All other data
#' returned in the tibble will be strings. If the query syntax is not valid or
#' the request fails for any other reason an error will be raised with the
#' response text.
#'
#' @param query A SPARQL SELECT query as a string.
#' @return A tibble containing the results of the query.
#' @export

sparql_select <- function(query) {

    # Send the query and get the response
    response <- request(query)
    response_text <- httr::content(response, as = "text", encoding = "utf-8")

    # If the server returned an error raise it with the response text
    if (response$status_code != 200) stop(request_error(response_text))

    # Process the response as tabular data and return it as a tibble
    json <- jsonlite::fromJSON(response_text, simplifyVector = FALSE)
    headers <- unlist(json$head$vars)
    records <- json$results$bindings

    # Create an empty table to store the data
    data_table <- purrr::map(headers, ~ rep(NA, length(records)))
    names(data_table) <- headers

    # For each record copy each item of data that exists into the table
    for (i in 1:length(records)) {
        record_headers <- names(records[[i]])
        # For each data item in the record copy it into the table
        for (j in 1:length(record_headers)) {
            record_header <- record_headers[[j]]
            record_value <- records[[i]][[record_header]][["value"]]
            data_table[[record_header]][i] <- record_value
        }
    }

    # For each column check whether it needs type conversion
    for (i in 1:length(headers)) {

        header <- headers[[i]]
        data_vector <- data_table[[header]]
        item_pos <- 0

        # Find the position of the first data item i.e. not NA
        for (j in 1:length(data_vector)) {
            if(!is.na(data_vector[j])) {
                item_pos <- j
                break
             }
        }

        # If all values are NA skip to the next header
        if (item_pos == 0) next

        # Inspect the first data item to see if column needs type conversion
        item <- records[[item_pos]][[header]]
        if ("datatype" %in% names(item)) {
            if (item[["datatype"]] == XML_DATE) {
                data_table[[header]] <- convert_pdp_date(data_vector)
            }
        } else {
            data_table[[header]] <- stringr::str_trim(data_vector)
        }
    }

    tibble::as_tibble(data_table)
}
