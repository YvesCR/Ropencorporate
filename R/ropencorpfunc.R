#' Query opencorporate website
#'
#' @param term Term to query on the open corporate website
#' @param nb.page Number of pages to query
#' @param token Token for the owner of an opencorporate account
#' @param country List or vector of countries to query
#'
#' @note The returned object is a data.table if the data.table package is attached. Otherwise, it is a data.frame. Here, using the data.table package speed up the script.
#'
#' @export
#'
#' @examples
#'   ropencorp("gold holding", nb.page = 2)
#'
ropencorp <- function(term, nb.page = 2, token = NULL, country = NULL) {
# term <- "edf" ; nb.page <- 2 ; token <- NULL ; country <- NULL
# library(stringr); library(xml2); library(rjson) ; library(tidyjson) ; library(dplyr)

  if(is.null(country)){

    search.json <- paste0("https://api.opencorporates.com/v0.4/companies/search?q=", term)
  } else{
    search.json <- paste0("https://api.opencorporates.com/v0.4/companies/search?q=", term, "*jurisdiction_code=", country)
  }

  # if registered account, add the api key
  if(!is.null(token)) search.json <- paste0(search.json, "?api_token=", token)

  # if no internet connection, return an error
  suppressWarnings( res.json <- read_json(path = search.json, format = "jsonl") )

  nb.pages <-   res.json %>%
    spread_values(nb.pages = jstring("results", "total_pages") ) %>%
    dplyr::select(nb.pages) %>%
    as.numeric

  ### without an API key,I could only go up to 20.
  if(is.null(token)){
  if(min(nb.pages, nb.page) > 20){nb.pages <- 20
  } else{nb.pages <- min(nb.pages, nb.page)} }

  ##### Final function to query the full dataset:
  if(nb.pages == 1) {

    oc.dt <- Ropencorporate::scrape.dt(res.json)

  } else { # nb.pages <- 2
    oc.dt <- data.table::rbindlist(lapply(1:nb.pages, function(x) { # x <- 2

      if(is.null(country)){
        search.json <- paste0("https://api.opencorporates.com/v0.4/companies/search?q=", term, "&page=", x)
      } else{
        search.json <- paste0("https://api.opencorporates.com/v0.4/companies/search?q=", term, "*jurisdiction_code=", country, "&page=", x)
      }

      suppressWarnings( res.json <- read_json(path = search.json, format = "jsonl") )
      Ropencorporate::scrape.dt(res.json)

    }))

  }

  if ("package:data.table" %in% search()){
  oc.dt
  } else{
      oc.dt <- data.frame(oc.dt)
    }


}

#' Fingerprint function to clean the character
#'
#' @param x a string.
#'
#' @details The process that generates the key from a string value is the following:
#'
#' \itemize{
#' \item remove leading and trailing whitespace
#' \item change all characters to their lowercase representation
#' \item remove all punctuation and control characters
#' \item split the string into whitespace-separated tokens
#' \item sort the tokens and remove duplicates
#' \item join the tokens back together
#' \item normalize extended western characters to their ASCII representation
#' }
#'
#' @note If the entry is a numeric, it will be converted to a string character
#'
#' @export
#'
fingerprint.func <- function(x)  sapply(x, function(y) iconv(paste(sort(unique(str_split(gsub("[[:punct:]]|[[:cntrl:]]", "", tolower(str_trim(y))), " ")[[1]])), collapse = " ")))

#' Main function to scrape the API
#'
#' @param x a string to search through the API
#'
#' @note This is the core funciton of the package. All the rest is a wrapper for that one
#'
#' @export
#'
scrape.dt <- function(x) {
  data.table::data.table( x %>%
    enter_object("results") %>%
    enter_object("companies") %>%
    gather_array %>%
    enter_object("company") %>%
    spread_values(name = jstring("name")
                  , company.number = jstring("company_number")
                  , jurisdiction.code = jstring("jurisdiction_code")
                  , incorporation.date = jstring("incorporation_date")
                  , dissolution.date = jstring("dissolution_date")
                  , company.type = jstring("company_type")
                  , registry.url = jstring("registry_url")
                  , branch.status = jstring("branch_status")
                  , inactive = jstring("inactive")
                  , current.status = jstring("current_status")
                  , created.at = jstring("created_at")
                  , updated.at = jstring("updated_at")
                  , retrieved.at = jstring("retrieved_at")
                  , opencorporates.url = jstring("opencorporates_url") ) )
}
