#' Query opencorporate website
#'
#' @param term Term to query on the open corporate website
#' @param nb.page Number of pages to query
#' @param credential Credential, for the professionnal version
#' @param country List or vector of countries to query
#'
ropencorp <- function(term, nb.page = 2, credential = NULL, country = NULL) {
# term <- "edf" ; nb.page <- 2 ; credential <- NULL ; country <- NULL
# library(data.table) ; library(stringr); library(xml2); library(rjson) ; library(tidyjson) ; library(dplyr)


  if(is.null(country)){
    search.json <- paste0("https://api.opencorporates.com/v0.4/companies/search?q=", term)
  } else{
    search.json <- paste0("https://api.opencorporates.com/v0.4/companies/search?q=", term, "*jurisdiction_code=", country)
  }

  suppressWarnings( res.json <- read_json(path = search.json, format = "jsonl") )

  nb.pages <-   res.json %>%
    spread_values(nb.pages = jstring("results", "total_pages") ) %>%
    select(nb.pages) %>%
    as.numeric

  ### without an API key,I could only go up to 20.
  if(min(nb.pages, nb.page) > 20){nb.pages <- 20
  } else{nb.pages <- min(nb.pages, nb.page)}

  scrape.dt <- function(x) x %>%
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
                  , opencorporates.url = jstring("opencorporates_url") ) %>%
    data.table

  ##### Final function to query the full dataset:
  if(nb.pages == 1) {

    oc.dt <- scrape.dt(res.json)

  } else { # nb.pages <- 2
    oc.dt <- rbindlist(lapply(1:nb.pages, function(x) { # x <- 2

      if(is.null(country)){
        search.json <- paste0("https://api.opencorporates.com/v0.4/companies/search?q=", term, "&page=", x)
      } else{
        search.json <- paste0("https://api.opencorporates.com/v0.4/companies/search?q=", term, "*jurisdiction_code=", country, "&page=", x)
      }

      suppressWarnings( res.json <- read_json(path = search.json, format = "jsonl") )
      scrape.dt(res.json)

    }))

  }

  oc.dt

}

#' Fingerprint function to clean the character
#'
#' @param x a vector of strings.
#'
#' @note If the entry is a numeric vector, it will be converted to a string character
#'
fingerprint.func <- function(x)  paste(sort(str_split(gsub("[[:punct:]]|[[:cntrl:]]", "", iconv(str_trim(tolower(x)))), " ")[[1]]), collapse = " ")
