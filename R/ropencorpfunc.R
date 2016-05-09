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
ropencorp <- function(term, nb.page = 20, token = NULL, country = NULL) {
# term <- "edf" ; nb.page <- 2 ; token <- NULL ; country <- NULL
# library(stringr); library(jsonlite)

  if(is.null(country)){

    search.json <- paste0("https://api.opencorporates.com/v0.4/companies/search?q=", term)
  } else{
    search.json <- paste0("https://api.opencorporates.com/v0.4/companies/search?q=", term, "*jurisdiction_code=", country)
  }

  # if registered account, add the api key
  if(!is.null(token)) search.json <- paste0(search.json, "?api_token=", token)

  # if no internet connection, return an error
  res.json <- jsonlite::fromJSON(search.json)

  nb.pages <- res.json$results$total_pages

  ### without an API key, it is only possible to query up to 20.
  if(is.null(token)){
  if(min(nb.pages, nb.page) > 20){nb.pages <- 20
  } else{nb.pages <- min(nb.pages, nb.page)} }

  oc.dt <- res.json$results$companies$company[, c("name", "company_number", "jurisdiction_code", "incorporation_date"
            , "dissolution_date", "company_type", "registry_url", "branch_status"
            , "inactive", "current_status", "created_at", "updated_at"
            , "retrieved_at", "opencorporates_url"
            , "registered_address_in_full", "restricted_for_marketing")]

  list.prev <- res.json$results$companies$company$previous_names

  prev.dt <- data.table::rbindlist(lapply(1:nrow(oc.dt), function(x) if(nrow(list.prev[[x]]) == 0){
          oc.dt[x, c("company_number", "jurisdiction_code")]
   } else { cbind(list.prev[[x]]
                , oc.dt[x, c("company_number", "jurisdiction_code")])} )
                , use.names = T, fill = T)

  list1 <- list(oc.dt)
  list2 <- list(prev.dt)

   ##### Final function to query the full dataset:
   if(nb.pages > 1){ # nb.pages <- 2

     for(x in 2:nb.pages) { # x <- 2

       if(is.null(country)){
         search.json <- paste0("https://api.opencorporates.com/v0.4/companies/search?q=", term, "&page=", x)
       } else{
         search.json <- paste0("https://api.opencorporates.com/v0.4/companies/search?q=", term, "*jurisdiction_code=", country, "&page=", x)
       }

     res.json <- jsonlite::fromJSON(search.json)

     oc.dt <- res.json$results$companies$company[, c("name", "company_number", "jurisdiction_code", "incorporation_date"
           , "dissolution_date", "company_type", "registry_url", "branch_status"
           , "inactive", "current_status", "created_at", "updated_at"
           , "retrieved_at", "opencorporates_url"
           , "registered_address_in_full", "restricted_for_marketing")]

     list.prev <- res.json$results$companies$company$previous_names

     prev.dt <- data.table::rbindlist(lapply(1:nrow(oc.dt), function(x) if(nrow(list.prev[[x]]) == 0){
         oc.dt[x, c("company_number", "jurisdiction_code")]
       } else { cbind(list.prev[[x]]
                    , oc.dt[x, c("company_number", "jurisdiction_code")])} )
       , use.names = T, fill = T)

      list1 <- c(list1, list(oc.dt))
      list2 <- c(list2, list(prev.dt))

    }

     oc.dt <- rbindlist(list1, use.names = T, fill = T)
     prev.dt <- rbindlist(list2, use.names = T, fill = T)

  }

  if ("package:data.table" %in% search()){
  list(oc.dt, prev.dt)
  } else{
    list(data.frame(oc.dt), data.frame(prev.dt))
    }

}
#
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
