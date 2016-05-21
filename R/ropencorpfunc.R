#'
#' Query companies on the opencorporate API
#'
#' @param term Term to query on the open corporate API
#' @param nb.page Number of pages to query
#' @param token Token for the owner of an opencorporate account
#' @param country List or vector of countries to query
#'
#' @note The returned object is a list of two data.table if the data.table package is attached. Otherwise, it is a list of two data.frame.
#'  Here, using the data.table package speed up the script.
#'
#' @export
#'
#' @examples
#'   get.companies('gold holding', nb.page = 2)
#'
get.companies <- function(term, nb.page = 20, token = NULL, country = NULL) {
# term <- "edf" ; nb.page <- 20 ; token <- NULL ; country <- NULL

  name.var <- c("name", "company_number", "jurisdiction_code", "incorporation_date"
      , "dissolution_date", "company_type", "registry_url", "branch_status"
      , "inactive", "current_status", "created_at", "updated_at"
      , "retrieved_at", "opencorporates_url"
      , "registered_address_in_full", "restricted_for_marketing")

  call.func.api <- "https://api.opencorporates.com/v0.4/companies/search?q="

  var.prev <- c("company_number", "jurisdiction_code")

  # replace space by +
  term <- gsub(pattern = " ", replacement = "+", stringr::str_trim(term))

  if(is.null(country)){
    search.json <- paste0(call.func.api, term)
  } else{
    search.json <- paste0(call.func.api, term, "*jurisdiction_code=", country)
  }

  # if registered account, add the api key
  if(!is.null(token)) search.json <- paste0(search.json, "?api_token=", token)

  # if no internet connection, return an error
  try(res.json <- jsonlite::fromJSON(search.json), silent = T)

  if(exists("res.json")){

    nb.pages <- res.json$results$total_pages

    if(nb.pages == 0){
      print("No result. Please try another query.")
    } else{

      ### without an API key, it is only possible to query up to 20.
      if(is.null(token)){
        if(min(nb.pages, nb.page) > 20){nb.pages <- 20
      } else{nb.pages <- min(nb.pages, nb.page)} }

      oc.dt <- res.json$results$companies$company[, name.var]

      list.prev <- res.json$results$companies$company$previous_names

      # x <- 1
      prev.dt <- data.table::rbindlist(lapply(1:nrow(oc.dt)
            , function(x) if(is.null(nrow(list.prev[[x]]))){
                    oc.dt[x, var.prev]
                } else if (nrow(list.prev[[x]]) == 0 ){
                    oc.dt[x, var.prev]
                } else { cbind(list.prev[[x]]
            , oc.dt[x, var.prev]
            , stringsAsFactors = F)}  )
            , use.names = T, fill = T)

      data.table::setnames(oc.dt, names(oc.dt), gsub(pattern = "[ ]|[_]", ".",
               names(oc.dt)))
      data.table::setnames(prev.dt, names(prev.dt), gsub(pattern = "[ ]|[_]", ".",
                   names(prev.dt)))

      list1 <- list(oc.dt)
      list2 <- list(prev.dt)

       ##### Final function to query the full dataset:
       if(nb.pages > 1){ # nb.pages <- 2

         for(x in 2:nb.pages) { # x <- 2 ; x <- x + 1

           if(is.null(country)){
             search.json <- paste0(call.func.api, term, "&page=", x)
           } else{
             search.json <- paste0(call.func.api, term, "*jurisdiction_code=", country, "&page=", x)
           }

           # if registered account, add the api key
           if(!is.null(token)) search.json <- paste0(search.json, "?api_token=", token)

         res.json <- jsonlite::fromJSON(search.json)

         oc.dt <- res.json$results$companies$company[ , name.var]

         list.prev <- res.json$results$companies$company$previous_names

         prev.dt <- data.table::rbindlist(lapply(1:nrow(oc.dt)
                 , function(x) if(is.null(nrow(list.prev[[x]]))){
                   oc.dt[x, var.prev]
                 } else if (nrow(list.prev[[x]]) == 0 ){
                   oc.dt[x, var.prev]
                 } else { cbind(list.prev[[x]]
                                , oc.dt[x, var.prev]
                                , stringsAsFactors = F)}  )
          , use.names = T, fill = T)
         data.table::setnames(oc.dt, names(oc.dt), gsub(pattern = "[ ]|[_]", ".",
                                    names(oc.dt)))
         data.table::setnames(prev.dt, names(prev.dt), gsub(pattern = "[ ]|[_]", ".",
                                            names(prev.dt)))

          list1 <- c(list1, list(oc.dt))
          list2 <- c(list2, list(prev.dt))

        }

         oc.dt <- data.table::rbindlist(list1, use.names = T, fill = T)
         prev.dt <- data.table::rbindlist(list2, use.names = T, fill = T)

      }

      if ("package:data.table" %in% search()){
      list(oc.dt = oc.dt, prev.dt = prev.dt)
      } else{
        list(oc.dt = data.frame(oc.dt), prev.dt = data.frame(prev.dt))
      }
    }
  } else {print("Connection error.")}
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
#'  The returned object is a list of two data.table if the data.table package is attached. Otherwise, it is a list of two data.frame.
#'  Here, using the data.table package speed up the script.
#'
#' @export
#'
fingerprint.func <- function(x)
  stringr::str_trim(
        as.character(
            sapply(x, function(y)
              iconv(
                paste(
                  sort(
                    unique(
                      stringr::str_split(
                        gsub("[[:punct:]]|[[:cntrl:]]", ""
                             , tolower(
                               stringr::str_trim(y)
                               )
                             ), " "
                        )[[1]]
                      )
                    )
                  , collapse = " ")
                )
              )
            )
        )

#'Query opencorporate website
#'
#' @param term Term to query on the open corporate website
#' @param nb.page Number of pages to query
#' @param token Token for the owner of an opencorporate account
#' @param country List or vector of countries to query
#' @param ret.score Return the top results by score. Otherwise, the results are returned by alphabetical order.
#'
#' @details The returned object is two data.table if the data.table package is attached. Otherwise, it is two data.frame.
#'  Here, using the data.table package speed up the script.
#'
#' @note more on the API documentation: https://api.opencorporates.com/documentation/API-Reference#account_status
#'
#' @export
#'
#' @examples
#'   get.officers("Vincent de Rivaz", nb.page = 2)
#'
get.officers <- function(term, nb.page = 20, token = NULL, country = NULL, ret.score = FALSE) {
  # term <- "Vincent de Rivaz" ; nb.page <- 3 ; token <- NULL ; country <- NULL
  # library(stringr); library(jsonlite)

  officers.query <- "https://api.opencorporates.com/v0.4/officers/search?q="
  var.officers <- c("id", "uid"
      , "name", "jurisdiction_code", "position", "retrieved_at"
      , "opencorporates_url", "start_date", "end_date"
      , "occupation", "current_status", "inactive")

  # replace space by +
  term <- gsub(pattern = " ", replacement = "+", stringr::str_trim(term))

  if(is.null(country)){
    search.json <- paste0(officers.query, term)
  } else{
    search.json <- paste0(officers.query, term, "&jurisdiction_code=", country)
  }

  # if registered account, add the api key
  if(!is.null(token)) search.json <- paste0(search.json, "?api_token=", token)

  # if no internet connection, return an error
  try(res.json <- jsonlite::fromJSON(search.json), silent = T)

  if(exists("res.json")){

    nb.pages <- res.json$results$total_pages

    if(nb.pages == 0){
      print("No result. Please try another query.")
    } else{

      ### without an API key, it is only possible to query up to 20.
      if(is.null(token)){
        if(min(nb.pages, nb.page) > 20){nb.pages <- 20
        } else{nb.pages <- min(nb.pages, nb.page)} }

      off.dt <- res.json$results$officers$officer[, var.officers]
      comp.dt <- data.frame(company.name = res.json$results$officers$officer$company$name
          , company_number = res.json$results$officers$officer$company$company_number
          , company.jurisdiction_code = res.json$results$officers$officer$company$jurisdiction_code
          , company.opencorporates_url = res.json$results$officers$officer$company$opencorporates_url
          , stringsAsFactors = F)

      list1 <- list(data.table::data.table(off.dt, comp.dt))

      ##### Final function to query the full dataset:
      if(nb.pages > 1){ # nb.pages <- 2

        for(x in 2:nb.pages) { # x <- 2 ; x <- x + 1
          if(is.null(country)){
            search.json <- paste0(officers.query, term, "&page=", x)
          } else{
            search.json <- paste0(officers.query, term, "&jurisdiction_code=", country, "&page=", x)
          }

          res.json <- jsonlite::fromJSON(search.json)

          off.dt <- res.json$results$officers$officer[, var.officers]
          comp.dt <- data.frame(company.name = res.json$results$officers$officer$company$name
              , company_number = res.json$results$officers$officer$company$company_number
              , company.jurisdiction_code = res.json$results$officers$officer$company$jurisdiction_code
              , company.opencorporates_url = res.json$results$officers$officer$company$opencorporates_url
              , stringsAsFactors = F)

          list1 <- c(list1, list(data.table::data.table(off.dt, comp.dt)))

        }

      }
      officers.dt <- data.table::rbindlist(list1, use.names = T, fill = T)

      # Clean the name of the table
      data.table::setnames(officers.dt, names(officers.dt)
            , gsub(pattern = "[ ]|[_]", ".", names(officers.dt)))

      if ("package:data.table" %in% search()){
        officers.dt = officers.dt

      } else{
        officers.dt = data.frame(officers.dt)
      }
    }
  } else {print("Connection error.")}
}

#'
#' Query opencorporate website to search a given company number in a given jurisdiction
#'
#' @param company.number A company number or a vector of comapny numbers.
#' @param jurisdiction.code A jurisdiction code or a vector of jurisdiction code which match the length of company.number.
#' @param token Token for the owner of an opencorporate account
#'
#' @details The returned object is two data.table if the data.table package is attached. Otherwise, it is two data.frame.
#'  Here, using the data.table package speed up the script.
#'
#' @note The returned object is a list of four data.table if the data.table package is attached. Otherwise, it is a list of four data.frame.
#'  Here, using the data.table package speed up the script.
#'
#' @export
#'
#' @examples
#'    get.comp.number(c("23286336", "30760997"), c("ro", "ro"))
#'
get.comp.number <- function(company.number, jurisdiction.code
        , token = NULL) {
    # company.number <- c("23286336", "30760997")
    # jurisdiction.code <- c("ro", "ro"); token <- NULL
    # library(stringr); library(jsonlite)

  # path call get company number:
  call.api.number <- "https://api.opencorporates.com/v0.4/companies/"

  var.comp.numb <- c("name", "company_number", "jurisdiction_code"
                     , "incorporation_date", "dissolution_date", "company_type"
                     , "registry_url", "branch_status", "inactive", "current_status"
                     , "created_at", "updated_at"
                     , "retrieved_at", "opencorporates_url", "agent_name"
                     , "agent_address", "number_of_employees"
                     , "registered_address_in_full", "home_company")

  var.source <- c("publisher", "url", "terms", "terms_url")

  var.identifiers <- c("uid", "identifier_system_code"
                       , "identifier_system_name")

  if(length(company.number) != length(jurisdiction.code))
    stop("company.number and jurisdiction.code should have the same length.")


# loop over the company numbers
list1 <- lapply(1:length(company.number), function(i) {

    # call the app with details
    search.comp.id <- paste0(call.api.number, "/", jurisdiction.code[i]
                             , "/", company.number[i])

    # if registered account, add the api key
    if(!is.null(token)) search.comp.id <- paste0(search.comp.id, "?api_token=", token)

    try(res.json.comp <- jsonlite::fromJSON(search.comp.id), silent = T)
    if (!exists("res.json.comp"))
        stop("wrong call. Check company.number.")

    # details of the company
    details.company <- res.json.comp$results$company

  } )

  # create a dataframe with one value per id
  list.df.var.uniq <- lapply(list1, function(x)
    data.frame(t(unlist(x[var.comp.numb]))
               , t(unlist(x$source[var.source]))
               , stringsAsFactors = F))
  company.id.dt <- data.table::rbindlist(list.df.var.uniq, use.names = T, fill = T)

  # Clean the name of the table
  data.table::setnames(company.id.dt, names(company.id.dt)
           , gsub("[ ]|[_]", ".", names(company.id.dt)))

  # industry_codes:
  ind.c.l <- lapply(list1, function(x) if(length(x$industry_codes) != 0)
    cbind(x$industry_codes$industry_code
          , company_number = x$company_number
          , jurisdiction_code = x$jurisdiction_code
          , stringsAsFactors = F))

  industry.code.dt <- data.table::rbindlist(ind.c.l[!unlist(lapply(ind.c.l
        , is.null))], use.names = T, fill = T)

  # Clean the name of the table
  if(ncol(industry.code.dt) != 0)
  data.table::setnames(industry.code.dt, names(industry.code.dt)
           , gsub("[ ]|[_]", ".", names(industry.code.dt)))

  # identifiers:
  ind.ident <- lapply(list1, function(x) if(length(x$identifiers) != 0)
    cbind(x$identifiers$identifier
          , company_number = x$company_number
          , jurisdiction_code = x$jurisdiction_code
          , stringsAsFactors = F))

  identifiers.dt <- data.table::rbindlist(ind.ident[!unlist(lapply(ind.ident
         , is.null))], use.names = T, fill = T)

  # Clean the name of the table
  if(ncol(identifiers.dt) != 0)
  data.table::setnames(identifiers.dt, names(identifiers.dt)
           , gsub("[ ]|[_]", ".", names(identifiers.dt)))

  # officers:
  ind.oficers <- lapply(list1, function(x) if(length(x$officers) != 0)
    cbind(x$officers$officer
          , company_number = x$company_number
          , jurisdiction_code = x$jurisdiction_code
          , stringsAsFactors = F))

  officers.comp.dt <- data.table::rbindlist(ind.oficers[!unlist(lapply(ind.oficers
                 , is.null))], use.names = T, fill = T)

  if(ncol(officers.comp.dt) != 0)
    data.table::setnames(officers.comp.dt, names(officers.comp.dt)
             , gsub("[ ]|[_]", ".", names(officers.comp.dt)))

  if ("package:data.table" %in% search()){
    company.out.l <- list(company.id.dt = company.id.dt, officers.comp.dt = officers.comp.dt
                      , identifiers.dt = identifiers.dt, industry.code.dt = industry.code.dt)
  } else{
    company.out.l <- list(company.id.dt = data.frame(company.id.dt)
        , officers.comp.dt = data.frame(officers.comp.dt)
        , identifiers.dt = data.frame(identifiers.dt)
        , industry.code.dt = data.frame(industry.code.dt))
  }
}
