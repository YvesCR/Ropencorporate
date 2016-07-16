#'
#' Wrapper for the get: companies method of the open corporate API
#'
#' @param term Term to query on the open corporate API
#' @param nb.page Number of pages to query. Without token, could be up to 20 and no more than 100 a day.
#' @param token Token for the owner of an opencorporate account
#' @param country List or vector of countries to query.
#'
#' @note The returned object is a list of two data.table if the data.table package is attached. Otherwise, it is a list of two data.frame.
#'  Here, using the data.table package speed up the script.
#'  More on the API documentation: https://api.opencorporates.com/documentation/API-Reference#account_status
#'
#' @export
#'
#' @examples
#'   get.companies('gold holding', nb.page = 2)
#'
get.companies <- function(term, nb.page = 20, token = NULL, country = NULL) {
# term <- "edf" ; nb.page <- 2 ; token <- NULL ; country <- NULL

  name.var <- c("name", "company_number", "jurisdiction_code", "incorporation_date"
      , "dissolution_date", "company_type", "registry_url", "branch_status"
      , "inactive", "current_status", "created_at", "updated_at"
      , "retrieved_at", "opencorporates_url"
      , "registered_address_in_full", "restricted_for_marketing")

  call.func.api <- "https://api.opencorporates.com/v0.4/companies/search?q="

  var.prev <- c("company_number", "jurisdiction_code")

  class.list.keep <- c("character", "integer", "logical", "Date", "numeric")

  # replace space by +
  term <- gsub(pattern = " ", replacement = "+", stringr::str_trim(term))

  if(is.null(country)){
    search.json <- paste0(call.func.api, term)
  } else{
    search.json <- paste0(call.func.api, term, "*jurisdiction_code=", country)
  }

  # if registered account, add the api key
  if(!is.null(token)) {
    search.json <- paste0(search.json, "&page=1&api_token=", token)
    try(get.status <- httr::GET(paste0("https://api.opencorporates.com/v0.4/account_status?api_token=", token)), silent = T)
    
    if(exists("get.status")){
      account.status <- jsonlite::fromJSON(httr::content(get.status, "text"))
      print(paste0("Calls made today:", account.status$results$account_status$usage$today))
      print(paste0("Calls made this month:", account.status$results$account_status$usage$this_month))
      print(paste0("Calls remaining today:", account.status$results$account_status$calls_remaining$today))
      print(paste0("Calls remaining this month:", account.status$results$account_status$calls_remaining$this_month))
    }
  }

  # if no internet connection, return an error
  try(get.id <- httr::GET(search.json), silent = T)
  
  if(exists("get.id")){

    res.json <- jsonlite::fromJSON(httr::content(get.id, "text"))
    nb.pages <- res.json$results$total_pages

    if(nb.pages == 0){
      print("No result. Please try another query.")
    } else{

      ### without an API key, it is only possible to query up to 20.
      if(is.null(token)){
        if(min(nb.pages, nb.page) > 20) nb.pages.fin <- 20
      } else{nb.pages.fin <- min(nb.pages, nb.page)}

      oc.dt <- res.json$results$companies$company[, name.var]

      list.prev <- res.json$results$companies$company$previous_names

      # x <- 1
      prev.dt <- data.table::rbindlist(lapply(1:nrow(oc.dt)
            , function(x) if(is.null(nrow(list.prev[[x]]))){
                    oc.dt[x, var.prev]
                } else if (nrow(list.prev[[x]]) == 0 ){
                    oc.dt[x, var.prev]
                } else { prev.step <- cbind(list.prev[[x]]
                      , oc.dt[x, var.prev]
                      , stringsAsFactors = F)
                prev.step[, sapply(prev.step, class) %in% class.list.keep] }  )
            , use.names = T, fill = T)

      data.table::setnames(oc.dt, names(oc.dt), gsub(pattern = "[ ]|[_]", ".",
               names(oc.dt)))
      data.table::setnames(prev.dt, names(prev.dt), gsub(pattern = "[ ]|[_]", ".",
                   names(prev.dt)))

      list1 <- list(oc.dt)
      list2 <- list(prev.dt)
      
      page.num <- 1
      ##### Final function to query the full dataset:
      if(nb.pages.fin > 1){ # nb.pages <- 2
        while(get.id$status_code == 200 & page.num < nb.pages.fin) { # page.num <- 1 ;
          page.num <- page.num + 1
          search.json <- NULL ; get.id <- NULL
          if(is.null(country)){
            search.json <- paste0(call.func.api, term, "&page=", page.num)
          } else{
            search.json <- paste0(call.func.api, term, "*jurisdiction_code=", country, "&page=", page.num)
          }
          # if registered account, add the api key
          if(!is.null(token)) search.json <- paste0(search.json, "&api_token=", token)

          try(get.id <- httr::GET(search.json))
          if(get.id$status_code == 200) {
            res.json <- jsonlite::fromJSON(httr::content(get.id, "text"))
             
            oc.dt <- res.json$results$companies$company[ , name.var]
  
            list.prev <- res.json$results$companies$company$previous_names
  
            prev.dt <- data.table::rbindlist(lapply(1:nrow(oc.dt)
                , function(x)
                  if(is.null(nrow(list.prev[[x]]))){ # x <- 4
                   oc.dt[x, var.prev]
                  } else if (nrow(list.prev[[x]]) == 0 ){
                    oc.dt[x, var.prev]
                  } else { prev.step <- cbind(list.prev[[x]]
                      , oc.dt[x, var.prev]
                      , stringsAsFactors = F)
                    prev.step[, sapply(prev.step, class) %in% class.list.keep] 
                   }
                )
              , use.names = T, fill = T)
  
            data.table::setnames(oc.dt, names(oc.dt)
              , gsub(pattern = "[ ]|[_]", ".", names(oc.dt)))
            data.table::setnames(prev.dt, names(prev.dt)
              , gsub(pattern = "[ ]|[_]", ".", names(prev.dt)))

            list1[[page.num]] <- oc.dt
            list2[[page.num]] <- prev.dt
            
          }
        }
      }

      if(page.num != nb.pages.fin) warning(paste0(page.num, "Pages loaded",
                    " over ", nb.pages, " available. Check for issues") )

      oc.dt <- data.table::rbindlist(list1, use.names = T, fill = T)
      prev.dt <- data.table::rbindlist(list2, use.names = T, fill = T)

    }

    if ("package:data.table" %in% search()){
        res.oc <- list(oc.dt = oc.dt, prev.dt = prev.dt)
    } else{
        res.oc <- list(oc.dt = data.frame(oc.dt), prev.dt = data.frame(prev.dt))
    }
  } else {print("Connection error.")}
}
#
#' Function to clean a given string based on the fingerprint method.
#'
#' @param x a vector of string.
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
fingerprint.func <- function(x)
  stringr::str_trim(
        as.character(
            sapply(x, function(y)
              iconv(
                paste(
                  sort(
                    unique(
                      strsplit(
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

#'
#' Wrapper for the get: officers method of the open corporate API
#'
#' @param term Term to query on the open corporate website
#' @param nb.page Number of pages to query. Without token, could be up to 20.
#' @param token Token for the owner of an opencorporate account
#' @param country List or vector of countries to query
#' @param ret.score Return the top results by score. Otherwise, the results are returned by alphabetical order.
#'
#' @return The returned object is two data.table if the data.table package is attached. Otherwise, it is two data.frame.
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
  # term <- "Vincent de Rivaz" ; nb.page <- 2 ; token <- NULL ; country <- NULL

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

  # if registered account, add the api key and check the account status
  if(!is.null(token)) {
    search.json <- paste0(search.json, "&api_token=", token)
    try(get.status <- httr::GET(paste0("https://api.opencorporates.com/v0.4/account_status?api_token=", token)), silent = T)
    
    if(exists("get.status")){
      account.status <- jsonlite::fromJSON(httr::content(get.status, "text"))
      print(paste0("Calls made today:", account.status$results$account_status$usage$today))
      print(paste0("Calls made this month:", account.status$results$account_status$usage$this_month))
      print(paste0("Calls remaining today:", account.status$results$account_status$calls_remaining$today))
      print(paste0("Calls remaining this month:", account.status$results$account_status$calls_remaining$this_month))
    }
  }
  
  # if no internet connection, return an error
  try(get.id <- httr::GET(search.json), silent = T)
  
  if(exists("get.id")){
    
    res.json <- jsonlite::fromJSON(httr::content(get.id, "text"))
    nb.pages <- res.json$results$total_pages

    if(nb.pages == 0){
      print("No result. Please try another query.")
    } else{

      ### without an API key, it is only possible to query up to 20.
      if(is.null(token)){
        if(min(nb.pages, nb.page) > 20) nb.pages.fin <- 20
        } else{nb.pages.fin <- min(nb.pages, nb.page)}

      off.dt <- res.json$results$officers$officer[, var.officers]
      comp.dt <- data.frame(company.name = res.json$results$officers$officer$company$name
          , company_number = res.json$results$officers$officer$company$company_number
          , company.jurisdiction_code = res.json$results$officers$officer$company$jurisdiction_code
          , company.opencorporates_url = res.json$results$officers$officer$company$opencorporates_url
          , stringsAsFactors = F)

      list1 <- list(data.table::data.table(off.dt, comp.dt))

      page.num <- 1
      ##### Final function to query the full dataset:
      if(nb.pages.fin > 1){ # nb.pages <- 2
        while(get.id$status_code == 200 & page.num < nb.pages.fin) { # page.num <- 1 ;
          page.num <- page.num + 1
          search.json <- NULL ; get.id <- NULL
          if(is.null(country)){
            search.json <- paste0(officers.query, term, "&page=", page.num)
          } else{
            search.json <- paste0(officers.query, term
              , "&jurisdiction_code=", country, "&page=", page.num)
          }
          
          # if registered account, add the api key
          if(!is.null(token)) search.json <- paste0(search.json, "&api_token=", token)
          
          try(get.id <- httr::GET(search.json))
          if(get.id$status_code == 200) {
            res.json <- jsonlite::fromJSON(httr::content(get.id, "text"))

            off.dt <- res.json$results$officers$officer[, var.officers]
            comp.dt <- data.frame(company.name = res.json$results$officers$officer$company$name
                , company_number = res.json$results$officers$officer$company$company_number
                , company.jurisdiction_code = res.json$results$officers$officer$company$jurisdiction_code
                , company.opencorporates_url = res.json$results$officers$officer$company$opencorporates_url
                , stringsAsFactors = F)
            # list1.1 <- list1
            list1[[page.num]] <- data.table::data.table(off.dt, comp.dt)
          }
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
#' @return The returned object is a list of four data.table if the data.table package is attached. Otherwise, it is a list of four data.frame.
#'  Here, using the data.table package speed up the script.
#'
#' \itemize{
#' \item company.id.dt: Company details
#' \item officers.comp.dt: List of officers. Link with the previous table withcompany.number and jurisdiction.code
#' \item identifiers.dt: Identifiers
#' \item industry.code.dt: Industry code. Link with the previous tables with company.number and jurisdiction.code
#' }
#'
#' @export
#'
#' @examples
#'    get.comp.number(c("02228297", "30760997"), c("gb", "ro"))
#'
get.comp.number <- function(company.number, jurisdiction.code
        , token = NULL) {
    # company.number <- c("02228297", "30760997")
    # jurisdiction.code <- c("gb", "ro"); token <- NULL

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

  # create all the query at once:
  search.comp.id <- paste0(call.api.number, jurisdiction.code
                           , "/", company.number)
  
  # if registered account, add the api key
  if(!is.null(token)) search.comp.id <- paste0(search.comp.id
       , "&api_token=", token)

  # initialise the iteration
  item.num <- 0; item.op.num <- 0 ; miss.item <- NULL; ind.cl.num <- 0
  item.id.num <- 0; item.of.num <- 0
  list1 <- list(); list2 <- list(); list3 <- list(); list4 <- list()
  
  # loop over the company numbers:
  # if ok, stock; if not, don't
  while(item.num < length(company.number)){
    item.num <- item.num + 1
    get.id <- NULL ; res.json.comp <- NULL
    
    # if no internet connection, return an error
    try(get.id <- httr::GET(search.comp.id[item.num]), silent = T)
    if(exists("get.id") & get.id$status_code == 200){
    
      item.op.num <- item.op.num + 1
      res.json.comp <- jsonlite::fromJSON(httr::content(get.id, "text"))
      small.get <- res.json.comp$results$company
      list1[[item.op.num]] <- data.frame(t(unlist(small.get[var.comp.numb]) )
                 , t(unlist(small.get$source[var.source]) )
                 , stringsAsFactors = F)
      
      if(length(small.get$industry_codes) != 0){
        ind.cl.num <- ind.cl.num + 1
        list2[[ind.cl.num]] <- cbind(small.get$industry_codes$industry_code
             , company_number = small.get$company_number
             , jurisdiction_code = small.get$jurisdiction_code
             , stringsAsFactors = F)
      }
      
      if(length(small.get$identifiers) != 0){
        item.id.num <- item.id.num + 1
        list3[[item.id.num]] <- cbind(small.get$identifiers$identifier
            , company_number = small.get$company_number
            , jurisdiction_code = small.get$jurisdiction_code
            , stringsAsFactors = F)
      }
      
      if(length(small.get$officers$officer) != 0){
        item.of.num <- item.of.num + 1
        list4[[item.of.num]] <- cbind(small.get$officers$officer
            , company_number = small.get$company_number
            , jurisdiction_code = small.get$jurisdiction_code
            , stringsAsFactors = F)
      }

    } else{miss.item <- c(miss.item, item.num)}
  }
  
  if(!is.null(miss.item))
    warning(paste0("items "
      , paste(miss.item, collapse = ", "), " not loaded"))

  # create the data frames:
  # company.id.dt
  if(length(list1) != 0){ 
    company.id.dt <- data.table::rbindlist(list1, use.names = T, fill = T)

    # Clean the name of the table
    data.table::setnames(company.id.dt, names(company.id.dt)
           , gsub("[ ]|[_]", ".", names(company.id.dt)))
  } else{ company.id.dt <- NULL}
    
  # industry_codes:
  if(length(list2) != 0){ 
    industry.code.dt <- data.table::rbindlist(list2, use.names = T, fill = T)
    
    # Clean the name of the table
      data.table::setnames(industry.code.dt, names(industry.code.dt)
             , gsub("[ ]|[_]", ".", names(industry.code.dt)))
    
  } else{ industry.code.dt <- NULL}
  
  # identifiers:
  if(length(list3) != 0){ 
    identifiers.dt <- data.table::rbindlist(list3, use.names = T, fill = T)
    
    # Clean the name of the table
    data.table::setnames(identifiers.dt, names(identifiers.dt)
                         , gsub("[ ]|[_]", ".", names(identifiers.dt)))
    
  } else{ identifiers.dt <- NULL}
  
  # officers:
  if(length(list4) != 0){ 
    officers.comp.dt <- data.table::rbindlist(list4, use.names = T, fill = T)
    
    # Clean the name of the table
    data.table::setnames(officers.comp.dt, names(officers.comp.dt)
                         , gsub("[ ]|[_]", ".", names(officers.comp.dt)))
    
  } else{ officers.comp.dt <- NULL}
  
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
