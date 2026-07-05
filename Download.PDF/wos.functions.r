
api_url<-function(){
  "https://api.clarivate.com/apis/wos-starter/v1"
}
get_token<-function(key="WOS_STARTER_KEY"){
  wos_token <- Sys.getenv(key)
  if (wos_token == "") {
    stop("Missing Web of Science API key.\n", "Please make sure you:\n", 
         " 1. have obtained you own API key, and\n", " 2. have stored the API key in the `~/.Renviron` file ", 
         "using the function `usethis::edit_r_environ()`.\n", 
         "    Add this line: WOS_STARTER_KEY='XXX' and restart R.")
  }
  wos_token
}
get_field<-function (data, field) 
{
  if (!(field %in% colnames(data))) {
    return(rep(NA, nrow(data)))
  }
  else {
    data[, field]
  }
}

list_to_df<-function (x) 
{
  if (is.null(x)) {
    NA
  }
  else {
    x <- unlist(lapply(x, function(y) paste0(y, collapse = " | ")))
    pos <- which(x == "")
    if (length(pos)) 
      x[pos] <- NA
    x
  }
}

wos_search<-function (query, database = "WOS") 
{
  if (!is.character(query)) {
    stop("Argument 'query' must be a character", call. = FALSE)
  }
  if (length(query) != 1) {
    stop("Argument 'query' must be a character of length 1", 
         call. = FALSE)
  }
  if (!is.character(database)) {
    stop("Argument 'database' must be a character", call. = FALSE)
  }
  if (length(database) != 1) {
    stop("Argument 'database' must be a character of length 1", 
         call. = FALSE)
  }
  database <- toupper(database)
  valid_databases <- c("BCI", "BIOABS", "BIOSIS", "CCC", "DIIDW", 
                       "DRCI", "MEDLINE", "PPRN", "WOK", "WOS", "ZOOREC")
  if (!(database %in% valid_databases)) 
    stop("Invalid 'database' value", call. = FALSE)
  query <- utils::URLencode(query, reserved = TRUE)
  request <- paste0(api_url(), "/documents", "?db=", database, 
                    "&q=", query, "&limit=", 1, "&page=", 1)
  response <- httr::GET(url = request, config = httr::add_headers(accept = "application/json", 
                                                                  `X-ApiKey` = get_token()))
  httr::stop_for_status(response)
  content <- httr::content(response, as = "text", encoding = "UTF-8")
  content <- jsonlite::fromJSON(content)
  content$metadata$total
}
