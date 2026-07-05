
library("rwosstarter")
library("data.table")
library("pdftools")
library("stringr")
library("stringi")
library("RCurl")
library("rvest")
library("httr")
library("pdftools")


setwd("/path_to_your_project")
source("Download.PDF/wos.functions.r")
source("../tokens.r")
articles.biogeography<-readRDS("../Data/BIOGEOGRAPHY/articles.crossref.rda")
doi_list<-unique(articles.biogeography$doi)
BATCH_SIZE <- 40 
database<-"WOS"
total_dois <- length(doi_list)
num_batches <- ceiling(total_dois / BATCH_SIZE)
i=1
for (i in 1:num_batches) {
  start_idx <- (i - 1) * BATCH_SIZE + 1
  end_idx <- min(i * BATCH_SIZE, total_dois)
  
  current_batch <- sprintf('"%s"', doi_list[start_idx:end_idx])
  doi_query_string <- paste(current_batch, collapse = " OR ")
  
  query_str <- paste0("DO=(", doi_query_string, ")")
  
  tryCatch({
    response <- httr::GET(
      url = "https://api.clarivate.com/apis/wos-starter/v2/documents",
      query = list(
        db = database,
        q = query_str,
        limit = 50,
        page = 1
      ),
      config = httr::add_headers(
        accept = "application/json",
        `X-ApiKey` = get_token()
      )
    )
    
    httr::stop_for_status(response)
    print(sprintf("%s/%s requests remaining today. %d/%d pages. %s", response$headers$`x-ratelimit-remaining-day`,
                  response$headers$`x-ratelimit-limit-day`,
                  page, length(pages), journal))
    content <- httr::content(response, as = "text", encoding = "UTF-8")
    content <- jsonlite::fromJSON(content)
    content$hits
    saveRDS(content, target)
    
  },
  error = function(e) {
    message("Error: ", e$message)
    
  },
  warning = function(w) {
    message("Warning: ", w$message)
    
  },
  finally = {
    
  })
}