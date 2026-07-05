
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
source("Download.PDF/download.pdf.r")
source("Download.PDF/read_html.r")
source("API.KEYS/tokens.r")

#GEB:Online ISSN:1466-8238 Print ISSN:1466-822X
journal<-"GLOBAL ECOLOGY AND BIOGEOGRAPHY"
issn.str<-sprintf("(%s OR %s)", "1466-8238", "1466-822X")

#ECOGRAPHY:Online ISSN:1600-0587 Print ISSN:0906-7590
journal<-"ECOGRAPHY"
issn.str<-sprintf("(%s OR %s)", "1600-0587", "0906-7590")

#JBI:Online ISSN:1365-2699 Print ISSN:0305-0270
journal<-"JOURNAL OF BIOGEOGRAPHY"
issn.str<-sprintf("(%s OR %s)", "1365-2699", "0305-0270")

#DDI:Online ISSN:1472-4642 Print ISSN:1366-9516
journal<-"DIVERSITY AND DISTRIBUTIONS"
issn.str<-sprintf("(%s OR %s)", "1472-4642", "1366-9516")

query <- sprintf("(IS=%s)",
                 issn.str)
database<-"WOS"
query <- utils::URLencode(query, reserved = TRUE)
n_refs <- wos_search(query, database)

if (n_refs == 0) {
  next()
}
folder<-sprintf("/path_to_your_project/Data/BIOGEOGRAPHY/WOS/%s", journal)
if (!dir.exists(folder)){
  dir.create(folder)
}
refs <- data.frame()
n_records_per_page <- 50
sleep<-1
pages <- seq(1, ceiling(n_refs/n_records_per_page), by = 1)
for (page in pages) {
  target<-sprintf("%s/%d.rda", folder, page)
  
  if (file.exists(target)){
    next()
  }
  tryCatch({
    print(target)
    request <- paste0(api_url(), "/documents", "?db=", database, 
                      "&q=", query, "&limit=", n_records_per_page, "&page=", 
                      page)
    response <- httr::GET(url = request, config = httr::add_headers(accept = "application/json", 
                                                                    `X-ApiKey` = get_token()))
    httr::stop_for_status(response)
    print(sprintf("%s/%s requests remaining today. %d/%d pages. %s", response$headers$`x-ratelimit-remaining-day`,
                  response$headers$`x-ratelimit-limit-day`,
                  page, length(pages), journal))
    content <- httr::content(response, as = "text", encoding = "UTF-8")
    content <- jsonlite::fromJSON(content)
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
  if (length(pages) > 1) 
    Sys.sleep(sample(seq(0, sleep, by = 0.01), 1))
  
}

folder<-sprintf("/path_to_your_project/Data/BIOGEOGRAPHY/WOS/%s", journal)
contents<-list.files(folder, pattern="\\.rda")

all<-list()
for (j in c(1:length(contents))){
  content<-readRDS(sprintf("%s/%s", folder, contents[j]))
  content <- content$hits
  uid <- list_to_df(get_field(content, "uid"))
  document_type <- list_to_df(get_field(content, "sourceTypes"))
  title <- list_to_df(get_field(content, "title"))
  authors <- unlist(lapply(get_field(content$names, "authors"), 
                           function(x) {
                             if (!is.data.frame(x)) {
                               NA
                             }
                             else {
                               paste0(x[, 1], collapse = " | ")
                             }
                           }))
  published_year <- list_to_df(get_field(content$source, 
                                         "publishYear"))
  published_month <- list_to_df(get_field(content$source, 
                                          "publishMonth"))
  source <- list_to_df(get_field(content$source, "sourceTitle"))
  volume <- list_to_df(get_field(content$source, "volume"))
  issue <- list_to_df(get_field(content$source, "issue"))
  pages <- list_to_df(get_field(content$source$pages, 
                                "range"))
  no_article <- list_to_df(get_field(content$source, "articleNumber"))
  supplement_number <- list_to_df(get_field(content$source, 
                                            "supplement"))
  special_issue <- list_to_df(get_field(content$source, 
                                        "specialIssue"))
  book_editors <- unlist(lapply(get_field(content$names, 
                                          "bookEditors"), function(x) {
                                            if (!is.data.frame(x)) {
                                              NA
                                            }
                                            else {
                                              paste0(x[, 1], collapse = " | ")
                                            }
                                          }))
  keywords <- list_to_df(get_field(content$keywords, "authorKeywords"))
  doi <- list_to_df(get_field(content$identifiers, "doi"))
  eissn <- list_to_df(get_field(content$identifiers, "eissn"))
  issn <- list_to_df(get_field(content$identifiers, "issn"))
  isbn <- list_to_df(get_field(content$identifiers, "isbn"))
  pmid <- list_to_df(get_field(content$identifiers, "pmid"))
  citations <- unlist(lapply(get_field(content, "citations"), 
                             function(x) {
                               ifelse(is.na(x$count), NA, x$count)
                             }))
  data <- data.table(uid, document_type, title, authors, 
                     published_year, published_month, source, volume, 
                     issue, pages, no_article, supplement_number, special_issue, 
                     book_editors, keywords, doi, eissn, issn, isbn, 
                     pmid, citations)
  data$journal<-journal
  all[[length(all)+1]]<-data
}
all.df<-rbindlist(all)
all.df[, c("doi.prefix", "doi.suffix") := {
  parts <- stri_split_fixed(doi, "/", n = 2)
  list(sapply(parts, `[`, 1), sapply(parts, `[`, 2))
}]
all.df$pdf.path.hold<-sprintf("%s/%s/%s.PDF", 
                        "#../Data/Literature/PDF", journal, 
                        URLencode(toupper(all.df$doi.suffix), reserved = T))
saveRDS(all.df, sprintf("/path_to_your_project/Data/BIOGEOGRAPHY/WOS/%s.rda", journal))


all.df<-readRDS(sprintf("/path_to_your_project/Data/BIOGEOGRAPHY/WOS/%s.rda", journal))
d.item<-all.df[published_year==2025]
#d.item<-all.df



for (i in rev(c(1:nrow(d.item)))){
  item<-d.item[i]
  print(paste(i, nrow(d.item)))
  if (file.exists(item$pdf.path.hold)){
    print("Skip")
    next()
  }
  
  if (item$doi.prefix=="NA") {
    next()
  }
 
  code.frame<-
    tryCatch({
      download.wiley<-function(wiley.api, url, doi.prefix, doi.suffix, filename){
        pdf.url <- sprintf("https://api.wiley.com/onlinelibrary/tdm/v1/articles/%s/%s", doi.prefix, URLencode(doi.suffix, reserved = T))
        print(pdf.url)
        token <- wiley.api
        headers <- c("Wiley-TDM-Client-Token" = token)
        code.frame<-download.pdf.url(pdf.url, url, host="wiley.com", headers=headers, filename=filename, sleep=10)
        return(code.frame)
      }
      
      download.wiley(wiley.api[4], "", 
                     item$doi.prefix, 
                     gsub("1566-822x", "1466-822X", item$doi.suffix), 
                     item$pdf.path.hold)
      
      
    },
    error = function(e) {
      message("Error: ", e$message)
      return(data.table(code=-1, note=e$message, sleep=10))
    },
    warning = function(w) {
      message("Warning: ", w$message)
      return(data.table(code=-1, note=w$message, sleep=10))
    },
    finally = {
      
    })
  print(sprintf("Download stauts code: %d", code.frame$code))
  Sys.sleep(10)
  
}
