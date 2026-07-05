download_scihub<-function (query, path = tempfile(fileext = ".pdf"), open = TRUE) 
{
  content <- read_html(paste0("https://sci-hub.ru/", query))
  paper_url_raw <- content %>% xml_find_all("//*[@id='pdf']") %>% 
    xml_attr("src") %>% parse_url()
  unescape_html <- function(str) {
    lapply(str, function(x) {
      xml_text(read_html(paste0("<x>", x, "</x>")))
    })
  }
  paper_citation <- content %>% xml_find_all("//div[@id='citation']//text()") %>% 
    as.character() %>% str_trim() %>% unescape_html()
  paper_url_raw$scheme <- "https"
  paper_url_raw$fragment <- NULL
  paper_url <- paper_url_raw %>% build_url()
  read_file_raw(paper_url) %>% writeBin(path)
  ui_info("Cite as:")
  ui_code_block(paper_citation)
  if (open) 
    system2("open", path)
}


safe_read_html <- function(url, 
                           user_agent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/108.0.0.0 Safari/537.36",
                           timeout_seconds = 10) {
  
  response <- tryCatch(
    {
      httr::GET(
        url = url, 
        add_headers(`User-Agent` = user_agent),
        httr::timeout(timeout_seconds)
      )
    },
    error = function(e) {
      message(sprintf("Network request error %s\nURL: %s", e$message, url))
      return(NULL)
    }
  )
  if (is.null(response)) {
    return(NULL)
  }
  status <- httr::status_code(response)
  
  if (status == 200) {
    html_content <- httr::content(response, as = "text", encoding = "UTF-8")
    #return(html_content)
    #if (F){
      webpage <- tryCatch({
        read_html(html_content)
      }, 
      error = function(e){
        message(sprintf("HTML pasted failed %s\nURL: %s", e$message, url))
        return(NULL)
      })
      
      return(webpage)
    #}
    
  } else {
    message(sprintf("HTTP request error code %d - %s\nURL: %s", 
                    status, http_status(status)$reason, url))
    return(NULL)
  }
}

library(urltools)

get_base_url <- function(url) {
  parsed_urls <- url_parse(url)
  base_urls <- paste0(parsed_urls$scheme, "://", parsed_urls$domain)
  
  return(base_urls)
}
