

download.pdf<-function(publisher, url, doi.prefix, doi.suffix, 
                       wiley.api="", elsevier.api="", filename, 
                       journal, xml=F){
  
  code.frame<- data.table(code=-5, note="Unhandled journal", sleep=0)
  if (journal %in% no_open_access){
    code.frame<- data.table(code=-6, note="no_open_access", sleep=0)
    return(code.frame)
  }
  if (grepl("link\\.springer-ny\\.com", url)){
    code.frame<- data.table(code=-2, note="springer-ny", sleep=0)
    return(code.frame)
  }
  if (publisher %in% cannot.download.journal.list){
    code.frame<- data.table(code=-1, note="cannot.download.journal.list", sleep=0)
    return(code.frame)
  }
  if (grepl("nature\\.com", url)){
    pdf.url <- sprintf("%s.pdf", url)
    
    code.frame<-download.pdf.url(pdf.url, url, host="nature.com", headers=NULL, filename=filename, sleep=1)
    return(code.frame)
  }
  if (publisher=="Academic Journals"){
    #https://academicjournals.org/journal/AJB/article-abstract/FC29AF165663
    #https://academicjournals.org/journal/AJB/article-full-text-pdf/FC29AF165663
    #download.file("https://academicjournals.org/journal/AJB/article-full-text/FC29AF165663", "~/Downloads/test.pdf")
  }
  if (publisher=="American Society for Horticultural Science"){
    print(publisher)
    pdf.url<-gsub("journals.ashs.org/view/journals", "journals.ashs.org/downloadpdf/view/journals", url)
    pdf.url<-gsub("\\.xml", "\\.pdf", pdf.url)
    v<-download.file(pdf.url, filename)
    print(filename)
    if (v!=0){
      file.remove(filename)
      code.frame<-data.table(code=-12, note="download error (American Society for Horticultural Science)", sleep=10)
      return(code.frame)
    }
    
    code.frame<- data.table(code=1, note="download successfully (American Society for Horticultural Science)", sleep=10)
    return(code.frame)
  }
  if (publisher %in% html.download.journal.list.type1){
    code.frame<-download.html(url, host=NULL, filename=filename, type=1)
    return(code.frame)
  }
  if (publisher %in% html.download.journal.list.type2){
    code.frame<-download.html(url, host=NULL, filename=filename, type=2)
    return(code.frame)
  }
  if (publisher=="Scientific Societies"){
    
  }
  
  if (publisher=="PeerJ"){
    pdf.url<-sprintf("%s.pdf",url)
    code.frame<-download.pdf.url(pdf.url, url, host="peerj.com", headers=NULL, filename=filename, sleep=1)
    return(code.frame)
  }
  
  if (publisher=="MDPI AG"){
    pdf.url<-sprintf("%s/pdf", url)
    code.frame<-download.pdf.url(pdf.url, url, host="mdpi.com", headers=NULL, filename=filename, sleep=10)
    return(code.frame)
  }
  if (endsWith(tolower(url), ".pdf")){
    code.frame<-download.pdf.url(url, url, host="google.com", headers=NULL, filename=filename, , sleep=10)
    return(code.frame)
  }
  if (grepl("\\.plos\\.", url)){
    code.frame<-download.html(url, host="plos.org", filename, sleep=1)
    return(code.frame)
  }
  if (grepl("\\.biomedcentral\\.", url)){
    code.frame<-download.html(url, host="biomedcentral.com", filename, sleep=10)
    return(code.frame)
  }
  if (publisher=="Smithsonian Institution"){
    pdf.url<-gsub("/part/", "/partpdf/", url)
    code.frame<- download.pdf.url(pdf.url, url, host="si.edu", headers=NULL, filename=filename, sleep=10)
    return(code.frame)
  }
  
  if (publisher=="American Association for the Advancement of Science (AAAS)"){
    code.frame<-data.table(code=-1, note="download error (AAAS)", sleep=10)
    return(code.frame)
    pdf.url<-sprintf("https://www.science.org/doi/pdf/%s?download=true",
                     gsub("https://www.science.org/doi/", "", url))
    code.frame<-download.pdf.url(pdf.url, url, host="science.org", headers=NULL, filename=filename)
    return(code.frame)
  }
  if (publisher=="Proceedings of the National Academy of Sciences"){
    code.frame<-data.table(code=-1, note="PNAS", sleep=10)
    return(code.frame)
    pdf.url<-sprintf("https://pnas.org/doi/pdf/%s?download=true",
                     gsub("https://pnas.org/doi/", "", url))
    code.frame<-download.pdf.url(pdf.url, url, host="pnas.org", headers=NULL, filename=filename)
    return(code.frame)
  }
  
  if (publisher=="Wiley" | grepl("\\.wiley\\.", url)){
    code.frame<-download.wiley(wiley.api, url, doi.prefix, doi.suffix, filename)
    return(code.frame)
  }
  if (publisher=="Elsevier BV"){
    code.frame<-download.elsevier(elsevier.api, url, doi.prefix, doi.suffix, filename, xml=xml)
    return(code.frame)
  }
  return(code.frame)
}

download.pdf.url<-function(pdf.url, url, host=NULL, headers=NULL, filename=NULL, format="application/pdf", sleep=10){
  if (is.null(headers)){
    if (!is.null(host)){
    headers <- c(
      Accept = "text/html,application/xhtml+xml,application/pdf,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8",
      `Accept-Encoding` = "gzip, deflate, br",
      `Accept-Language` = "en-US,en;q=0.5",
      Connection = "keep-alive",
      Referer = url,
      `Sec-Fetch-Dest` = "document",
      `Sec-Fetch-Mode` = "navigate",
      `Sec-Fetch-Site` = "same-origin",
      `Sec-Fetch-User` = "?1",
      `Upgrade-Insecure-Requests` = "1",
      `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:95.0) Gecko/20100101 Firefox/95.0"
    )
    }else{
      headers <- c(
        Accept = "text/html,application/xhtml+xml,application/pdf,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8",
        `Accept-Encoding` = "gzip, deflate, br",
        `Accept-Language` = "en-US,en;q=0.5",
        Connection = "keep-alive",
        host = host,
        Referer = url,
        `Sec-Fetch-Dest` = "document",
        `Sec-Fetch-Mode` = "navigate",
        `Sec-Fetch-Site` = "same-origin",
        `Sec-Fetch-User` = "?1",
        `Upgrade-Insecure-Requests` = "1",
        `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:95.0) Gecko/20100101 Firefox/95.0"
      )
    }
  }
  resp <- httr::GET(pdf.url, httr::add_headers(.headers = headers), 
                    httr::write_disk(filename, overwrite = TRUE), 
                    httr::progress(),
                    httr::config(proxy = ""))
  
  content_type <- resp$headers$`content-type`
  if (httr::status_code(resp) == 200 & grepl(format, content_type, ignore.case = TRUE)) {
    message(sprintf("%s Downloaded successfully. File size:%d KB. ", format, round(file.size(filename)/1024)), filename)
    code.frame<- data.table(code=1, note="Downloaded successfully", sleep=sleep)
    return(code.frame)
    break()
  } else {
    content.text<-httr::content(resp)
    if (is.null(content.text)){
      content.text<-""
    }
    if (is.null(content_type)){
      content_type<-""
    }
    message(sprintf("Download failed: HTTP %s, Content-Type=%s, Content=%s", 
                    httr::status_code(resp), content_type, substr(content.text, start = 1, stop = 100)))
    unlink(filename)
    msg<-sprintf("(%d):(%s)", httr::status_code(resp), content_type)
    if (grepl("wiley", pdf.url)){
      code.frame<- data.table(code=0, note=msg, sleep=10)
      return(code.frame)
    }
    code.frame<- data.table(code=0, note=msg, sleep=sleep)
    return(code.frame)
    break()
  }
  code.frame<- data.table(code=-1, note="Unknown status", sleep=sleep)
  return(code.frame)
}
download.wiley<-function(wiley.api, url, doi.prefix, doi.suffix, filename){
  print(wiley.api)
  pdf.url <- sprintf("https://api.wiley.com/onlinelibrary/tdm/v1/articles/%s/%s", doi.prefix, URLencode(doi.suffix, reserved = T))
  token <- wiley.api
  headers <- c("Wiley-TDM-Client-Token" = token)
  code.frame<-download.pdf.url(pdf.url, url, host="wiley.com", headers=headers, filename=filename, sleep=10)
  return(code.frame)
}
download.elsevier<-function(elsevier.api, url, doi.prefix, doi.suffix, filename, xml=F){
  print(paste(elsevier.api, filename))
  base_url <- "https://api.elsevier.com/content/article/doi"
  pdf.url <- sprintf("%s/%s/%s", base_url, doi.prefix, URLencode(doi.suffix, reserved = T))
  if (xml==T){
    headers<-c(`X-ELS-APIKey` = elsevier.api,
               `Accept` = "application/xml")
    format="application/xml"
    filename<-gsub("/PDF/", "/XML/", filename)
  }
  else{
    headers<-c(`X-ELS-APIKey` = elsevier.api,
             `Accept` = "application/pdf")
    format="application/pdf"
  }
  
  code.frame<-download.pdf.url(pdf.url, url, host="elsevier.com", headers=headers, 
                   filename=filename, format, sleep=1)
  
  if (code.frame$code==1){
    if (xml==T){
      return(code.frame)
    }else{
      pdf_metadata <- pdf_info(filename)
      num_pages <- pdf_metadata$pages
      if (num_pages==1){
        file.remove(filename)
        code.frame$code<- -8
        code.frame$note<-"elsevier one page"
        return(code.frame)
      }else{
        return(code.frame)
      }
    }
    
  }
  return(code.frame)
}
download.html<-function(url, host=host, filename=filename, sleep=10, type=1){
  
  if (type==1){
    webpage <- safe_read_html(url)
  }
  if (type==2){
    webpage<-rvest::read_html(url)
  }
  if (is.null(webpage)){
    code.frame<- data.table(code=-6, note="null webpage", sleep=sleep)
    return(code.frame)
  }
  pdf.url <- webpage %>%
    html_element("meta[name='citation_pdf_url']") %>%
    html_attr("content")
  if (is.na(pdf.url)){
    code.frame<- data.table(code=-3, note="no citation_pdf_url", sleep=sleep)
    return(code.frame)
  }
  
  if (!startsWith(pdf.url, "http")){
    pdf.url<-sprintf("%s%s", get_base_url(url), pdf.url)
  }
  code.frame<-download.pdf.url(pdf.url, url, host=host, headers=NULL, filename=filename, sleep=sleep)
  return(code.frame)
}


