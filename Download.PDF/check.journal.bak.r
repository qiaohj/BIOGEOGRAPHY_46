library(data.table)
library(pdftools)
library(stringr)
library(stringi)
library(RCurl)
library(rvest)
library(httr)
library(pdftools)
library(scihubr)


setwd("/path_to_your_project")
source("Download.PDF/read_html.r")
source("../tokens.r")
source("Download.PDF/getArticles.r")
args = commandArgs(trailingOnly=TRUE)
api_index<-as.numeric(args[1])
if (is.na(api_index)){
  api_index<-1
}
wiley.api<-wiley.api[api_index]
elsevier.api<-elsevier.api[api_index]

t.journal.name<-args[2]
crossref.year<-2025



all_journal_folders<-readRDS(sprintf("../Data/datatable_crossref/CrossRef_By_Journal.%d.rda", crossref.year))

i=1
journals<-readRDS("../Data/JCR/Target.Journals.rda")
target.journals<-fread("../Data/CSC/target.journals_20250725.csv")
target.journals<-target.journals[Note.Qiao=="Wiley" & Note=="Auto"]
journals<-journals[journal %in% target.journals$Journal_name]
journals<-journals[sample(nrow(journals), nrow(journals))]
#t.journal.name<-"PROCEEDINGS OF THE NATIONAL ACADEMY OF SCIENCES OF THE UNITED STATES OF AMERICA"
if (is.na(t.journal.name)){
  journals<-journals[sample(nrow(journals), nrow(journals))]
  
}else{
  journals<-journals[journal==t.journal.name]
  
}
i=1


#conf.item<-journals[journal==t.journal.name]
for (i in c(1:nrow(journals))){
  conf.item<-journals[i]
  #conf.item<-journals[journal==journal.names[journal.index]]
  journal.name<-conf.item$journal
  print(journal.name)
  target_folder<-sprintf("/path_to_your_project/Data/GROBID.XML/%s", journal.name)
  middle_folder<-sprintf("/path_to_your_project/Data/Middle.PDF/%s", journal.name)
  if (!dir.exists(target_folder)){
    dir.create(target_folder)
  }
  
  if (!dir.exists(middle_folder)){
    dir.create(middle_folder)
  }
  
  grobid_url <- "http://172.16.120.92:8070/api/processFulltextDocument"
  
  
  pdf_folder<-sprintf("/path_to_your_project/Data/PDF/%s", conf.item$journal)
  
  
  article_item<-getArticles(conf.item, all_journal_folders)
  
  if (nrow(article_item)==0){
    next()
  }
  
  article_item$pdf<-sprintf("%s/%s.PDF", 
                            pdf_folder, URLencode(toupper(article_item$doi.suffix), reserved = T))
  article_item<-article_item[type=="journal-article"]
  article_item$xml<-sprintf("%s/%s.XML", 
                            target_folder, URLencode(toupper(article_item$doi.suffix), reserved = T))
  article_item<-article_item[!file.exists(article_item$xml)]
  setorderv(article_item, "pdf", -1)
  article_item<-article_item[sample(nrow(article_item), nrow(article_item))]
  if (nrow(article_item)==0){
    next()
  }
  for (j in c(1:nrow(article_item))){
    print(paste(j, nrow(article_item), article_item[j]$pdf, i, nrow(journals), journal.name))
    
    pdf<-basename(article_item[j]$pdf)
    target<-sprintf("%s/%s", target_folder, gsub("\\.PDF", "\\.XML", pdf))
    if (file.exists(target)){
      next()
    }
    if (!file.exists(article_item[j]$pdf)){
      next()
    }
    text<-NULL
    text<-tryCatch({
      pdf_text(article_item[j]$pdf)
    },
    error = function(e) {
      message("Error: ", e$message)
      return(NULL)
    },
    warning = function(w) {
      message("Warning: ", w$message)
      
    },
    finally = {
      
    })
    if (is.null(text) & file.size(article_item[j]$pdf)<1e5){
      file.rename(article_item[j]$pdf, 
                  sprintf("%s/%s", middle_folder, pdf))
      print("MOVED")
      next()
    }
    text<-unlist(text)
    text<-paste(text, collapse = '')
    
    
    text<-gsub(" ", "", text)
    
    if (grepl("AcceptedArticle", text)){
      file.rename(article_item[j]$pdf, 
                  sprintf("%s/%s", middle_folder, pdf))
      print("MOVED")
      next()
    }
    pdf_path<-article_item[j]$pdf
    res <- POST(
      grobid_url,
      body = list(
        input = upload_file(pdf_path),
        segmentSentences=0,
        includeRawAffiliations=1,
        consolidatFunders=1)
    )
    
    
    if (status_code(res) == 200) {
      xml_content <- content(res, as = "text", encoding = "UTF-8")
      writeLines(xml_content, target)
    } else {
      cat("Error:", status_code(res), "\n")
      if (status_code(res)==500){
        #file.rename(article_item[j]$pdf, 
        #            sprintf("%s/%s", middle_folder, pdf))
        #print("MOVED")
      }
    }
    
    
  }
  #next()
  if (journal.name %in% c("TROPICAL PLANT PATHOLOGY", 
                          "FASEB JOURNAL")){
    next()
  }
  
  all_articles<-article_item
  all_articles$exist<-file.exists(all_articles$pdf)
  all_articles<-all_articles[exist==F]
  
  all_articles<-all_articles[type=="journal-article"]

  
  error.pdf<-sprintf("../Data/Error.PDF/%s.%d.rda", journal.name, crossref.year)
  error.df<-NULL
  if (file.exists(error.pdf)){
    error.df<-readRDS(error.pdf)
    if (nrow(error.df)>0){
      #all_articles<-all_articles[!doi %in% error.df$doi]
    }
  }
  j=1
  #next()
  #all_articles$doi.suffix<-
  #  gsub("j.1365-2699", "j.1466-822x", all_articles$doi.suffix)
  if (F){
    
    all_articles.bak<-all_articles
    all_articles<-all_articles[grepl("wiley", all_articles$resource_primary_url)]
    all_articles<-all_articles[grepl("-", all_articles$doi.suffix)]
    all_articles$doi.suffix<-sprintf("%s.1", all_articles$doi.suffix)
    all_articles$doi.suffix <- sub("^j\\.(.*?)\\.(\\d{4}-\\d{4})\\.(.*?)$", "j.\\2.\\1.\\3", all_articles$doi.suffix)
    
    all_articles$doi.suffix<-str_replace(all_articles$doi.suffix, 
                                         pattern = "(?<=j\\.\\d{4}-\\d{4}\\.)\\d{4}",
                                         replacement = function(year) {
                                           as.character(as.integer(year) + 1)
                                         })
    all_articles$doi.suffix<-str_replace(all_articles$doi.suffix, "\\.(0*)(\\d+)\\.x$", function(m) {
      # 提取去掉前导0的数字部分
      num <- sub("^0+", "", str_match(m, "\\.(0*)(\\d+)\\.x$")[,3])
      paste0(".", num, ".x")
    })
    
    all_articles$doi.suffix<-gsub("0030-1299", "1600-0706", all_articles$doi.suffix)
    
    all_articles<-error.df
    
  }
  all_articles<-all_articles[title!="Volume Information"]
  start<-1
  if (nrow(all_articles)==0){
    next()
  }
  all_articles<-all_articles[sample(nrow(all_articles), nrow(all_articles))]
  for (j in c(start:nrow(all_articles))){
    item<-all_articles[j]
    
    print(sprintf("%d/%d, %s (%d/%d), %s, %s, %s", j, nrow(all_articles), 
                  conf.item$journal, i, nrow(journals),
                  item$publisher, item$resource_primary_url, item$pdf))
    
    filename<-item$pdf
    if (file.exists(filename)){
      next()
    }
    publisher<-item$publisher
    if (publisher=="JSTOR"){
      next()
    }
    
    #next()
    url<-item$resource_primary_url
    doi.prefix<-item$doi.prefix
    doi.suffix<-item$doi.suffix
    
    tryCatch({
      no.service<-T
      if (publisher=="PeerJ"){
        pdf.url<-sprintf("%s.pdf",url)
        headers <- c(
          Accept = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8",
          `Accept-Encoding` = "gzip, deflate, br",
          `Accept-Language` = "en-US,en;q=0.5",
          Connection = "keep-alive",
          Host = "peerj.com",
          Referer = url,
          `Sec-Fetch-Dest` = "document",
          `Sec-Fetch-Mode` = "navigate",
          `Sec-Fetch-Site` = "same-origin",
          `Sec-Fetch-User` = "?1",
          `Upgrade-Insecure-Requests` = "1",
          `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:95.0) Gecko/20100101 Firefox/95.0"
        )
        resp <- httr::GET(pdf.url, httr::add_headers(.headers = headers), httr::write_disk(filename, overwrite = TRUE), httr::progress())
        
        content_type <- resp$headers$`content-type`
        if (httr::status_code(resp) == 200 && grepl("application/pdf", content_type, ignore.case = TRUE)) {
          message("PDF Downloaded successfully", filename)
        } else {
          stop(sprintf("Download failed: HTTP %s, Content-Type=%s", httr::status_code(resp), content_type))
        }
        unlink(filename)
        Sys.sleep(10)
        next()
      }
      if (publisher=="American Association for the Advancement of Science (AAAS)"){
        next()
       #https://www.science.org/doi/pdf/10.1126/sciadv.add2185?download=true
        pdf.url<-sprintf("https://www.science.org/doi/pdf/%s?download=true",
                         gsub("https://www.science.org/doi/", "", url))
        
        headers <- c(
          Accept = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8",
          `Accept-Encoding` = "gzip, deflate, br",
          `Accept-Language` = "en-US,en;q=0.5",
          Connection = "keep-alive",
          Host = "science.org",
          Referer = url,
          `Sec-Fetch-Dest` = "document",
          `Sec-Fetch-Mode` = "navigate",
          `Sec-Fetch-Site` = "same-origin",
          `Sec-Fetch-User` = "?1",
          `Upgrade-Insecure-Requests` = "1",
          `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:95.0) Gecko/20100101 Firefox/95.0"
        )
        resp <- httr::GET(pdf.url, httr::add_headers(.headers = headers), httr::write_disk(filename, overwrite = TRUE), httr::progress())
        
        content_type <- resp$headers$`content-type`
        if (httr::status_code(resp) == 200 && grepl("application/pdf", content_type, ignore.case = TRUE)) {
          message("PDF Downloaded successfully", filename)
        } else {
          stop(sprintf("Download failed: HTTP %s, Content-Type=%s", httr::status_code(resp), content_type))
        }
        
        Sys.sleep(10)
        next()
        
      }
      
      if (publisher=="Proceedings of the National Academy of Sciences"){
        #download_paper(item$doi, filename, open = TRUE)
        next()
        #https://www.science.org/doi/pdf/10.1126/sciadv.add2185?download=true
        pdf.url<-sprintf("https://pnas.org/doi/pdf/%s?download=true",
                         gsub("https://pnas.org/doi/", "", url))
        headers <- c(
          Accept = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8",
          `Accept-Encoding` = "gzip, deflate, br",
          `Accept-Language` = "en-US,en;q=0.5",
          Connection = "keep-alive",
          Host = "pnas.org",
          Referer = url,
          `Sec-Fetch-Dest` = "document",
          `Sec-Fetch-Mode` = "navigate",
          `Sec-Fetch-Site` = "same-origin",
          `Sec-Fetch-User` = "?1",
          `Upgrade-Insecure-Requests` = "1",
          `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:95.0) Gecko/20100101 Firefox/95.0"
        )
        resp <- httr::GET(pdf.url, httr::add_headers(.headers = headers), httr::write_disk(filename, overwrite = TRUE), httr::progress())
        
        content_type <- resp$headers$`content-type`
        if (httr::status_code(resp) == 200 && grepl("application/pdf", content_type, ignore.case = TRUE)) {
          message("PDF Downloaded successfully", filename)
        } else {
          unlink(filename)
          stop(sprintf("Download failed: HTTP %s, Content-Type=%s", httr::status_code(resp), content_type))
        }
        
        Sys.sleep(10)
        next()
      }
      if (publisher=="MDPI AG"){
        #next()
        no.service<-F
        pdf.url<-sprintf("%s/pdf", url)
        download.file(pdf.url, 
                      destfile = filename, method="wget",
                      extra='--header="User-Agent: Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"')
        Sys.sleep(10)
        next()
      }
      
      if (endsWith(tolower(url), ".pdf")){
        no.service<-F
        download.file(url, 
                      destfile = filename, method="wget",
                      extra="-U 'Mozilla/5.0 (X11; Linux x86_64; rv:30.0) Gecko/20100101 Firefox/30.0'")
        next()
      }
      
      if (grepl("nature\\.com", url)){
        no.service<-F
        pdf.url <- sprintf("%s.pdf", url)
        download.file(pdf.url, 
                      destfile = filename, method="wget",
                      extra="-U 'Mozilla/5.0 (X11; Linux x86_64; rv:30.0) Gecko/20100101 Firefox/30.0'")
        Sys.sleep(10)
        
        next()
      }
      if (publisher=="Wiley" | grepl("\\.wiley\\.", url)){
        #next()
        no.service<-F
        
        
        url <- sprintf("https://api.wiley.com/onlinelibrary/tdm/v1/articles/%s/%s", doi.prefix, URLencode(doi.suffix, reserved = T))
        token <- wiley.api
        headers <- c("Wiley-TDM-Client-Token" = token)
        
        resp <- httr::GET(url, httr::add_headers(.headers = headers), httr::write_disk(filename, overwrite = TRUE), httr::progress())
        
        content_type <- resp$headers$`content-type`
        if (httr::status_code(resp) == 200 && grepl("application/pdf", content_type, ignore.case = TRUE)) {
          message("PDF Downloaded successfully", filename)
        } else {
          unlink(filename)
          stop(sprintf("Download failed: HTTP %s, Content-Type=%s", httr::status_code(resp), content_type))
        }
        
        Sys.sleep(10)
        next()
      }
      if (publisher=="Smithsonian Institution"){
        no.service<-F
        pdf.url<-gsub("/part/", "/partpdf/", url)
        download.file(pdf.url, 
                      destfile = filename, method="wget",
                      extra="-U 'Mozilla/5.0 (X11; Linux x86_64; rv:30.0) Gecko/20100101 Firefox/30.0'")
        Sys.sleep(10)
        next()
        
      }
      if (publisher=="Elsevier BV"){
        no.service<-F
        base_url <- "https://api.elsevier.com/content/article/doi"
        request_url <- sprintf("%s/%s/%s", base_url, doi.prefix, URLencode(doi.suffix, reserved = T))
        response <- GET(
          url = request_url,
          add_headers(
            `X-ELS-APIKey` = elsevier.api,
            `Accept` = "application/pdf"
          ),
          write_disk(path = filename, overwrite = TRUE),
          timeout(90) 
        )
        content_type <- response$headers$`content-type`
        if (httr::status_code(response) == 200 && grepl("application/pdf", content_type, ignore.case = TRUE)) {
          message("PDF Downloaded successfully", filename)
        } else {
          unlink(filename)
          stop(sprintf("Download failed: HTTP %s, Content-Type=%s", httr::status_code(response), content_type))
        }
        Sys.sleep(10)
        next()
      }
      
      if (publisher=="The Royal Society"){
        next()
        #https://royalsocietypublishing.org/doi/10.1098/rspb.2020.2714
        #https://royalsocietypublishing.org/doi/pdf/10.1098/rspb.2020.2714?download=true
        
        no.service<-F
        pdf.url<-sprintf("%s?download=true", gsub("/doi/", "/doi/pdf/",  url))
        
        
        download.file(pdf.url, 
                      destfile = filename, method="wget",
                      extra="-U 'Mozilla/5.0 (X11; Linux x86_64; rv:30.0) Gecko/20100101 Firefox/30.0'")
        next()
      }
      
      
      if (grepl("\\.plos\\.", url)){
        no.service<-F
        webpage <- safe_read_html(url)
        if (is.null(webpage)){
          next()
        }
        pdf.url <- webpage %>%
          html_element("meta[name='citation_pdf_url']") %>%
          html_attr("content")
        if (is.na(pdf.url)){
          next()
        }
        
        if (!startsWith(pdf.url, "http")){
          pdf.url<-sprintf("%s%s", get_base_url(url), pdf.url)
        }
        download.file(pdf.url, 
                      destfile = filename, method="wget",
                      extra="-U 'Mozilla/5.0 (X11; Linux x86_64; rv:30.0) Gecko/20100101 Firefox/30.0'")
        Sys.sleep(10)
        next()
      }
      if (grepl("link\\.springer-ny\\.com", url)){
        next()
      }
      if (grepl("\\.biomedcentral\\.", url)){
        no.service<-F
        webpage <- safe_read_html(url)
        if (is.null(webpage)){
          next()
        }
        pdf.url <- webpage %>%
          html_element("meta[name='citation_pdf_url']") %>%
          html_attr("content")
        if (is.na(pdf.url)){
          next()
        }
        
        if (!startsWith(pdf.url, "http")){
          pdf.url<-sprintf("%s%s", get_base_url(url), pdf.url)
        }
        download.file(pdf.url, 
                      destfile = filename, method="wget",
                      extra="-U 'Mozilla/5.0 (X11; Linux x86_64; rv:30.0) Gecko/20100101 Firefox/30.0'")
        next()
      }
      
      if (publisher=="Universidad Nacional Autonoma de Mexico"){
        next()
        no.service<-F
        webpage <- safe_read_html(url)
        
        pdf.url <- webpage %>%
          html_nodes("a.obj_galley_link.PDF") %>%
          html_attr("href")
        
        download.file(pdf.url, 
                      destfile = filename, method="wget",
                      extra="-U 'Mozilla/5.0 (X11; Linux x86_64; rv:30.0) Gecko/20100101 Firefox/30.0'")
        next()
      }
      
      
      if (publisher %in% c(
        "FapUNIFESP (SciELO)", 
        "Springer Science and Business Media LLC",
        "Resilience Alliance, Inc.",
        "Biodiversity Heritage Library",
        
        "Pleiades Publishing Ltd",
        "Copernicus GmbH",
        "Oles Honchar Dnipropetrovsk National University",
        "Norwegian Polar Institute",
        "Masaryk University Press",
        "Frontiers Media SA")){
        
        no.service<-F
        webpage <- safe_read_html(url)
        if (is.null(webpage)){
          next()
        }
        pdf.url <- webpage %>%
          html_element("meta[name='citation_pdf_url']") %>%
          html_attr("content")
        if (!startsWith(pdf.url, "http")){
          pdf.url<-sprintf("%s%s", get_base_url(url), pdf.url)
        }
        download.file(pdf.url, 
                      destfile = filename, method="wget",
                      extra="-U 'Mozilla/5.0 (X11; Linux x86_64; rv:30.0) Gecko/20100101 Firefox/30.0'")
        Sys.sleep(10)
        next()
      }
      if (publisher %in% c("ALOKI Ltd")){
        no.service<-F
        download.file(url, 
                      destfile = filename, method="wget",
                      extra="-U 'Mozilla/5.0 (X11; Linux x86_64; rv:30.0) Gecko/20100101 Firefox/30.0'")
        next()
      }
      if (grepl("bioone", url)){
        next()
      }
      if (F){
        if (publisher=="Penerbit Universiti Sains Malaysia"){
          if (!grepl("pdf",url)){
            url<-sprintf("%s/pdf", url)
          }
          download.file(url, 
                        destfile = filename, method="wget",
                        extra="-U 'Mozilla/5.0 (X11; Linux x86_64; rv:30.0) Gecko/20100101 Firefox/30.0'")
          next()
        }
      }
      if (publisher %in% c("Humboldt Field Research Institute",
                           "Pensoft Publishers",
                           "CSIRO Publishing",
                           "Regional Euro-Asian Biological Invasions Centre Oy (REABIC)",
                           "Universidad Nacional Autonoma de Mexico",
                           "SciELO Agencia Nacional de Investigacion y Desarrollo (ANID)",
                           "Osterreichische Akademie der Wissenschaften",
                           "Informa UK Limited",
                           "Southwestern Association of Naturalists",
                           "Cambridge University Press (CUP)",
                           "JSTOR",
                           "O-Kratkoe Ltd",
                           "The Royal Society",
                           "Universidad Nacional Agraria la Molina",
                           "University of Wisconsin Press",
                           "Oxford University Press (OUP)",
                           "U.S. Fish and Wildlife Service",
                           "Schweizerbart",
                           "Universidad de Costa Rica",
                           "Penerbit Universiti Sains Malaysia",
                           "Inter-Research Science Center")){
        no.service<-F
        next()
      }
      if (no.service){
        #asdf
      }
      
    },
    error = function(e) {
      message("Error: ", e$message)
      return(NA)
    },
    warning = function(w) {
      message("Warning: ", w$message)
      
    },
    finally = {
      
    })
    #downloadPDF(item$resource_primary_url, item$doi.prefix, item$doi.suffix, item$publisher, filename)
  }
  
  
  if (!is.null(error.df)){
    error.df<-unique(rbindlist(list(error.df, all_articles)))
  }else{
    error.df<-all_articles
  }
  saveRDS(error.df, error.pdf)
}
