#usethis::edit_r_environ()

library("rwosstarter")
library("data.table")
library("pdftools")
library("stringr")
library("stringi")
library("RCurl")
library("rvest")
library("httr")
library("pdftools")
library("scihubr")

setwd("/path_to_your_project")
source("Download.PDF/wos.functions.r")
source("Download.PDF/getArticles.r")
source("Download.PDF/download.pdf.r")
source("Download.PDF/read_html.r")
source("../tokens.r")
token.index<-3
#wiley.api<-wiley.api[token.index]
#elsevier.api<-elsevier.api[token.index]
#rwosstarter::wos_get_records()
if (F){
  target.journals<-fread("../Data/CSC/target.journals_20250730.csv")
  #target.journals<-target.journals[Journal_name=="AIP CONFERENCE PROCEEDINGS"]
  target.journals[is.na(ISSN), ISSN:=""]
  target.journals[is.na(eISSN), eISSN:=""]
  journals<-readRDS("../Data/JCR/Target.Journals.rda")
  for (i in c(1:nrow(target.journals))){
    item<-target.journals[i]
    if (item$Journal_name %in% c("PROCEEDINGS OF THE FLORIDA STATE HORTICULTURAL SOCIETY",
                                 "JOURNAL OF HORTICULTURAL SCIENCE",
                                 "ZEITSCHRIFT FUR PFLANZENKRANKHEITEN UND PFLANZENSCHUTZ JOURNAL OF PLANT DISEASES AND PROTECTION")){
      next()
    }
    
    journals.item<-journals[journal==item$Journal_name]
    if (nrow(journals.item)==0 & item$ISSN=="" & item$eISSN==""){
      next()
    }
    target.journals[i, ISSN:=journals.item$ISSN]
    target.journals[i, eISSN:=journals.item$eISSN]
    
  }
  target.journals[is.na(ISSN), ISSN:=""]
  target.journals[is.na(eISSN), eISSN:=""]
  saveRDS(target.journals, "../Data/CSC/target.journals_20250730.rda")
  fwrite(target.journals, "../Data/CSC/target.journals_20250730.csv")
}
if (F){
  limit<-NULL
  target.journals<-readRDS("../Data/CSC/target.journals_20250730.rda")
  if (F){
    target.journals[Journal_name=="PROCEEDINGS OF SPIE", eISSN:="0277-786X"]
    target.journals[Journal_name=="AIP CONFERENCE PROCEEDINGS", eISSN:="0094-243X"]
    target.journals[Journal_name=="ADVANCED MATERIALS RESEARCH", eISSN:="1022-6680"]
    target.journals[Journal_name=="JOURNAL OF HORTICULTURAL SCIENCE", ISSN:="0973-354X"]
    target.journals[Journal_name=="JOURNAL OF HORTICULTURAL SCIENCE", eISSN:="2582-4899"]
    target.journals[Journal_name=="ZEITSCHRIFT FUR PFLANZENKRANKHEITEN UND PFLANZENSCHUTZ JOURNAL OF PLANT DISEASES AND PROTECTION", ISSN:="1861-3829"]
    target.journals[Journal_name=="ZEITSCHRIFT FUR PFLANZENKRANKHEITEN UND PFLANZENSCHUTZ JOURNAL OF PLANT DISEASES AND PROTECTION", eISSN:="1861-3837"]
    
    saveRDS(target.journals, "../Data/CSC/target.journals_20250730.rda")
    fwrite(target.journals, "../Data/CSC/target.journals_20250730.csv")
  }
  if (T){
    target.journals<-target.journals[Journal_name=="ADVANCED MATERIALS RESEARCH"]
    
  }
  i=1
  for (i in c(1:nrow(target.journals))){
    item<-target.journals[i]
    
    if (item$ISSN=="" & item$eISSN==""){
      next()
    }
    if (item$ISSN=="" & item$eISSN!=""){
      issn.str<-item$eISSN
    }
    if (item$ISSN!="" & item$eISSN==""){
      issn.str<-item$ISSN
    }
    if (item$ISSN!="" & item$eISSN!=""){
      issn.str<-sprintf("(%s OR %s)", item$ISSN, item$eISSN)
    }
    query <- sprintf("TS=((seed* OR fruit* OR diaspore* OR propagule*) AND (size* OR mass* OR weight* OR germinat* OR dorman*)) AND (IS=%s)",
                     issn.str)
    database<-"WOS"
    query <- utils::URLencode(query, reserved = TRUE)
    n_refs <- wos_search(query, database)
    
    if (n_refs == 0) {
      next()
    }
    folder<-sprintf("/path_to_your_project/Data/CSC/seeds/%s", item$Journal_name)
    if (!dir.exists(folder)){
      dir.create(folder)
    }
    refs <- data.frame()
    n_records_per_page <- 50
    
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
                      page, length(pages), item$Journal_name))
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
  }
  
  i=1
  for (i in c(1:nrow(target.journals))){
    item<-target.journals[i]
    print(sprintf("%d/%d: %s", i, nrow(target.journals), item$Journal_name))
    folder<-sprintf("/path_to_your_project/Data/CSC/seeds/%s", item$Journal_name)
    contents<-list.files(folder, pattern="\\.rda")
    if (length(contents)<=0){
      next()
    }
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
      data$journal<-item$Journal_name
      all[[length(all)+1]]<-data
    }
    all.df<-rbindlist(all)
    saveRDS(all.df, sprintf("../Data/CSC/wos.journals/%s.rda", item$Journal_name))
  }
}
#get PDF and download links.
crossref.year<-2025
#WEED SCIENCE


all_journal_folders<-readRDS(sprintf("../Data/datatable_crossref/CrossRef_By_Journal.%d.rda", crossref.year))
target.journals<-readRDS("../Data/CSC/target.journals_20250730.rda")
result<-list()
#target.journals<-target.journals[Journal_name=="SEED SCIENCE AND TECHNOLOGY"]
i=1
#for (i in c(1:nrow(target.journals))){
for (i in c(1:500)){
  item<-target.journals[i]
  if (item$Note.Qiao=="[no exist in jcr categories]"){
    #next()
  }
  journal.folder<-sprintf("/path_to_your_project/Data/CSC/pdf/%s", item$Journal_name)
  if (!dir.exists(journal.folder)){
    dir.create(journal.folder)
  }
  article.file<-sprintf("../Data/CSC/wos.journals/%s.rda", item$Journal_name)
  if (!file.exists(article.file)){
    result.item<-data.table(journal=item$Journal_name, 
                            wos=0, 
                            type.matched=0,
                            with.doi=0,
                            crossref=0,
                            with.pdf=0
    )
  }else{
    articles<-readRDS(article.file)
    wos<-nrow(articles)
    articles<-articles[document_type %in% c("Article", "Data Paper", "Early Access")]
    type.matched<-nrow(articles)
    articles<-articles[!is.na(doi)]
    articles<-articles[doi!="NA"]
    with.doi<-nrow(articles)
    articles[, c("doi.prefix", "doi.suffix") := {
      parts <- stri_split_fixed(doi, "/", n = 2)
      list(sapply(parts, `[`, 1), sapply(parts, `[`, 2))
    }]
    articles$pdf<-sprintf("%s.PDF", 
                          URLencode(toupper(articles$doi.suffix), reserved = T))
    
    articles<-articles[, c("uid", "journal", "doi", "pdf", "title")]
    
    missing.file<-sprintf("/path_to_your_project/Data/CSC/missing.pdf/%s.csv", item$Journal_name)
    if (!file.exists(missing.file)){
      missing.df<-data.table(uid=articles$uid, code=999, note="")
    }else{
      missing.df<-fread(missing.file)
      if (nrow(missing.df)==0){
        missing.df<-data.table(uid=articles$uid, code=999, note="")
      }
      if ("code" %in% colnames(missing.df)){
        
      }else{
        missing.df$code<-999
        missing.df$note<-""
      }
    }
    #articles<-articles[uid %in% wos.uid]
    
    crossref.articles<-getArticles(item, all_journal_folders)
    if (nrow(crossref.articles)==0){
      result.item<-data.table(journal=item$Journal_name, 
                              wos=wos,
                              type.matched=type.matched,
                              with.doi=with.doi,
                              crossref=0,
                              with.pdf=0
      )
    }else{
      crossref.articles<-crossref.articles[, c("pdf", "url", "resource_primary_url", "publisher")]
      articles.crossref<-merge(articles, crossref.articles, by="pdf", all.x=T)
      if (nrow(articles.crossref)==0){
        result.item<-data.table(journal=item$Journal_name, 
                                wos=wos,
                                type.matched=type.matched,
                                with.doi=with.doi,
                                crossref=0,
                                with.pdf=0
        )
      }else{
        articles.crossref$pdf.path<-sprintf("%s/%s", journal.folder, articles.crossref$pdf)
        articles.crossref$with.pdf<-file.exists(articles.crossref$pdf.path)
        articles.crossref[, c("doi.prefix", "doi.suffix") := {
          parts <- stri_split_fixed(doi, "/", n = 2)
          list(sapply(parts, `[`, 1), sapply(parts, `[`, 2))
        }]
        articles.crossref<-unique(articles.crossref)
        articles.crossref<-merge(articles.crossref, missing.df[, c("uid", "code", "note")], by="uid", all.x=T)
        articles.crossref$code<-as.numeric(articles.crossref$code)
        articles.crossref$note<-as.character(articles.crossref$note)
        articles.crossref[is.na(code), code:=1]
        articles.crossref[is.na(note), note:=""]
        
        articles.crossref[is.na(code), code:=1]
        articles.crossref[is.na(note), note:=""]
        
        articles.crossref<-articles.crossref[sample(nrow(articles.crossref), nrow(articles.crossref))]
        
        for (j in c(1:nrow(articles.crossref))){
          if (articles.crossref[j]$code!=999){
            #next()
          }
          
          if (articles.crossref[j]$pdf=="PLPHYS%2FKIAA015.PDF"){
            
          }
          pdf<-sprintf("/path_to_your_project/Data/PDF/%s/%s", item$Journal_name, articles.crossref[j]$pdf)
          xml<-sprintf("/path_to_your_project/Data/XML/%s/%s", item$Journal_name, articles.crossref[j]$pdf)
          
          target<-articles.crossref[j]$pdf.path
          pdf.exist<-file.exists(pdf)
          xml.exist<-file.exists(xml)
          
          if (pdf.exist | xml.exist){
            if (!file.exists(target)){
              if (pdf.exist){
                file.copy(pdf, journal.folder)
              }
              if (xml.exist){
                file.copy(xml, journal.folder)
              }
            }
          }else{
            
            if (!is.na(articles.crossref[j]$resource_primary_url)){
              
              print(sprintf("J: %d/%d; A: %d/%d; %s @ %s, exist (%d)", 
                            i, nrow(target.journals), 
                            j, nrow(articles.crossref), 
                            item$Journal_name, articles.crossref[j]$publisher,
                            pdf.exist))
              publisher<-articles.crossref[j]$publisher
              url<-articles.crossref[j]$resource_primary_url
              doi.prefix<-articles.crossref[j]$doi.prefix
              doi.suffix<-articles.crossref[j]$doi.suffix
              journal<-item$Journal_name
              xml.download<-ifelse(publisher=="Elsevier BV", T, F)
              if (xml.download==T){
                xml.dir<-sprintf("/path_to_your_project/Data/XML/%s", item$Journal_name)
                if (!dir.exists(xml.dir)){
                  dir.create(xml.dir)
                }
              }
              code.frame<-
                tryCatch({
                  download.pdf(publisher, url, doi.prefix, doi.suffix, 
                               wiley.api[2], elsevier.api[2], 
                               pdf, journal, xml=xml.download)
                  
                  
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
              
              if (code.frame$code>0){
                print(code.frame)
                if (xml.download){
                  file.copy(xml, journal.folder)
                }else{
                  file.copy(pdf, journal.folder)
                }
                
                Sys.sleep(code.frame$sleep)
                #Sys.sleep(1)
              }
              articles.crossref[j, code:=code.frame$code]
              articles.crossref[j, note:=code.frame$note]
            }
          }
        }
      }
      articles.crossref$with.pdf<-file.exists(sprintf("%s/%s", journal.folder, articles.crossref$pdf))
      result.item<-data.table(journal=item$Journal_name, 
                              wos=wos, 
                              type.matched=type.matched,
                              with.doi=with.doi,
                              crossref=nrow(articles.crossref[!is.na(resource_primary_url)]),
                              with.pdf=nrow(articles.crossref[with.pdf==T])
      )
    }
  }
  
  result.item$differ<-result.item$with.doi-result.item$with.pdf
  print(result.item)
  
  articles.crossref<-articles.crossref[with.pdf==F]
  publishers<-unique(articles.crossref[!is.na(publisher)]$publisher)
  publishers<-paste(publishers, collapse =", ")
  result.item$publisers<-publishers
  result[[length(result)+1]]<-result.item
  setorderv(articles.crossref, "doi", 1)
  fwrite(articles.crossref, 
         missing.file)
}

result.df<-rbindlist(result)
fwrite(result.df, 
       "/path_to_your_project/Data/CSC/target.20250813.csv")
