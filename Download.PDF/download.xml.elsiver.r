
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

#get PDF and download links.
crossref.year<-2025
#WEED SCIENCE


all_journal_folders<-readRDS(sprintf("../Data/datatable_crossref/CrossRef_By_Journal.%d.rda", crossref.year))
target.journals<-readRDS("../Data/CSC/target.journals_20250730.rda")
result<-list()
target.journals<-target.journals[Journal_name=="INTERNATIONAL JOURNAL OF BIOLOGICAL MACROMOLECULES"]
i=1
for (i in c(1:nrow(target.journals))){
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
                            crossref=0,
                            with.pdf=0
    )
  }else{
    articles<-readRDS(article.file)
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
                              wos=nrow(articles), 
                              crossref=0,
                              with.pdf=0
      )
    }else{
      crossref.articles<-crossref.articles[, c("pdf", "url", "resource_primary_url", "publisher")]
      articles.crossref<-merge(articles, crossref.articles, by="pdf", all.x=T)
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
      articles.crossref[is.na(note), note:="done"]
      
      articles.crossref[is.na(code), code:=1]
      articles.crossref[is.na(note), note:="done"]
      
      articles.crossref<-articles.crossref[sample(nrow(articles.crossref), nrow(articles.crossref))]
      j=1
      for (j in c(1:nrow(articles.crossref))){
        if (articles.crossref[j]$code!=999){
          #next()
        }
        
        
        pdf<-sprintf("/path_to_your_project/Data/XML/%s/%s", item$Journal_name, articles.crossref[j]$pdf)
        target<-articles.crossref[j]$pdf.path
        pdf.exist<-file.exists(pdf)
        
        
        if (pdf.exist){
          if (!file.exists(target)){
            file.copy(pdf, journal.folder)
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
            
            code.frame<-
              tryCatch({
                
                filename<-pdf
                print(paste(elsevier.api[2], filename))
                base_url <- "https://api.elsevier.com/content/article/doi"
                pdf.url <- sprintf("%s/%s/%s", base_url, doi.prefix, URLencode(doi.suffix, reserved = T))
                headers<-c(`X-ELS-APIKey` = elsevier.api[2],
                           `Accept` = "application/xml")
                
                download.pdf.url(pdf.url, url, host="elsevier.com", headers=headers, 
                                             filename=filename, format="application/xml", sleep=1)
                
                
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
              file.copy(pdf, journal.folder)
              #Sys.sleep(code.frame$sleep)
              Sys.sleep(1)
            }
            articles.crossref[j, code:=code.frame$code]
            articles.crossref[j, note:=code.frame$note]
          }
        }
      }
    }
    articles.crossref$with.pdf<-file.exists(sprintf("%s/%s", journal.folder, articles.crossref$pdf))
    result.item<-data.table(journal=item$Journal_name, 
                            wos=nrow(articles), 
                            crossref=nrow(articles.crossref[!is.na(resource_primary_url)]),
                            with.pdf=nrow(articles.crossref[with.pdf==T])
    )
  }
  
  
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
result.df$differ<-result.df$wos-result.df$with.pdf
fwrite(result.df, 
       "/path_to_your_project/Data/CSC/target.20250802.csv")
