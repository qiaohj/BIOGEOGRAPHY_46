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
crossref.year<-2025

journals<-readRDS("../Data/CSC/target.journals_20250730.rda")
for (k in c(1:500)){
  print(k)
  journal<-journals[k]$Journal_name
  journal<-"REVISTA BRASILEIRA DE FRUTICULTURA"
  all_journal_folders<-readRDS(sprintf("../Data/datatable_crossref/CrossRef_By_Journal.%d.rda", crossref.year))
  target.journals<-readRDS("../Data/CSC/target.journals_20250730.rda")
  missing.file<-sprintf("/path_to_your_project/Data/CSC/missing.pdf/%s.csv", journal)
  if (!file.exists(missing.file)){
    next()
  }
  articles<-fread(missing.file)
  if (nrow(articles)==0){
    next()
  }
  articles[, c("doi.prefix", "doi.suffix") := {
    parts <- stri_split_fixed(doi, "/", n = 2)
    list(sapply(parts, `[`, 1), sapply(parts, `[`, 2))
  }]
  articles$pdf.path.hold<-sprintf("%s/%s/%s.PDF", 
                                  "/path_to_your_project/Data/PDF", journal, 
                                  URLencode(toupper(articles$doi.suffix), reserved = T))
  
  articles.raw<-getArticles(target.journals[Journal_name==journal], all_journal_folders)
  
  article.raw<-articles.raw[1]
  resource<-article.raw$resource_primary_url
  publisher<-article.raw$publisher
  if (is.null(publisher)){
    next()
  }
  if (is.na(publisher)){
    next()
  }
  
  templates<-list("FRONTIERS IN PLANT SCIENCE"="https://www.frontiersin.org/articles/%s/full",
                  "PLANTS-BASEL"="https://doi.org/%s",
                  "AGRONOMY-BASEL"="https://doi.org/%s",
                  "FOODS"="https://doi.org/%s",
                  "MOLECULES"="https://doi.org/%s",
                  "INTERNATIONAL JOURNAL OF MOLECULAR SCIENCES"="https://doi.org/%s",
                  "SCIENTIFIC REPORTS"="https://www.nature.com/articles/%s",
                  "NATURE"="https://www.nature.com/articles/%s",
                  "NATURE COMMUNICATIONS"="https://www.nature.com/articles/%s",
                  "NATURE PLANTS"="https://www.nature.com/articles/%s",
                  "EUPHYTICA"="https://link.springer.com/%s",
                  "BMC PLANT BIOLOGY"="https://bmcplantbiol.biomedcentral.com/articles/%s",
                  "PESQUISA AGROPECUARIA BRASILEIRA"="https://doi.org/%s",
                  "JOURNAL OF FOOD SCIENCE AND TECHNOLOGY MYSORE"="https://link.springer.com/%s",
                  "JOURNAL OF THE AMERICAN SOCIETY FOR HORTICULTURAL SCIENCE"="http://journal.ashspublications.org/lookup/doi/%s",
                  "HORTICULTURAE"="https://doi.org/%s")
  i=1
  for (i in c(1:nrow(articles))){
    item<-articles[i]
    doi<-item$doi
    template<-templates[[journal]]
    if (is.na(doi)){
      next()
    }
    if (doi!=""){
      if (publisher!="Wiley"){
        next()
      }else{
        template<-"https://doi.org/%s"
      }
      print(paste(i, nrow(articles), journal))
      
      if (is.null(template)){
        url<-""
      }else{
        url<-sprintf(template, doi)
      }
      
      if (journal %in% c("SCIENTIFIC REPORTS", "NATURE",
                         "NATURE COMMUNICATIONS", "NATURE PLANTS")){
        url<-sprintf(template, item$doi.suffix)
      }
      if (journal=="FRONTIERS IN PLANT SCIENCE"){
        url<-gsub("fple", "fpls", url)
      }
      filename<-item$pdf.path.hold
      if (file.exists(filename)){
        next()
      }
      
      #if (journal %in% c("PLANTS-BASEL", "AGRONOMY-BASEL", "FOODS", "MOLECULES",
      #                   "INTERNATIONAL JOURNAL OF MOLECULAR SCIENCES", 
      #                   "PESQUISA AGROPECUARIA BRASILEIRA")){
      if (publisher=="MDPI AG"){
        response <- GET(url)
        if (response$url==url){
          next()
        }
        url <- response$url
      }
      if (publisher=="Elsevier BV"){
        xml.download<-T
        xml.folder<-sprintf("/path_to_your_project/Data/XML/%s", item$journal)
        if (!dir.exists(xml.folder)){
          dir.create(xml.folder)
        }
        xml.path<-sprintf("%s/%s", xml.folder, item$pdf)
        if (file.exists(xml.path)){
          next()
        }
      }else{
        xml.download<-F
      }
      code.frame<-download.pdf(article.raw$publisher, resource, item$doi.prefix, item$doi.suffix, 
                               wiley.api[1], elsevier.api[1], filename, 
                               journal, xml=xml.download)
      if (code.frame$code>0){
        
        #Sys.sleep(code.frame$sleep)
      }
      Sys.sleep(code.frame$sleep)
      print(code.frame)
    }
  }
}
