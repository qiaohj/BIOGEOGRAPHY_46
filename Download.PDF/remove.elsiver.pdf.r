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
source("Download.PDF/check.journal.pdf.r")
source("Download.PDF/download.journal.pdf.r")

journals<-readRDS("../Data/JCR/Target.Journals.rda")
#journals[journal=="CROP SCIENCE"]
#target.journals<-fread("../Data/CSC/target.journals_20250726.csv")
#target.journals<-target.journals[Count>500]
#target.journals<-target.journals[Publisher=="FapUNIFESP (SciELO)" & Note=="Auto"]
#journals<-journals[journal %in% target.journals$Journal_name]
i=1
crossref.year<-2025
all_journal_folders<-readRDS(sprintf("../Data/datatable_crossref/CrossRef_By_Journal.%d.rda", crossref.year))

#journals<-journals[journal=="FOOD CHEMISTRY"]
if (nrow(journals)>0){
  
  for (i in c(1:nrow(journals))){
    conf.item<-journals[i]
    
    articles<-getArticles(conf.item, all_journal_folders)
    articles<-articles[publisher=="Elsevier BV"]
    if (nrow(articles)>0){
      articles$pdf.path<-sprintf("%s/%s/%s", "/path_to_your_project/Data/PDF/", conf.item$journal, articles$pdf)
      articles$pdf.exists<-file.exists(articles$pdf.path)
      articles<-articles[pdf.exists==T]
      #articles$xml.path<-gsub("\\.PDF", "\\.XML", articles$pdf.path)
      #articles$xml.exists<-file.exists(articles$xml.path)
      #articles<-articles[xml.exists==T]
      
      for (j in c(1:nrow(articles))){
        xml.path<-gsub("\\.PDF", "\\.XML", articles[j]$pdf.path)
        
        if (!file.exists(articles[j]$pdf.path) & !file.exists(xml.path)){
          print(sprintf("1. %d/%d: %s. %d/%d", i, nrow(journals), conf.item$journal, j, nrow(articles)))
          download.elsevier(elsevier.api[4], articles[j]$resource_primary_url,
                            articles[j]$doi.prefix, articles[j]$doi.suffix, articles[j]$pdf.path)
          Sys.sleep(1)
          next()
        }
        if (!file.exists(articles[j]$pdf.path)){
          next()
        }
        pdf_metadata <- pdf_info(articles[j]$pdf.path)
        num_pages <- pdf_metadata$pages
        file_info <- file.info(articles[j]$pdf.path)
        last_modified_date <- file_info$mtime
        modified_year <- as.numeric(format(last_modified_date, "%Y"))
        
        if (modified_year==2025 & num_pages==1){
          print(sprintf("2. %d/%d: %s. %d/%d", i, nrow(journals), conf.item$journal, j, nrow(articles)))
          file.remove(articles[j]$pdf.path)
          download.elsevier(elsevier.api[4], articles[j]$resource_primary_url,
                            articles[j]$doi.prefix, articles[j]$doi.suffix, articles[j]$pdf.path)
          Sys.sleep(1)
        }
      }
      
    }
  }
}
