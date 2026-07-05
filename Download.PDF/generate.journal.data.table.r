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

crossref.year<-2025
all_journal_folders<-readRDS(sprintf("../Data/datatable_crossref/CrossRef_By_Journal.%d.rda", crossref.year))

i=6
journals<-readRDS("../Data/JCR/Target.Journals.rda")
for (i in c(851:nrow(journals))){
  conf.item<-journals[i]
  
  articles.file<-sprintf("/path_to_your_project/Data/LOG/%s.%d.rda", conf.item$journal, crossref.year)
  if (file.exists(articles.file)){
    #if (F){
    articles<-readRDS(articles.file)
    if (!"pdf.path" %in% colnames(articles)){
      articles<-getArticles(conf.item, all_journal_folders)
    }
  }else{
    articles<-getArticles(conf.item, all_journal_folders)
  }
  print(sprintf("%d/%d: %s (%d)", i, nrow(journals), conf.item$journal, nrow(articles)))
  if (nrow(articles)>0){
    if ("File" %in% colnames(articles)){
      articles$File<-NULL
    }
    if ("license" %in% colnames(articles)){
      articles$license<-NULL
    }
    articles$pdf<-sprintf("%s.PDF", URLencode(toupper(articles$doi.suffix), reserved = T))
    articles$pdf.path<-sprintf("/path_to_your_project/Data/PDF/%s/%s", 
                               conf.item$journal, articles$pdf)
    articles$xml<-sprintf("%s.XML", URLencode(toupper(articles$doi.suffix), reserved = T))
    articles$xml.path<-sprintf("/path_to_your_project/Data/GROBID.XML/%s/%s", 
                               conf.item$journal,articles$xml)
    articles$pdf.exist<-file.exists(articles$pdf.path)
    articles$xml.exist<-file.exists(articles$xml.path)
    articles$pdf.xml.elsevier.path<-gsub("GROBID.XML", "XML", articles$xml.path)
    articles$pdf.xml.elsevier.exist<-file.exists(articles$pdf.xml.elsevier.path)
    if (nrow(articles)>0){
      for (j in c(1:nrow(articles))){
        if (articles[j]$pdf.exist==F){
          next()
        }
        pdf_metadata <- tryCatch({
          pdf_info(articles[j]$pdf.path)
        }, error = function(e) {
          cat("Error occurred:", e$message, "\n")
          return(NULL)
        }, warning = function(w) {
          cat("Warning occurred:", w$message, "\n")
          return(NULL)
        })
        
        if (is.null(pdf_metadata)) {
          if (articles[j]$pdf.exist==T){
            file.remove(articles[j]$pdf.path)
          }
          if (articles[j]$xml.exist==T){
            file.remove(articles[j]$xml.path)
          }
          articles[j, pdf.exist:=F]
          articles[j, xml.exist:=F]
          next()
        }
        
        publisher<-articles[j]$publisher
        if (is.na(publisher)){
          publisher<-""
        }
        if (publisher=="Elsevier BV" & articles[j]$pdf.exist==T){
          
          num_pages <- pdf_metadata$pages
          if (num_pages==1){
            file.remove(articles[j]$pdf.path)
            if (articles[j]$xml.exist==T){
              file.remove(articles[j]$xml.path)
            }
            articles[j, pdf.exist:=F]
            articles[j, xml.exist:=F]
          }
        }
      }
      #articles<-articles[pdf.exist==F | xml.exist==F]
    }
    remove<-articles[pdf.exist==F & xml.exist==T]
    print(sprintf("REMOVE %d XML FILES WITHOUT PDF.", nrow(remove)))
    if (nrow(remove)>0){
      file.remove(remove$xml.path)
      articles[pdf.exist==F & xml.exist==T, xml.exist:=F]
    }
    print(articles[, .(N=.N), by=list(pdf.exist, xml.exist)])
    saveRDS(articles, articles.file)
  }
}
