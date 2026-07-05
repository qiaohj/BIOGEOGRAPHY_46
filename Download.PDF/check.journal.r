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
if (F){
  for (i in c(1:nrow(journals))){
    conf.item<-journals[i]
    check.point<-sprintf("../Data/LOG/%s.%d.rda", conf.item$journal, crossref.year)
    
    if (file.exists(check.point)){
      df<-readRDS(check.point)
      if ("note" %in% (colnames(df))){
        print(check.point)
        file.remove(check.point)
      }
    }
  }
}
args = commandArgs(trailingOnly=TRUE)
api_index<-as.numeric(args[1])
if (is.na(api_index)){
  api_index<-3
}
wiley.api<-wiley.api[api_index]
elsevier.api<-elsevier.api[api_index]

t.journal.name<-ifelse(is.na(args[2]), "NA", args[2])

force.check<-ifelse(is.na(args[3]), "F", args[3])
force.check<-ifelse(force.check=="F", F, T)

is.download<-ifelse(is.na(args[4]), "F", args[4])
is.download<-ifelse(is.download=="F", F, T)

crossref.year<-2025
all_journal_folders<-readRDS(sprintf("../Data/datatable_crossref/CrossRef_By_Journal.%d.rda", crossref.year))

i=1
journals<-readRDS("../Data/JCR/Target.Journals.rda")
#journals[journal=="VEGETATIO"]
#target.journals<-fread("../Data/CSC/target.journals_20250726.csv")
#target.journals<-target.journals[Count>500]
#target.journals<-target.journals[Publisher=="FapUNIFESP (SciELO)" & Note=="Auto"]
#journals<-journals[journal %in% target.journals$Journal_name]
journals<-journals[sample(nrow(journals), nrow(journals))]
#t.journal.name<-"VEGETATIO"
if (t.journal.name=="NA"){
  journals<-journals[sample(nrow(journals), nrow(journals))]
  
}else{
  journals<-journals[journal==t.journal.name]
  
}

i=1
if (nrow(journals)>0){
  
  for (i in c(1:nrow(journals))){
    conf.item<-journals[i]
    check.point<-sprintf("../Data/LOG/%s.%d.rda", conf.item$journal, crossref.year)
    
    if (!file.exists(check.point) | force.check==T){
      check.journal.pdf(conf.item, crossref.year)
    }
    if (is.download==T){
      download.journal.pdf(conf.item, crossref.year, wiley.api, elsevier.api)
    }
  }
}
