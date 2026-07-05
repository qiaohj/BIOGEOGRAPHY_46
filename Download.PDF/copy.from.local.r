library(data.table)
library(pdftools)
library(stringr)
library(stringi)
setwd("/path_to_your_project")
source("Download.PDF/getArticles.r")
if (F){
  crossref.year<-2025
  journals<-readRDS(sprintf("../Data/CrossRef_Full/%d/journals.rda", crossref.year))
  journals[, c("doi.prefix", "doi.suffix") := {
    parts <- stri_split_fixed(article_DOI, "/", n = 2)
    list(sapply(parts, `[`, 1), sapply(parts, `[`, 2))
  }]
  journals[, c("doi.key", "doi.id") := {
    parts <- stri_split_fixed(doi.suffix, "\\.", n = 2)
    list(sapply(parts, `[`, 1), sapply(parts, `[`, 2))
  }]
  
  pdfs<-readRDS("/media/huijieqiao/Almost_Broken/SCIHUB/Data/PDF/full_pdf_scihub.rda")
  
  target_journal<-"Methods in Ecology and Evolution"
  items<-journals[Title==target_journal]
  
  prefix<-unique(items$doi.prefix)
  issn<-unique(items$ISSN)
  items<-items[!is.na(ISSN)]
  issn<-unique(items$ISSN)
  issn
  head(pdfs)
  pdfs[grepl("ecy.2993", DOI2)]
}

categories<-list.files("../Data/JCR/Target.Journals/", pattern="\\.csv")
categories<-gsub("\\.csv", "", categories)
category<-"MULTIDISCIPLINARY SCIENCES.2025"
crossref.year<-2025
pdfs<-readRDS("../Data/full_pdf_scihub.rda")
pdfs$DOI2<-tolower(pdfs$DOI2)
cross_ref_folders<-readRDS(sprintf("../Data/cross_ref_folders.%d.rda", crossref.year))
cross_ref_folders[grepl("2097-2113", cross_ref_folders)]

all_journal_folders<-readRDS(sprintf("../Data/datatable_crossref/CrossRef_By_Journal.%d.rda", crossref.year))
if (F){
  jcr<-readRDS("/path_to_your_project/Data/JCR/journals.rda")
  item<-jcr[Title=="HORTSCIENCE"]
  conf.item<-data.table(journal=toupper(item$Title),
                        ISSN=item$ISSN,
                        eISSN=item$eISSN)
  
  journal.conf<-readRDS("../Data/Vegetation/vegetation.journal.rda")
}
for (category in categories){
  
  journal.conf<-readJournal(category)
  
  i=1
  for (i in c(1:nrow(journal.conf))){
    conf.item<-journal.conf[i]
    
    target_folder<-sprintf("/path_to_your_project/Data/PDF/%s", conf.item$journal)
    if (dir.exists(target_folder)){
      next()
    }
    
    dir.create(target_folder)
    article_item<-getArticles(conf.item, all_journal_folders)
    if (nrow(article_item)==0){
      next()
    }
    
    doi.suffix<-tolower(unique(article_item$doi.suffix))
    pdf_target<-pdfs[DOI2 %in% doi.suffix]
    if (nrow(pdf_target)==0){
      next()
    }
    for (j in c(1:nrow(pdf_target))){
      #print(sprintf("%d/%d, %s (%d/%d), %s", j, nrow(pdf_target), 
      #              conf.item$journal, i, nrow(journal.conf), category))
      
      item<-pdf_target[j]
      pdf.name<-toupper(basename(item$pdf)) 
      target.file<-sprintf("%s/%s", target_folder, pdf.name)
      f1<-file.size(item$pdf)
      f2<-file.size(target.file)
      if (is.na(f1)){
        f1<-0
      }
      if (is.na(f2)){
        f2<-0
      }
      if (!file.exists(target.file) | (f1!=f2)){
        print(sprintf("%d/%d, %s", j, nrow(pdf_target), 
                      conf.item$journal))
        file.copy(item$pdf, target.file, overwrite=T)
      }
    }
  }
}
