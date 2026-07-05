
source("Download.PDF/download.pdf.r")

download.journal.pdf<-function(conf.item, crossref.year, wiley.api, elsevier.api){
  journal.name<-conf.item$journal
  journal.folder<-sprintf("../Data/PDF/%s", journal.name)
  if (!dir.exists(journal.folder)){
    dir.create(journal.folder)
  }
  #print(journal.name)
  download.pdf.file<-sprintf("../Data/LOG/%s.%d.rda", journal.name, crossref.year)
  if (!file.exists(download.pdf.file)){
    return(0)
  }
  all_articles<-readRDS(download.pdf.file)
  if (nrow(all_articles)==0){
    return(0)
  }
  
  all_articles<-
    all_articles<-all_articles[sample(nrow(all_articles), nrow(all_articles))]
  #all_articles<-all_articles[!file.exists(pdf.path)]
  if (journal.name %in% c("CURRENT SCIENCE")){
    all_articles[, resource_primary_url:=gsub("http://", "https://", resource_primary_url)]
  }
  if (!("code" %in% colnames(all_articles))){
    all_articles$code<-999
  }
  if (!("note" %in% colnames(all_articles))){
    all_articles$note<-""
  }
  if (!("sleep" %in% colnames(all_articles))){
    all_articles$sleep<-10
  }
  j=1
  for (j in c(1:nrow(all_articles))){
    item<-all_articles[j]
    if (item$code!=999){
      next()
    }
    filename<-item$pdf.path
    if (file.exists(filename)){
      #print("Exist! Skip!")
      next()
    }
    
    print(sprintf("%d/%d, %s (%d/%d), %s, %s, %s", j, nrow(all_articles), 
                  conf.item$journal, i, nrow(journals),
                  item$publisher, item$resource_primary_url, item$pdf.path))
    publisher<-item$publisher
    if (is.na(publisher)){
      publisher<-"N/A"
    }
    url<-item$resource_primary_url
    doi.prefix<-item$doi.prefix
    doi.suffix<-item$doi.suffix
    journal<-conf.item$journal
    code.frame<-
    tryCatch({
      download.pdf(publisher, url, doi.prefix,doi.suffix, wiley.api, elsevier.api, filename, journal)
      
    },
    error = function(e) {
      message("Error: ", e$message)
      code.frame<- data.table(code=-7, note=e$message, sleep=10)
      
    },
    warning = function(w) {
      message("Warning: ", w$message)
      code.frame<- data.table(code=-17, note=w$message, sleep=10)
    },
    finally = {
      
    })
    
    print(code.frame)
    all_articles[j, code:=code.frame$code]
    all_articles[j, note:=code.frame$note]
    all_articles[j, sleep:=code.frame$sleep]
    if (code.frame$sleep>0){
      Sys.sleep(code.frame$sleep)
    }
  }
  setorderv(all_articles, "doi")
  all_articles$pdf.exist<-file.exists(all_articles$pdf.path)
  all_articles$xml.exist<-file.exists(all_articles$xml.path)
  print(all_articles[, .(N=.N), by=list(pdf.exist, xml.exist)])
  saveRDS(all_articles, download.pdf.file)
  
}
