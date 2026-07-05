library(data.table)
setwd("/path_to_your_project")
journals<-readRDS("../Data/JCR/Target.Journals.rda")
all_journal_folders<-readRDS(sprintf("../Data/datatable_crossref/CrossRef_By_Journal.%d.rda", crossref.year))

all.doi<-list()
for (i in c(1:nrow(journals))){
  conf.item<-journals[i]
  print(paste(i, nrow(journals), conf.item$journal))
  article_item<-getArticles(conf.item, all_journal_folders)
  pdf_folder<-sprintf("/path_to_your_project/Data/PDF/%s", conf.item$journal)
  if (nrow(article_item)==0){
    next()
  }
  article_item$pdf<-sprintf("%s/%s.PDF", 
                            pdf_folder, URLencode(toupper(article_item$doi.suffix), reserved = T))
  article_item<-article_item[type=="journal-article"]
  article_item$pdf.exists<-file.exists(article_item$pdf)
  article_item<-article_item[pdf.exists==F]
  if (nrow(article_item)==0){
    next()
  }
  publishers<-unique(article_item$publisher)
  require<-F
  for (j in c(1:length(publishers))){
    if ((publishers[j] %in% cannot.download.journal.list) & publishers[j]!="MDPI AG"){
      require<-T
    }
  }
  if (require==T){
    all.doi[[length(all.doi)+1]]<-article_item
  }
}

all.doi<-rbindlist(all.doi)
saveRDS(all.doi, "../Data/cannot.download.20250727.rda")
all.doi.item<-unique(all.doi[, c("doi", "container_title", "url", "publisher")])




journals.list<-all.doi.item[, .(N=.N), by=c("container_title")]
setorderv(journals.list, "N", -1)
all.doi.item$journal<-factor(all.doi.item$container_title, levels=journals.list$container_title)
all.doi.item$container_title<-NULL
setorderv(all.doi.item, "journal")
saveRDS(all.doi.item, "../Data/cannot.download.20250727.item.rda")
fwrite(journals.list, "../Data/cannot.download.20250727.journals.list.csv")
fwrite(all.doi.item, "../Data/cannot.download.20250727.item.csv")
