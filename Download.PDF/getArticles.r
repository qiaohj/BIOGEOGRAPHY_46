cannot.download.journal.list<-c("Humboldt Field Research Institute",
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
                                "Inter-Research Science Center",
                                "JSTOR",
                                #"MDPI AG",
                                "American Association for the Advancement of Science (AAAS)",
                                "Proceedings of the National Academy of Sciences",
                                "Canadian Science Publishing",
                                "Scientific Societies")
no_open_access<-c(
  "FASEB JOURNAL",
  "BIOLOGIA",
  "LANDSCAPE ECOLOGY",
  "American Chemical Society (ACS)",
  "Current Science Association",
  "JOURNAL OF SOIL SCIENCE AND PLANT NUTRITION")

html.download.journal.list.type1<-c(
  "FapUNIFESP (SciELO)", 
  "Springer Science and Business Media LLC",
  "Pleiades Publishing Ltd",
  "Copernicus GmbH",
  "Oles Honchar Dnipropetrovsk National University",
  "Norwegian Polar Institute",
  "Masaryk University Press",
  "Frontiers Media SA", "Brill")
html.download.journal.list.type2<-c("Resilience Alliance, Inc.",
                                    "Biodiversity Heritage Library"
)
if (F){
  all_journal_folders<-list.dirs(sprintf("/media/huijieqiao/NAS/Literature/CrossRef_By_Journal/%d/", crossref.year))
  saveRDS(all_journal_folders, sprintf("/media/huijieqiao/NAS/Literature/datatable_crossref/CrossRef_By_Journal.%d.rda", crossref.year))
  
}
if (F){
  categories<-list.files("/media/huijieqiao/NAS/Literature/JCR/Target.Journals/", pattern="\\.csv")
  categories<-gsub("\\.csv", "", categories)
  category<-"MULTIDISCIPLINARY SCIENCES.2025"
  
  journals<-list()
  for (category in categories){
    journal.conf<-readJournal(category)
    journals[[length(journals)+1]]<-journal.conf
  }
  journals<-rbindlist(journals, fill=T)
  journals$Category<-NULL
  journals<-journals[, c("ISSN", "eISSN", "journal")]
 
  journals[journal=="JOURNAL OF PHYCOLOGY", ISSN:="0022-3646"]
  journals[journal=="WATER BIOLOGY AND SECURITY", ISSN:="2097-4132"]
  journals[journal=="INTERNATIONAL JOURNAL OF SEDIMENT RESEARCH"]
  journals[journal=="INTERNATIONAL JOURNAL OF SEDIMENT RESEARCH", eISSN:="2589-7284"]
  
  journals[journal=="INTERNATIONAL JOURNAL OF SEDIMENT RESEARCH"]
  journals[journal=="INTERNATIONAL JOURNAL OF SEDIMENT RESEARCH", eISSN:="2589-7284"]
  
  journals[journal=="NATURAL RESOURCES FORUM"]
  journals[journal=="NATURAL RESOURCES FORUM", ISSN:="0165-0203"]
  
  journals[journal=="ACTA BOTANICA MEXICANA"]
  journals[journal=="ACTA BOTANICA MEXICANA", ISSN:="0187-7151"]
  
  journals<-unique(journals)
  journals.N<-journals[,.(N=.N), by=list(journal)]
  journals.N[N>1]
  saveRDS(journals, "/media/huijieqiao/NAS/Literature/JCR/Target.Journals.rda")
  
  all_journal_folders<-readRDS(sprintf("/media/huijieqiao/NAS/Literature/datatable_crossref/CrossRef_By_Journal.%d.rda", crossref.year))
  for (i in c(1:nrow(journals))){
    item<-journals[i]
    target<-sprintf("/media/huijieqiao/NAS/Literature/Journal.Article/%d/%s.rda", crossref.year, item$journal)
    if (file.exists(target)){
      next()
    }
    
    article<-getArticles(item, all_journal_folders)
    article$pdf<-sprintf("%s.PDF", 
            URLencode(toupper(article$doi.suffix), reserved = T))
    print(paste(item$journal, ":", nrow(article)))
    
    saveRDS(article, target)
  }
}
getArticles<-function(conf.item, all_journal_folders){
  
  conf.item$ISSN_1<-toupper(ifelse(conf.item$ISSN=="", "XXXXXXXXXXXXXXXX", conf.item$ISSN))
  conf.item$ISSN_2<-toupper(ifelse(conf.item$eISSN=="", "XXXXXXXXXXXXXXXX", conf.item$eISSN))
  
  
  folders<-all_journal_folders[grepl(conf.item$ISSN_1, toupper(all_journal_folders)) | 
                                 grepl(conf.item$ISSN_2, toupper(all_journal_folders))]
  if (length(folders)==0){
    return(data.table())
  }
  article_item<-list()
  for (f in folders){
    f<-gsub("\\|", "\\.", f)
    if (!file.exists(sprintf("%s/articles.rda", f))){
      next()
    }
    article_item[[length(article_item)+1]]<-readRDS(sprintf("%s/articles.rda", f))
  }
  
  article_item<-rbindlist(article_item)
  if (nrow(article_item)==0){
    return(article_item)
  }
  article_item$abstract<-NULL
  #article_item[doi=="10.1002/ecy.2993"]
  article_item[, c("doi.prefix", "doi.suffix") := {
    parts <- stri_split_fixed(doi, "/", n = 2)
    list(sapply(parts, `[`, 1), sapply(parts, `[`, 2))
  }]
  article_item<-unique(article_item)
  #journal-article
  article_item<-article_item[type=="journal-article"]
  article_item$pdf<-sprintf("%s.PDF", 
          URLencode(toupper(article_item$doi.suffix), reserved = T))
  article_item
}

readJournal<-function(category){
  journal.conf<-fread(sprintf("/media/huijieqiao/NAS/Literature/JCR/Target.Journals/%s.csv", category), header=T)
  journal.conf$journal<-toupper(journal.conf$`Journal name`)
  journal.conf$journal<-gsub("&", "AND", journal.conf$journal)
  journal.conf$journal<-toupper(gsub("ANDAMP;", "AND", journal.conf$journal))
  journal.conf$journal<-gsub("/", "-", journal.conf$journal)
  journal.conf
}
