
check.journal.pdf<-function(conf.item, crossref.year){
  #conf.item<-journals[journal==journal.names[journal.index]]
  journal.name<-conf.item$journal
  print(journal.name)
  target_folder<-sprintf("/path_to_your_project/Data/GROBID.XML/%s", journal.name)
  middle_folder<-sprintf("/path_to_your_project/Data/Middle.PDF/%s", journal.name)
  if (!dir.exists(target_folder)){
    dir.create(target_folder)
  }
  
  if (!dir.exists(middle_folder)){
    dir.create(middle_folder)
  }
  grobid_url <- "http://172.16.120.92:8070/api/processFulltextDocument"
  pdf_folder<-sprintf("/path_to_your_project/Data/PDF/%s", conf.item$journal)
  article_item<-getArticles(conf.item, all_journal_folders)
  if (nrow(article_item)==0){
    return(1)
  }
  article_item$pdf<-sprintf("%s.PDF", URLencode(toupper(article_item$doi.suffix), reserved = T))
  article_item$pdf.path<-sprintf("/path_to_your_project/Data/PDF/%s/%s", 
                             conf.item$journal, article_item$pdf)
  article_item$xml<-sprintf("%s.XML", URLencode(toupper(article_item$doi.suffix), reserved = T))
  article_item$xml.path<-sprintf("/path_to_your_project/Data/GROBID.XML/%s/%s", 
                             conf.item$journal,article_item$xml)
  article_item$pdf.exist<-file.exists(article_item$pdf.path)
  article_item$xml.exist<-file.exists(article_item$xml.path)
  article_item$pdf.xml.elsevier.path<-gsub("GROBID.XML", "XML", article_item$xml.path)
  article_item$pdf.xml.elsevier.exist<-file.exists(article_item$pdf.xml.elsevier.path)
  
  #article_item<-article_item[!file.exists(article_item$xml.path)]
  #setorderv(article_item, "pdf", -1)
  article_item<-article_item[sample(nrow(article_item), nrow(article_item))]
  if (nrow(article_item)==0){
    return(1)
  }
  for (j in c(1:nrow(article_item))){
    pdf.path<-article_item[j]$pdf.path
    pdf<-article_item[j]$pdf
    
    xml.path<-article_item[j]$xml.path
    
    if (file.exists(xml.path)){
      #print("XML Exist SKIP")
      next()
    }
    if (!file.exists(pdf.path)){
      #print("NO PDF SKIP")
      next()
    }
    print(paste(j, nrow(article_item), pdf.path, i, nrow(journals), journal.name))
    
    text<-NULL
    
    text<-tryCatch({
      print("Reading PDF")
      pdf_text(pdf.path)
    },
    error = function(e) {
      message("Error: ", e$message)
      return(NULL)
    },
    warning = function(w) {
      message("Warning: ", w$message)
      
    },
    finally = {
      
    })
    if (is.null(text)){
      file.rename(pdf.path, 
                  sprintf("%s/%s", middle_folder, pdf))
      print("MOVE NULL and SMALL FILE")
      next()
    }
    text<-unlist(text)
    text<-paste(text, collapse = '')
    text<-gsub(" ", "", text)
    
    if (grepl("AcceptedArticle", text)){
      file.rename(pdf.path, 
                  sprintf("%s/%s", middle_folder, pdf))
      print("MOVE AcceptedArticle")
      next()
    }
    print("Pasting PDF")
    res <- POST(
      grobid_url,
      body = list(
        input = upload_file(pdf.path),
        segmentSentences=0,
        includeRawAffiliations=1,
        consolidatFunders=1)
    )
    
    
    if (status_code(res) == 200) {
      xml_content <- content(res, as = "text", encoding = "UTF-8")
      writeLines(xml_content, xml.path)
    } else {
      cat("Error:", status_code(res), "\n")
      if (status_code(res)==500){
        #file.rename(pdf.path, 
        #            sprintf("%s/%s", middle_folder, pdf))
        #print("MOVED")
      }
    }
  }
  error.pdf<-sprintf("../Data/LOG/%s.%d.rda", journal.name, crossref.year)
  article_item$pdf.exist<-file.exists(article_item$pdf.path)
  article_item$xml.exist<-file.exists(article_item$xml.path)
  
  saveRDS(article_item, error.pdf)
  print(sprintf("%d papers need to be downloaded", nrow(article_item[pdf.exist==F])))
}
