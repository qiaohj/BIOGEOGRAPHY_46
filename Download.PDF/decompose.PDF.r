library(httr)
#conda create -n grobid38 python=3.8
#conda activate grobid38
#export LD_LIBRARY_PATH=~/miniconda3/envs/grobid38/lib:$LD_LIBRARY_PATH
#cd /home/huijieqiao/GIT/grobid
#pip3 install -r requirements.txt
#pip install jep tensorflow
#./gradlew run

setwd("/path_to_your_project/Script")
journal<-"ECOGRAPHY"
target_folder<-sprintf("../Data/GROBID.XML/%s", journal)
if (!dir.exists(target_folder)){
  dir.create(target_folder)
}
grobid_url <- "http://172.16.120.92:8070/api/processFulltextDocument"
pdfs_path <- sprintf("/path_to_your_project/Data/PDF/%s", journal)
pdfs<-list.files(pdfs_path)

for (pdf in pdfs){
  print(pdf)
  target<-sprintf("%s/%s", target_folder, gsub("\\.pdf", "\\.xml", pdf))
  if (file.exists(target)){
    next()
  }
  pdf_path<-sprintf("%s/%s", pdfs_path, pdf)
  res <- POST(
    grobid_url,
    body = list(
      input = upload_file(pdf_path),
      segmentSentences=0,
      includeRawAffiliations=1,
      consolidatFunders=1)
  )
  
  
  if (status_code(res) == 200) {
    xml_content <- content(res, as = "text", encoding = "UTF-8")
    writeLines(xml_content, target)
  } else {
    cat("Error:", status_code(res), "\n")
  }
  
}



