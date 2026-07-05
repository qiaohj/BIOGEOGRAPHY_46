library(pdftools)
library(tesseract)
setwd("/path_to_your_project")

journal.name<-"NATURE"

ocr_folder<-sprintf("/path_to_your_project/Data/OCR.PDF/%s", journal.name)

if (!dir.exists(ocr_folder)){
  dir.create(ocr_folder)
}
temp.folder<-"/path_to_your_project/Data/TEMP"
pdf_folder<-sprintf("/path_to_your_project/Data/PDF/%s", journal.name)

pdfs<-list.files(pdf_folder, pattern = "\\.PDF")
pdf<-pdfs[1]
pdfs<-pdfs[sample(length(pdfs), length(pdfs))]
remove_symbols_keep_letters <- function(input_string) {
  # Use gsub to replace any character that is NOT an uppercase or lowercase letter
  # ([^A-Za-z]) with an empty string ("").
  cleaned_string <- gsub("[^A-Za-z]", "", input_string)
  return(cleaned_string)
}



for (i in c(1:length(pdfs))){
  pdf<-pdfs[i]
  pdf.path<-sprintf("%s/%s", pdf_folder, pdf)
  ocr.path<-sprintf("%s/%s", ocr_folder, pdf)
  if (grepl("news", tolower(pdf.path))){
    next()
  }
  if (!file.exists(pdf.path)){
    next()
  }
  file.size<-file.size(pdf.path)
  if (is.na(file.size)){
    next()
  }
  print(sprintf("%d/%d: %s (%d)", i, length(pdfs), pdf.path, file.size))
  if (file.size<1024){
    next()
  }
  #pdf.path<-"/path_to_your_project/Data/OCR.PDF/NATURE/118027E0.PDF"
  
  text<-pdf_text(pdf.path)
  text<-unlist(text)
  text<-paste(text, collapse = '')
  text<-gsub(" ", "", text)
  text<-remove_symbols_keep_letters(text)
  if (sum(nchar(text))>100){
    next()
  }
  skip.words<-c("sorrywedonthave", "authorcorrection", "correctionsamendments", "erratacorrigenda",
                "naturejobs", "natureaudio", "correctionsendentsc", "picturestory", "thispagehasbeenremoved",
                "thereisnoarticle", "snapshotfeatures")
  skip<-F
  for (word in skip.words){
    if (grepl(word, tolower(text))){
      skip<-T
      break
    }
  }
  if (skip==T){
    next()
  }
  
  
  
  file.rename(pdf.path, ocr.path)
  
  message("Step 1/3: Converting PDF to images...")
  output_file_template <- file.path(temp.folder, "%s.page%04d.png")
  
  xx<-basename(ocr.path)
  pages <- pdftools::pdf_info(ocr.path)$pages
  output_filenames <- sprintf(output_file_template, xx, 1:pages)
  images <- pdftools::pdf_convert(ocr.path, dpi = 300, filenames=output_filenames)
  message(paste("Successfully converted", length(images), "pages."))
  
  # 4. 循环调用命令行工具生成单页可搜索PDF
  message("Step 2/3: Performing OCR via command line...")
  single_page_pdfs <- c()
  temp_dir <- tempdir() # 使用临时目录
  
  tesseract_cmd <- "/usr/bin"
  
  
  for (i in seq_along(images)) {
    image_file <- images[i]
    
    # Tesseract命令行会自己添加.pdf后缀，所以我们提供不带扩展名的输出路径
    output_base <- file.path(temp_dir, sprintf("page_%03d", i))
    output_pdf_page <- paste0(output_base, ".pdf")
    
    # 构建命令行指令： tesseract input.png output_base -l lang pdf
    # 我们需要用 shQuote() 来处理可能包含空格的路径
    command <- sprintf('"%s/tesseract" %s %s -l %s pdf',
                       tesseract_cmd,
                       shQuote(image_file),
                       shQuote(output_base),
                       "eng")
    
    # 执行命令
    cat(sprintf("Processing page %d/%d: %s\n", i, length(images), command))
    system(command) # system()会执行这条指令
    
    # 检查文件是否生成
    if (file.exists(output_pdf_page)) {
      single_page_pdfs <- c(single_page_pdfs, output_pdf_page)
    } else {
      warning(sprintf("Failed to create PDF for page %d.", i))
    }
  }
  
  # 5. 合并所有单页PDF
  if (length(single_page_pdfs) > 0) {
    message("Step 3/3: Combining pages into a final PDF...")
    pdftools::pdf_combine(input = single_page_pdfs, output = pdf.path)
    message(paste("Success! Searchable PDF saved to:", pdf.path))
  } else {
    message("No pages were successfully processed. Final PDF not created.")
  }
  
  
  # 6. 清理临时文件
  unlink(images)
  unlink(single_page_pdfs)
  message("Temporary files cleaned up.")
  
}
