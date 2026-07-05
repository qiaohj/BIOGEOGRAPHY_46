#usethis::edit_r_environ()
library(reticulate)
library(httr)
library(data.table)
library(xml2)
library(stringr)
library(stringi)
library(zoo)
library(pdftools)
library(readr)
library(pdftools)
library(jsonlite)

setwd("/path_to_your_project")


apis<-fread("API.KEAPI.KEYS/gemini.keys")

#Sys.setenv("http_proxy"="http://127.0.0.1:7897")
#Sys.setenv("https_proxy"="http://127.0.0.1:7897")
#Sys.setenv("all_proxy"="http://127.0.0.1:7897")

if (F){
  rep<-GET("https://google.com")
  rep
  py_run_string("import os; print(os.environ.get('http_proxy'))")
  py_run_string("import requests; print(requests.get('https://google.com').status_code)")
  
}

use_condaenv("rag.literature", required = TRUE)

google_genai <- import("google.generativeai")
asyncio <- import("asyncio")


safety_settings <- list(
  dict(category = "HARM_CATEGORY_HARASSMENT", threshold = "BLOCK_NONE"),
  dict(category = "HARM_CATEGORY_SEXUAL", threshold = "BLOCK_NONE"),
  dict(category = "HARM_CATEGORY_HATE_SPEECH", threshold = "BLOCK_NONE"),
  dict(category = "HARM_CATEGORY_DANGEROUS_CONTENT", threshold = "BLOCK_NONE")
)

gen_config <- list(
  temperature = 0.1,
  top_p = 0.95,
  max_output_tokens = 100000L
)


models<-c("gemini-2.5-pro", "gemini-flash-latest", "gemini-2.0-flash", "gemini-3-flash")
mstr<-models[2]
clean_text <- function(text) {
  if (is.null(text) || nchar(text) == 0) {
    return("")
  }
  cleaned_text <- iconv(text, from = "", to = "UTF-8", sub = " ")
  cleaned_text <- str_replace_all(cleaned_text, "[\\x00-\\x1f\\x7f-\\x9f]", "")
  #cleaned_text <- str_replace_all(cleaned_text, "\\s+", " ")
  #cleaned_text <- str_trim(cleaned_text)
  
  return(cleaned_text)
}



system_instruction<-read_file("BIOGEOGRAPHY/extract.info.prompt.md")

target.journals<-data.table(Title=c("ECOGRAPHY", "DIVERSITY AND DISTRIBUTIONS", 
                                    "GLOBAL ECOLOGY AND BIOGEOGRAPHY", "JOURNAL OF BIOGEOGRAPHY"))
i=1
for (i in c(1:nrow(target.journals))){
  item<-target.journals[i]
  folder<-sprintf("#../Data/Literature/PDF/%s", item$Title)
  target<-sprintf("../Data/BIOGEOGRAPHY/Extracted.Items/%s", item$Title)
  if (dir.exists(target)){
    #next()
  }else{
    dir.create(target)
  }
  pdfs<-list.files(folder, pattern="\\.PDF", full.name=T)
  if (length(pdfs)==0){
    next()
  }
  j=1
  pdfs<-pdfs[sample(length(pdfs), length(pdfs))]
  for (j in c(1:length(pdfs))){
    pdf_path<-pdfs[j]
    xml_path<-gsub("\\.PDF", "\\.XML", pdf_path)
    display_name<-gsub("\\.PDF", "", basename(pdf_path))
    target.file<-sprintf("%s/%s.raw.rda", target, display_name)
    json.file<-gsub(".raw.rda", ".json", gsub("LLM.Parse", "LLM.Parse.JSON", target.file))
    json.rda.file<-sprintf("%s/%s.json.rda", target, display_name)
    if (file.exists(json.file)){
      next()
    }
    if (file.exists(json.rda.file)){
      next()
    }
    
    if (!file.exists(json.rda.file) & file.exists(target.file)){
      #print(pdf_path)
      #file.remove(target.file)
      #next()
    }
    if (file.exists(target.file)){
      if (file.size(target.file)<1000){
        #print(pdf_path)
        
        
        #file.remove(target.file)
      }
      next()
    }
    #next()
    
    print(pdf_path)
    
    saveRDS(NULL, target.file)
    xx<-tryCatch({
      if (file.exists(xml_path)){
        upload.path<-NULL
        #content<-read_file(xml_path)
        xml_doc <- read_xml(xml_path)
        all_text_info <- character()
        text_nodes <- xml_find_all(xml_doc, "//text()")
        for (node in text_nodes) {
          text <- xml_text(node)
          cleaned_text <- trimws(text)
          if (nchar(cleaned_text) > 0) {
            all_text_info <- c(all_text_info, cleaned_text)
          }
        }
        all_text_info<-str_c(all_text_info, collapse = "\n")
        
        type<-"XML.Content"
      }else{
        pdfinfo<-tryCatch({pdf_info(pdf_path)},
                          
                          error = function(e) {
                            #message("Error: ", e$message)
                            return(NULL)
                          })
        if (is.null(pdfinfo)){
          file.copy(pdf_path, xml_path)
          upload.path<-NULL
          #content<-read_file(xml_path)    
          xml_doc <- read_xml(xml_path)
          all_text_info <- character()
          text_nodes <- xml_find_all(xml_doc, "//text()")
          for (node in text_nodes) {
            text <- xml_text(node)
            cleaned_text <- trimws(text)
            if (nchar(cleaned_text) > 0) {
              all_text_info <- c(all_text_info, cleaned_text)
            }
          }
          all_text_info<-str_c(all_text_info, collapse = "\n")
          
          type<-"XML.Content"
        }else{
          upload.path<-pdf_path
          type<-"PDF"
          #stop("Test")
          if (T){
            text_content <- pdf_text(pdf_path)
            all_text_info<-str_c(text_content, collapse = "\n")
            all_text_info<-clean_text(all_text_info)
            upload.path<-NULL
            type<-"PDF.TEXT"
          }
          
        }
      }
      "A"},
      error = function(e) {
        message("Error: ", e$message)
        if (grepl("Document is empty", e$message) | (grepl("Start tag expected", e$message))){
          file.remove(pdf_path)
          file.remove(xml_path)
          file.remove(target.file)
        }
        
        return("B")
      },
      warning = function(w) {
        message("Warning: ", w$message)
      },
      finally = {
        
      })
    if (xx=="B"){
      
      next()
    }
    
    
    tryCatch({
      
      api.index <- sample(1:nrow(apis), 1)
      gemini.key<-apis[api.index]$gemini.api
      google_genai$configure(api_key = gemini.key)
      model <- google_genai$GenerativeModel(mstr,
                                            generation_config=gen_config,
                                            safety_settings = safety_settings,
                                            system_instruction = system_instruction)
      
      print(sprintf("%d/%d %d/%d: api.index: %d, %s %s type: %s", i, nrow(target.journals),
                    j, length(pdfs), api.index, item$Title, display_name, type))
      if (!is.null(upload.path)){
        uploaded_file <- google_genai$upload_file(path = upload.path, 
                                                  display_name = display_name)
        message(sprintf("File uploaded successfully. Name: %s", uploaded_file$name))
        print(system.time({
          message(sprintf("2. Generating content with Gemini (%s)...", mstr))
          response <- model$generate_content(uploaded_file)
          
        }))
      }else{
        print(system.time({
          message(sprintf("2. Generating content with Gemini (%s)...", mstr))
          #write_file(all_text_info, sprintf("~/Downloads/%s.txt", display_name))
          #next()
          response <- model$generate_content(all_text_info)
        }))
      }
      
      
      rrr<-py_to_r(response$to_dict())
      
      saveRDS(rrr, target.file)
      extracted_text <- response$text
      saveRDS(extracted_text, sprintf("%s/%s.txt.rda", target, display_name))
      write_file(extracted_text, sprintf("%s/%s.json", target, display_name))
      parsed_data <- fromJSON(gsub("json", "", gsub("`", "", extracted_text)))
      #parsed_data$Introduction
      saveRDS(parsed_data, json.rda.file)
    },
    error = function(e) {
      message("Error: ", e$message)
      
      if (grepl("429", e$message, ignore.case = TRUE) || grepl("exceeded your current quota", e$message, ignore.case = TRUE)){
        apis<<-apis[-api.index]
        print(sprintf("remove api index %d from apis, %d api.keys left.", api.index, nrow(apis)))
        file.remove(target.file)
        if (nrow(apis)==0){
          stop("no api left. stop");
        }
      }
      if (grepl("he document has no pages", e$message)){
        file.remove(pdf_path)
        file.remove(xml_path)
        file.remove(target.file)
      }
    },
    warning = function(w) {
      message("Warning: ", w$message)
    },
    finally = {
      
    })
  }
}
