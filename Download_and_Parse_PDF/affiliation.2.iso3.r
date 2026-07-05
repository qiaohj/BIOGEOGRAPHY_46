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


affiliations<-readRDS("../Data/BIOGEOGRAPHY/affiliations.rda")
gdp<-data.table(read.csv("../Data/BIOGEOGRAPHY/gdp.type.csv"))
gdp<-unique(gdp[, c("country_iso3", "country_name")])

i=1
j=1

result<-list()
for (i in c(1:nrow(affiliations))){
  print(i)
  item<-affiliations[i]
  find<-F
  for (j in c(1:nrow(gdp))){
    gdp.item<-gdp[j]
    if (grepl(gdp.item$country_name, item$affiliation)){
      item$country_name<-gdp.item$country_name
      item$country_iso3<-gdp.item$country_iso3
      result[[length(result)+1]]<-item
      find<-T
    }
  }
  if (find==F){
    for (j in c(1:nrow(gdp))){
      gdp.item<-gdp[j]
      if (grepl(sprintf(" %s", gdp.item$country_iso3), item$affiliation)){
        item$country_name<-gdp.item$country_name
        item$country_iso3<-gdp.item$country_iso3
        result[[length(result)+1]]<-item
        find<-T
      }
    }
  }
  if (find==F){
    item$country_name<-""
    item$country_iso3<-""
    result[[length(result)+1]]<-item
  }
}
result<-rbindlist(result)

states<-tolower(c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", 
          "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", 
          "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", 
          "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", 
          "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", 
          "New Hampshire", "New Jersey", "New Mexico", "New York", 
          "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", 
          "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", 
          "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", 
          "West Virginia", "Wisconsin", "Wyoming"))

for (i in c(1:nrow(result))){
  if (result[i]$country_name!=""){
    next()
  }
  print(i)
  for (j in c(1:length(states))){
    if (grepl(states[j], tolower(result[i]$affiliation))){
      result[i]$country_name<-"United States"
      result[i]$country_iso3<-"USA"
    }
  }
}

View(result[country_name==""])

xxx<-result[country_name=="", c("article_DOI", "affiliation")]
ggg<-fread("/path_to_your_project/Data/BIOGEOGRAPHY/gemini.reference.affiliation.csv")

xxx<-merge(xxx, ggg, by=c("article_DOI"), all.x=T)
xxx[is.na(country_iso_3), country_iso_3:=""]
xxx[country_iso_3==""]
fwrite(xxx[country_iso_3=="", c("article_DOI", "affiliation")], "~/Downloads/xxx.csv")

part1<-result[country_name!=""]
part2<-xxx[country_iso_3!=""]
part1[str_length(part1$country_iso3)!=3]
part2[str_length(part2$country_iso_3)!=3]

part2 <- part2[, .(country_iso_3 = unlist(strsplit(country_iso_3, split = "[;, ]\\s*"))), 
               by = .(article_DOI, affiliation)]
part2$country_name<-""
colnames(part2)<-c("article_DOI", "affiliation", "country_iso3", "country_name")

reference.affiliation<-rbindlist(list(part1, part2), use.names = T)

saveRDS(reference.affiliation, "../Data/BIOGEOGRAPHY/reference.affiliation.rda")

if (F){
  apis<-fread("API.KEYS/gemini.keys")
  
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
  
  
  models<-c("gemini-2.5-pro", "gemini-flash-latest", 
            "gemini-2.0-flash", "gemini-3-pro-preview",
            "gemini-3-flash-preview")
  mstr<-models[5]
  
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
  
  
  #system_instruction<-read_file("LLM.API/PROMPT/read.paper.md")
  system_instruction<-read_file("BIOGEOGRAPHY/affiliation_2_iso.md")
  affiliations<-readRDS("../Data/BIOGEOGRAPHY/affiliations.rda")
  
  BATCH_SIZE<-100
  
  for (i in seq(1, nrow(affiliations), by=BATCH_SIZE)){
    target.file<-sprintf("../Data/BIOGEOGRAPHY/References.Affiliations/gemini.part.%d.csv", i)
    if (file.exists(target.file)){
      next()
    }
    saveRDS(NULL, target.file)
    content<-affiliations[i:(i+BATCH_SIZE-1)]
    file<-sprintf("../Data/BIOGEOGRAPHY/References.Affiliations/part.%d.csv", i)
    fwrite(content, file)
    
    
    tryCatch({
      
      api.index <- sample(1:nrow(apis), 1)
      gemini.key<-apis[api.index]$gemini.api
      google_genai$configure(api_key = gemini.key)
      model <- google_genai$GenerativeModel(mstr,
                                            generation_config=gen_config,
                                            safety_settings = safety_settings,
                                            system_instruction = system_instruction)
      
      print(i)
      
      uploaded_file <- google_genai$upload_file(path = file, 
                                                display_name = file)
      message(sprintf("File uploaded successfully. Name: %s", uploaded_file$name))
      print(system.time({
        message(sprintf("2. Generating content with Gemini (%s)...", mstr))
        response <- model$generate_content(uploaded_file)
        
      }))
      
      
      rrr<-py_to_r(response$to_dict())
      
      saveRDS(rrr, target.file)
      extracted_text <- response$text
      saveRDS(extracted_text, gsub("\\.csv", "\\.rda", target.file))
      write_file(extracted_text, gsub("\\.csv", "\\.txt", target.file))
      dt<-fread(gsub("\\.csv", "\\.txt", target.file))
      
      saveRDS(dt, gsub("\\.csv", "\\.dt.rda", target.file))
    },
    error = function(e) {
      message("Error: ", e$message)
      
      if (grepl("429", e$message, ignore.case = TRUE) || grepl("exceeded your current quota", e$message, ignore.case = TRUE)){
        removed.spis<-apis[api.index]
        apis<<-apis[-api.index]
        print(sprintf("%s : %s remove api index %d from apis, %d api.keys left.", 
                      removed.spis$gmail,
                      removed.spis$gemini.api,
                      api.index, nrow(apis)))
        file.remove(target.file)
        if (nrow(apis)==0){
          stop("no api left. stop");
        }
      }
    },
    warning = function(w) {
      message("Warning: ", w$message)
    },
    finally = {
      
    })
    
  }
}