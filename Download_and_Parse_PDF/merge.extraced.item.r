library(data.table)
library(stringr)
library(stringi)
library(zoo)
library(readr)
library(pdftools)
library(jsonlite)
library(ggplot2)
library(RColorBrewer)
library(ggrepel)
library(scales)
library(colorspace)
library(httr)
library(xml2)

setwd("/path_to_your_project")
source("Download.PDF/getArticles.r")
source("RAG.LLM/xml2csv.r")
crossref.year<-2025
if (F){
  all_journal_folders<-gsub("../Data", "/media/huijieqiao/NAS_Share/Literature", all_journal_folders)
  
  all_journal_folders<-gsub("//", "/", all_journal_folders)
  
  all_journal_folders<-gsub("/media/huijieqiao/NAS/Literature", "/media/huijieqiao/NAS_Share/Literature", all_journal_folders)
  saveRDS(all_journal_folders, sprintf("#../Data/Literature/datatable_crossref/CrossRef_By_Journal.%d.rda", crossref.year))
}
all_journal_folders<-readRDS(sprintf("#../Data/Literature/datatable_crossref/CrossRef_By_Journal.%d.rda", crossref.year))
journals<-readRDS("#../Data/Literature/JCR/Target.Journals.rda")



target.journals<-data.table(Title=c("JOURNAL OF BIOGEOGRAPHY", 
                                    "DIVERSITY AND DISTRIBUTIONS", 
                                    "ECOGRAPHY", 
                                    "GLOBAL ECOLOGY AND BIOGEOGRAPHY"))
i=1
authors.list<-list()
geographical_scope.list<-list()
data_sources.list<-list()
keywords.list<-list()
articles.list<-list()
grobid_url <- "http://172.16.120.92:8070/api/processFulltextDocument"
#target.journals<-target.journals[sample(nrow(target.journals), nrow(target.journals))]
missing.pdfs<-c()
for (i in c(1:nrow(target.journals))){
  item<-target.journals[i]
  folder<-sprintf("/path_to_your_project/Data/BIOGEOGRAPHY/Extracted.Items/%s", item$Title)
  conf.item<-journals[journal==item$Title]
  jsons<-list.files(folder, pattern="\\.json.rda", full.name=T)
  
  articles<-getArticles(conf.item, all_journal_folders)
  articles<-articles[, c("issue", "volume", "page", "title", "published", "doi.prefix", "doi.suffix", "doi")]
  
  articles.wos<-readRDS(sprintf("../Data/BIOGEOGRAPHY/WOS/%s.rda", item$Title))
  articles.wos$page<-articles.wos$pages
  articles.wos$published<-as.Date(sprintf("%s-1-1", articles.wos$published_year))
  articles.wos<-articles.wos[published_year=="2025"]
  articles.wos<-articles.wos[, c("issue", "volume", "page", "title", "published", "doi.prefix", "doi.suffix", "doi")]
  articles.wos<-articles.wos[!doi.suffix %in% articles$doi.suffix]
  articles<-rbindlist(list(articles, articles.wos))
  articles$doi.raw<-articles$doi
  articles$doi<-toupper(articles$doi.suffix)
  articles$journal<-item$Title
  articles[, abstract:=""]
  articles<-articles[title!="Issue Information"]
  articles<-articles[title!="Author Index"]
  articles<-articles[title!="An Attractive and Accessible Text"]
  articles<-articles[title!="Book Reviews"]
  articles<-articles[title!="Book reviews"]
  articles<-articles[title!="Cover Image"]
  articles<-articles[title!="Front Cover"]
  articles<-articles[title!="Cover page"]
  articles<-articles[title!="Title Page"]
  articles<-articles[title!="Conclusions"]
  articles<-articles[toupper(title)!="ERRATUM"]
  articles<-articles[title!="List of Referees"]
  articles<-articles[title!="List of Reviewers"]
  articles<-articles[title!="List of Editors"]
  articles<-articles[!grepl("LIST OF REFEREES", toupper(title))]
  articles<-articles[!grepl("LIST OF REVIEWERS", toupper(title))]
  articles<-articles[!grepl("LIST OF EDITORS", toupper(title))]
  
  articles<-articles[!grepl("BOOK REVIEWS", toupper(title))]
  
  articles<-articles[title!="Contents"]
  
  
  articles<-articles[!grepl("Contents of", title)]
  articles<-articles[!grepl("goes double blind", title)]
  
  articles<-articles[!grepl("Title Page", title)]
  articles<-articles[!grepl("CORRIGENDUM", toupper(title))]
  articles<-articles[!grepl("DIVERSITY AND DISTRIBUTIONS", toupper(title))]
  articles<-articles[!grepl("FRONT COVER", toupper(title))]
  articles<-articles[!grepl("ECOGRAPHY", toupper(title))]
  articles<-articles[!grepl("GLOBAL ECOLOGY AND BIOGEOGRAPHY", toupper(title))]
  articles<-articles[!grepl("JOURNAL OF BIOGEOGRAPHY", toupper(title))]
  articles<-articles[!grepl("REVIEWER AND ASSOCIATE EDITOR AWARDS", toupper(title))]
  articles<-articles[toupper(title)!="INTRODUCTION"]
  
  articles<-articles[!grepl("EDITORIAL", toupper(title))]
  articles<-articles[!grepl("Correction to", title)]
  
  
  #jsons<-jsons[sample(length(jsons), length(jsons))]
  
  for (j in c(1:length(jsons))){
    print(paste(i, 4, j, length(jsons), item$Title))
    json.f<-jsons[j]
    doi.item<-basename(json.f)
    doi.item<-gsub("\\.json\\.rda", "", doi.item)
    article.item<-articles[doi==doi.item]
    if (nrow(article.item)==0){
      next()
    }
    json<-readRDS(json.f)
    xml.path<-sprintf("../Data/BIOGEOGRAPHY/GROBID.XML/%s.xml", doi.item)
    llm.json.path<-sprintf("#../Data/Literature/LLM.Parse/%s/%s.json.rda",
                           item$Title, doi.item)
    pdf.path<-sprintf("#../Data/Literature/PDF/%s/%s.PDF",
                      article.item$journal, 
                      article.item$doi)
    
    if (!file.exists(xml.path) & !file.exists(llm.json.path)){
      missing.pdfs<-c(missing.pdfs, pdf.path)
    }else{
      if (file.exists(xml.path)){
        csv<-xml2csv(xml.path)  
        abstract.str<-csv[active_section=="Abstract"]$text
      }
      if (abstract.str==""){
        if (!file.exists(llm.json.path)){
          missing.pdfs<-c(missing.pdfs, pdf.path)
          next()
        }
        llm.json<-readRDS(llm.json.path)
        abstract.str<-llm.json$Abstract
        if (is.null(abstract.str)){
          abstract.str<-""
        }
      }
    }
    
    if (abstract.str==""){
      # print(article.item$title)
      
      text.full<-pdf_text(pdf.path[1])
      if (grepl("CORRIGENDUM", toupper(str_c(text.full, collapse = "\n")))){
        abstract.str<-"CORRIGENDUM"
      }
      if (grepl("BOOK REVIEW", toupper(str_c(text.full, collapse = "\n")))){
        abstract.str<-"BOOK REVIEW"
      }
      if (grepl("GUEST REVIEW ARTICLE", toupper(str_c(text.full, collapse = "\n")))){
        abstract.str<-"GUEST REVIEW ARTICLE"
      }
      if (grepl(toupper("Addendum"), 
                toupper(str_c(text.full, collapse = "\n")))){
        abstract.str<-toupper("Addendum")
      }
      if (grepl(toupper("Erratum"), 
                toupper(str_c(text.full, collapse = "\n")))){
        abstract.str<-toupper("Erratum")
      }
      if (grepl(toupper("Errata"), 
                toupper(str_c(text.full, collapse = "\n")))){
        abstract.str<-toupper("Errata")
      }
      
      if (grepl(toupper("Review by"), 
                toupper(str_c(text.full, collapse = "\n")))){
        abstract.str<-toupper("Book Review")
      }
      
      if (grepl(toupper("Response to"), 
                toupper(str_c(text.full, collapse = "\n")))){
        abstract.str<-toupper("Response")
      }
      if (grepl(toupper("Commentary"), 
                toupper(str_c(text.full, collapse = "\n")))){
        abstract.str<-toupper("Commentary")
      }
      if (grepl(toupper("Short notice"), 
                toupper(str_c(text.full, collapse = "\n")))){
        abstract.str<-toupper("Short notice")
      }
      if (grepl(toupper("Correspondence"), 
                toupper(str_c(text.full, collapse = "\n")))){
        abstract.str<-toupper("Correspondence")
      }
      
      if (doi.item %in% c("2845491", "J.1365-2699.1996.TB00034.X")){
        abstract.str<-"OTHERS"
      }
      
    } 
    if (abstract.str==""){
      print(pdf.path)
    }
    
    articles[doi==doi.item, abstract:=abstract.str]
    
    if (length(json$authors)>0){
      authors<-json$authors
      authors$doi<-doi.item
      authors$journal<-item$Title
      authors.list[[length(authors.list)+1]]<-authors
    }
    if (length(json$geographical_scope)>0){
      geographical_scope<-json$geographical_scope
      geographical_scope$doi<-doi.item
      geographical_scope$journal<-item$Title
      geographical_scope.list[[length(geographical_scope.list)+1]]<-geographical_scope
    }
    if (length(json$data_sources)>0){
      data_sources<-json$data_sources
      data_sources$doi<-doi.item
      data_sources$journal<-item$Title
      data_sources.list[[length(data_sources.list)+1]]<-data_sources
    }
    if (length(json$keywords)>0){
      keywords<-data.table(keyword=json$keywords, doi=doi.item, journal=item$Title)
      keywords.list[[length(keywords.list)+1]]<-keywords
    }
  }
  articles.list[[length(articles.list)+1]]<-articles
}
saveRDS(missing.pdfs, "../Data/BIOGEOGRAPHY/missing.abstract.pdf.rda")
# print("DONE!")
articles.df<-rbindlist(articles.list)

for (i in c(1:length(missing.pdfs))){
  ppp<-missing.pdfs[i]
  print(paste(i, length(missing.pdfs), ppp))
  if (ppp %in% c("#../Data/Literature/PDF/JOURNAL OF BIOGEOGRAPHY/JBI.12909.PDF")){
    abs.str<-""
  }else{
    json.path<-gsub("/PDF/", "/LLM.Parse/", ppp)
    json.path<-gsub("\\.PDF", "\\.json\\.rda", json.path)
    if (!file.exists(json.path)){
      json.path.json<-gsub("\\.rda", "", json.path)
      json<-fromJSON(gsub("json", "", gsub("`", "", read_file(json.path.json))))
      saveRDS(json, json.path)
    }
    json<-readRDS(json.path)
    if (is.null(json)){
      asdf
    }
    if (is.null(json$Abstract)){
      abs.str<-""
    }else{
      abs.str<-json$Abstract
    }
  }
  doi.pdf<-gsub("\\.PDF", "", basename(ppp))
  articles.df[doi==doi.pdf, abstract:=abs.str]
}
View(articles.df[abstract==""])
#articles.df<-articles.df[abstract!=""]
if (F){
  xxx<-list()
  for (i in c(1:nrow(target.journals))){
    item<-target.journals[i]
    folder<-sprintf("/path_to_your_project/Data/BIOGEOGRAPHY/Extracted.Items/%s", item$Title)
    conf.item<-journals[journal==item$Title]
    jsons<-list.files(folder, pattern="\\.json.rda", full.name=T)
    
    articles<-getArticles(conf.item, all_journal_folders)
    articles<-articles[, c("issue", "volume", "page", "title", "published", "doi.suffix")]
    
    articles.wos<-readRDS(sprintf("../Data/BIOGEOGRAPHY/WOS/%s.rda", item$Title))
    articles.wos$page<-articles.wos$pages
    articles.wos$published<-as.Date(sprintf("%s-1-1", articles.wos$published_year))
    articles.wos<-articles.wos[published_year=="2025"]
    articles.wos<-articles.wos[, c("issue", "volume", "page", "title", "published", "doi.suffix")]
    articles.wos<-articles.wos[!doi.suffix %in% articles$doi.suffix]
    articles<-rbindlist(list(articles, articles.wos))
    xxx[[i]]<-articles
  }
  xxx.df<-rbindlist(xxx)
  xxx.df<-unique(xxx.df[, c("published", "doi.suffix")])
  articles.df$published<-NULL
  articles.fixed<-merge(articles.df, xxx.df, by="doi.suffix")
  xxxxxx<-articles.fixed[, .(N=.N), by=c("doi.suffix")]
  xxxxxx[N>1]
  articles.fixed[doi.suffix=="j.1365-2699.1999.00298.x"]
  articles.fixed<-articles.fixed[!doi.suffix %in% xxxxxx[N>1]$doi.suffix]
  articles.df<-unique(articles.fixed)
}
authors.df<-rbindlist(authors.list)

geographical_scope.df<-rbindlist(geographical_scope.list)
data_sources.df<-rbindlist(data_sources.list)
keywords.df<-rbindlist(keywords.list)

dois<-unique(authors.df$doi)
authors.df[doi==dois[sample(length(dois), 1)]]

library(zoo)
#https://data.worldbank.org/about/country-and-lending-groups
MSTI<-readRDS("../Data/BIOGEOGRAPHY/MSTI.rda")

articles.df$year <- as.numeric(format(as.Date(articles.df$published, format = "%Y-%m-%d"), "%Y"))
table(articles.df$year)
authors.df<-rbindlist(authors.list)
authors.df.full<-merge(authors.df, articles.df, by=c("doi", "journal"))
authors.df.full$year <- as.numeric(format(as.Date(authors.df.full$published, format = "%Y-%m-%d"), "%Y"))
table(authors.df.full$year)
authors.df.full[doi=="JBI.70001"]
authors.df.full<-authors.df.full[title!="Issue Information"]
authors.df.full<-authors.df.full[title!="Author Index"]
authors.df.full<-authors.df.full[title!="An Attractive and Accessible Text"]
authors.df.full<-authors.df.full[title!="Book Reviews"]
authors.df.full<-authors.df.full[title!="Book reviews"]
authors.df.full<-authors.df.full[title!="Cover Image"]
authors.df.full<-authors.df.full[title!="Front Cover"]
authors.df.full<-authors.df.full[title!="Cover page"]
authors.df.full<-authors.df.full[title!="Conclusions"]
authors.df.full<-authors.df.full[toupper(title)!="ERRATUM"]
authors.df.full<-authors.df.full[title!="List of Referees"]
authors.df.full<-authors.df.full[!grepl("LIST OF REFEREES", toupper(title))]
authors.df.full<-authors.df.full[!grepl("BOOK REVIEWS", toupper(title))]
authors.df.full<-authors.df.full[title!="Contents"]
authors.df.full<-authors.df.full[!grepl("Contents of", title)]
authors.df.full<-authors.df.full[!grepl("CORRIGENDUM", toupper(title))]
authors.df.full<-authors.df.full[!grepl("EDITORIAL", toupper(title))]
authors.df.full<-authors.df.full[!grepl("Correction to", title)]
authors.df.full[country_iso3=="ARG"]
authors.df.full.MSTI[country_iso3=="ARG"]
xxx<-authors.df.full[!country_iso3 %in% MSTI$SOC]
table(xxx$journal)
View(xxx)

colnames(MSTI)[1:2]<-c("country_iso3", "year")
MSTI[country_iso3=="ARG"]
authors.df.full.MSTI<-merge(authors.df.full, MSTI, by=c("country_iso3", "year"), all.x=T)

saveRDS(authors.df.full.MSTI, "../Data/BIOGEOGRAPHY/authors_MSTI.rda")

geographical_scope.df<-rbindlist(geographical_scope.list)
geographical_scope.df.full<-merge(geographical_scope.df, articles.df, by="doi")
geographical_scope.df.full$year <- as.numeric(format(as.Date(geographical_scope.df.full$published, format = "%Y-%m-%d"), "%Y"))
table(geographical_scope.df.full$year)

data_sources.df<-rbindlist(data_sources.list)
data_sources.df.full<-merge(data_sources.df, articles.df, by="doi")
data_sources.df.full$year <- as.numeric(format(as.Date(data_sources.df.full$published, format = "%Y-%m-%d"), "%Y"))
table(data_sources.df.full$year)
keywords.df<-rbindlist(keywords.list)
keywords.df.full<-merge(keywords.df, articles.df, by="doi")
keywords.df.full$year <- as.numeric(format(as.Date(keywords.df.full$published, format = "%Y-%m-%d"), "%Y"))
table(keywords.df.full$year)

saveRDS(geographical_scope.df.full, "../Data/BIOGEOGRAPHY/geographical_scope.rda")
saveRDS(data_sources.df.full, "../Data/BIOGEOGRAPHY/data_sources.rda")
saveRDS(keywords.df.full, "../Data/BIOGEOGRAPHY/keywords.rda")
saveRDS(articles.df, "../Data/BIOGEOGRAPHY/articles.rda")

articles.df[,.(N=.N), by=list(journal)]



#Country by year
N.type<-authors.df.full.gpd[between(year, 2010, 2024) & (is_first_author==T | is_corresponding_author==T), 
                            .(N.articles=length(unique(doi)),
                              N.yeaer=length(unique(year))), 
                            by=c("gdp.type")]


ggplot(N.country)+geom_point(aes(x=type, y=N.articles))

N.authors.article<-authors.df.full.gpd[, .(N.yearly=length(unique(doi))), 
                                       by=list(journal, year)]

ggplot(N.authors.article)+geom_line(aes(x=year, y=N, color=journal))+
  geom_vline(xintercept = 2019)+
  geom_vline(xintercept = 2023)

all_gdp_types <- unique(authors.df.full$type)


#First author GDP
first.author<-authors.df.full.gpd[is_first_author==T]
first.author.N<-first.author[,.(N=.N), by=list(year, journal, gdp.type)]
first.author.N<-merge(first.author.N, N.authors.article, by=c("journal", "year"))
first.author.N$t1<-"First author"
first.author.N$t2<-"ALL countries"
p1<-ggplot(first.author.N[year>=2010])+
  geom_line(aes(x=year, y=N/N.yearly, color=journal, linetype = gdp.type))+
  scale_y_sqrt()+
  geom_vline(xintercept = 2019)+
  geom_vline(xintercept = 2023)
p1
#First author no China GDP
first.author.no.china<-authors.df.full.gpd[is_first_author==T & country_iso3!="CHN"]
first.author.no.china.N<-first.author.no.china[,.(N=.N), by=list(year, journal, gdp.type)]
first.author.no.china.N<-merge(first.author.no.china.N, N.authors.article, by=c("journal", "year"))
first.author.no.china.N$t1<-"First author"
first.author.no.china.N$t2<-"Without China"
p2<-ggplot(first.author.no.china.N[year>=2010])+
  geom_line(aes(x=year, y=N/N.yearly, color=journal, linetype = gdp.type))+
  scale_y_sqrt()+
  geom_vline(xintercept = 2019)+
  geom_vline(xintercept = 2023)
p2

#corresponding_author GDP
corresponding_author<-authors.df.full.gpd[is_corresponding_author==T]
corresponding_author.N<-corresponding_author[,.(N=.N), by=list(year, journal, gdp.type)]
corresponding_author.N<-merge(corresponding_author.N, N.authors.article, by=c("journal", "year"))
corresponding_author.N$t1<-"Corresponding author"
corresponding_author.N$t2<-"ALL countries"
p3<-ggplot(corresponding_author.N[year>=2010])+
  geom_line(aes(x=year, y=N/N.yearly, color=journal, linetype = gdp.type))+
  scale_y_sqrt()+
  geom_vline(xintercept = 2019)+
  geom_vline(xintercept = 2023)
p3
#corresponding_author GDP
corresponding_author.no.china<-authors.df.full.gpd[is_corresponding_author==T & country_iso3!="CHN"]
corresponding_author.no.china.N<-corresponding_author.no.china[,.(N=.N), by=list(year, journal, gdp.type)]
corresponding_author.no.china.N<-merge(corresponding_author.no.china.N, N.authors.article, by=c("journal", "year"))
corresponding_author.no.china.N$t1<-"Corresponding author"
corresponding_author.no.china.N$t2<-"Without China"
p4<-ggplot(corresponding_author.no.china.N[year>=2010])+
  geom_line(aes(x=year, y=N/N.yearly, color=journal, linetype = gdp.type))+
  scale_y_sqrt()+
  geom_vline(xintercept = 2019)+
  geom_vline(xintercept = 2023)
p4
df.full<-rbindlist(list(first.author.N, first.author.no.china.N, 
                        corresponding_author.N, 
                        corresponding_author.no.china.N))
ggplot(df.full[year>=2010])+
  geom_line(aes(x=year, y=N/N.yearly, color=journal, linetype = gdp.type))+
  scale_y_sqrt()+
  geom_vline(xintercept = 2019)+
  geom_vline(xintercept = 2023)+
  facet_grid(t1~t2)+
  theme_bw()


dt_ebdi_results <- authors.df.full.gpd[, {
  N_Total <- .N
  ebdi_score <- NA_real_
  mean_gdp_score <- NA_real_
  if (N_Total > 0) {
    numeric_gdp_types <- gdp_type_numeric_mapping[type]
    mean_gdp_score <- mean(numeric_gdp_types, na.rm = TRUE)
    if (N_Total == 1) {
      ebdi_score <- 0
    } else {
      gdp_counts <- table(type)
      full_gdp_counts <- setNames(rep(0, length(all_gdp_types)), all_gdp_types)
      full_gdp_counts[names(gdp_counts)] <- gdp_counts
      proportions <- full_gdp_counts / N_Total
      sum_sq_proportions <- sum(proportions^2)
      ebdi_score <- 1 - sum_sq_proportions
    }
  }
  
  list(journal = unique(journal),
       year = unique(year),
       ebdi_score = ebdi_score,
       mean_gdp_score = mean_gdp_score,
       N_Total=N_Total)
}, by = "doi"]

dt_ebdi_results_co_author <- authors.df.full.gpd[is_corresponding_author==T, {
  N_Total <- .N
  ebdi_score <- NA_real_
  mean_gdp_score <- NA_real_
  if (N_Total > 0) {
    numeric_gdp_types <- gdp_type_numeric_mapping[type]
    mean_gdp_score <- mean(numeric_gdp_types, na.rm = TRUE)
    if (N_Total == 1) {
      ebdi_score <- 0
    } else {
      gdp_counts <- table(type)
      full_gdp_counts <- setNames(rep(0, length(all_gdp_types)), all_gdp_types)
      full_gdp_counts[names(gdp_counts)] <- gdp_counts
      proportions <- full_gdp_counts / N_Total
      sum_sq_proportions <- sum(proportions^2)
      ebdi_score <- 1 - sum_sq_proportions
    }
  }
  
  list(journal = unique(journal),
       year = unique(year),
       ebdi_score = ebdi_score,
       mean_gdp_score = mean_gdp_score,
       N_Total=N_Total)
}, by = "doi"]

ggplot(dt_ebdi_results[ebdi_score>0], 
       aes(x=year, y=ebdi_score, size=mean_gdp_score, color=journal))+
  #geom_point()+
  geom_vline(xintercept = 2019)+
  geom_vline(xintercept = 2023)+
  geom_smooth()

ggplot(dt_ebdi_results[ebdi_score>0 & year>2010], 
       aes(x=year, y=mean_gdp_score, color=journal))+
  #geom_point()+
  geom_vline(xintercept = 2019)+
  geom_vline(xintercept = 2023)+
  geom_smooth()

ggplot(dt_ebdi_results_co_author[ebdi_score>0 & year>2010], 
       aes(x=year, y=ebdi_score, size=mean_gdp_score, color=journal))+
  #geom_point()+
  geom_vline(xintercept = 2019)+
  geom_vline(xintercept = 2023)+
  geom_smooth()

ggplot(dt_ebdi_results_co_author[year>2010], 
       aes(x=year, y=mean_gdp_score, color=journal))+
  geom_point()+
  geom_vline(xintercept = 2019)+
  geom_vline(xintercept = 2023)+
  geom_smooth()


table(authors.df.full$type)

xxx<-authors.df.full[year>=2010, c("country_iso3", "doi", "journal", "type", "year")]
xxx[journal=="DIVERSITY AND DISTRIBUTIONS", journal:="DDI"]
xxx[journal=="ECOGRAPHY", journal:="E"]
xxx[journal=="GLOBAL ECOLOGY AND BIOGEOGRAPHY", journal:="GEB"]
xxx[journal=="JOURNAL OF BIOGEOGRAPHY", journal:="JBI"]

fwrite(xxx[sample(nrow(xxx), 1e4)], "../Data/BIOGEOGRAPHY/authors.df.full.csv")
table(authors.df.full$journal)
