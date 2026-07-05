library(data.table)
setwd("/path_to_your_project")
setDTthreads(20)

authors<-readRDS("/path_to_your_project/Data.OLD/CrossRef_Full/2025/authors.rda")
articles.full<-readRDS("/path_to_your_project/Data.OLD/CrossRef_Full/2025/articles.rda")
references<-readRDS("/path_to_your_project/Data.OLD/CrossRef_Full/2025/references.rda")
articles<-readRDS("../Data/BIOGEOGRAPHY/articles.rda")
articles$doi.raw<-trimws(tolower(articles$doi.raw))
all<-length(references)
references.full<-list()
for (i in c(1:all)){
  print(paste(i, all))
  references.item<-references[[i]]
  references.item$article_DOI<-trimws(tolower(references.item$article_DOI))
  references.item<-references.item[article_DOI %in% articles$doi.raw]
  if (nrow(references.item)>0){
    references.full[[length(references.full)+1]]<-references.item
  }
}

references.full.df<-rbindlist(references.full)
dim(references.full.df)
references.full.df$ref_DOI<-trimws(tolower(references.full.df$ref_DOI))
saveRDS(references.full.df, "../Data/BIOGEOGRAPHY/reference.crossref.rda")
authors$article_DOI<-trimws(tolower(authors$article_DOI))
authors.biogeography<-authors[article_DOI %in% references.full.df$ref_DOI]

articles.full$doi<-trimws(tolower(articles.full$doi))
articles.biogeography<-articles.full[doi %in% references.full.df$ref_DOI]
dim(authors.biogeography)
dim(articles.biogeography)
saveRDS(articles.biogeography, "../Data/BIOGEOGRAPHY/articles.crossref.rda")
saveRDS(authors.biogeography, "../Data/BIOGEOGRAPHY/authors.crossref.rda")

references.full.df<-readRDS("../Data/BIOGEOGRAPHY/reference.crossref.rda")



affiliations<-readRDS("../Data/BIOGEOGRAPHY/affiliations.rda")

authors.biogeography<-readRDS("../Data/BIOGEOGRAPHY/authors.crossref.rda")
affiliations<-unique(authors.biogeography[sequence=="first" & affiliation!="", 
                                          c("article_DOI", "affiliation")])
saveRDS(affiliations, "../Data/BIOGEOGRAPHY/affiliations.rda")
fwrite(data.table(affiliation=affiliations[1:10000]), "../Data/BIOGEOGRAPHY/affiliations.csv")
fwrite(data.table(affiliation=affiliations[10001:20000]), "../Data/BIOGEOGRAPHY/affiliations2.csv")
fwrite(data.table(affiliation=affiliations[20001:30000]), "../Data/BIOGEOGRAPHY/affiliations3.csv")
fwrite(data.table(affiliation=affiliations[30001:40000]), "../Data/BIOGEOGRAPHY/affiliations4.csv")
fwrite(data.table(affiliation=affiliations[40001:50000]), "../Data/BIOGEOGRAPHY/affiliations5.csv")
fwrite(data.table(affiliation=affiliations[50001:60000]), "../Data/BIOGEOGRAPHY/affiliations6.csv")
fwrite(data.table(affiliation=affiliations[60001:64237]), "../Data/BIOGEOGRAPHY/affiliations7.csv")

table(authors.biogeography$sequence)
authors.biogeography[article_DOI=="10.1371/journal.pone.0244150"]
