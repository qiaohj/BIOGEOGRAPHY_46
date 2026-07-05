library(data.table)
library(ggplot2)
setwd("/path_to_your_project")

xxx<-readRDS("../Data/BIOGEOGRAPHY/doi_10_5061_dryad_p5hqbzkst__v20221226/wos_results_biog.rds")
authors<-data.table(xxx$author)
address<-data.table(xxx$address)
author_address<-data.table(xxx$author_address)
article_type<-data.table(xxx$doc_type)
final_author_address <- address[author_address[authors, on = .(ut, author_no)], on = .(ut, addr_no)]


publication<-xxx$publication
publication$abstract<-NULL
publication<-data.table(publication)

publication_type<-publication[article_type, on=.(ut)]

#check the papers with multi types
publication_type_N<-publication_type[,.(N=.N), by=(ut)]
publication_type_N[N>1]

dawson<-publication_type[doc_type %in% c("Article")]

dawson$year<-year(dawson$date) 



dawson$journal_group<-dawson$journal
dawson[!journal_group %in% 
            c("Journal of Biogeography", "Ecography", 
              "Diversity and Distributions", "Global Ecology and Biogeography"),
          journal_group:="Others"]
dawson[,.(N=.N), by=c("journal_group")]

dawson_N<-dawson[,.(N=.N), by=c("journal_group", "year")]
dawson_N<-dawson_N[journal_group!="Others"]
range(dawson_N$year)

dawson_N <- dawson_N[CJ(year = unique(dawson_N$year), 
                          journal_group = unique(dawson_N$journal_group)), 
         on = .(year, journal_group)]
dawson_N[is.na(N), N := 0] 

p<-ggplot(dawson_N, aes(x = year, y = N, fill = journal_group)) +
 geom_area(alpha = 0.85, color = "white", linewidth = 0.2) + 
  
  scale_fill_manual(values = c(
    "Journal of Biogeography" = "#CC79A7", 
    "Ecography"               = "#56B4E9", 
    "Global Ecology and Biogeography"          = "#009E73", 
    "Diversity and Distributions"     = "#E69F00"
  )) +
  
  scale_x_continuous(breaks = seq(1970, 2025, 10), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  
  labs(x = "Year", 
       y = "Published manuscripts", 
       fill = "Journal") + # 图例标题
  
  theme_minimal() +
  theme(
    legend.position = "bottom", 
    
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10)
  )
ggsave(p, filename="../Figures/BIOGEOGRAPHY/Dawson.2023.pdf", width=8, height=6)


clean_and_lower <- function(x) {
  return(tolower(gsub("[^a-zA-Z]", "", x)))
}
dawson$fixed_title<-clean_and_lower(dawson$title)

qiao<-readRDS("../Data/BIOGEOGRAPHY/articles.rda")
qiao$title <- stri_trans_general(qiao$title, "Latin-ASCII")

qiao$title<-gsub("‐", "-", qiao$title)
qiao$title<-gsub("–", "-", qiao$title)
qiao$title<-gsub("‘", "'", qiao$title)
qiao$title<-gsub("’", "'", qiao$title)
qiao$title<-gsub("<i>", "", qiao$title)
qiao$title<-gsub("</i>", "", qiao$title)
qiao$fixed_title<-clean_and_lower(qiao$title)

journals<-unique(qiao$journal)
wos.list<-list()
for (i in c(1:length(journals))){
  wos<-readRDS(sprintf("../Data/BIOGEOGRAPHY/WOS/%s.rda", journals[i]))
  wos<-wos[, c("uid", "doi.suffix", "document_type")]
  wos$journal<-journals[i]
  
  wos.list[[i]]<-wos
  
}
wos<-rbindlist(wos.list)
table(wos$document_type)
qiao$doi.suffix<-tolower(qiao$doi.suffix)
wos$doi.suffix<-tolower(wos$doi.suffix)
wos[uid=="WOS:000240149100003"]

qiao.wos<-merge(qiao, wos, by.x=c("doi.suffix", "journal"), 
            by.y=c("doi.suffix", "journal"), all.x=T, all.y=F)

qiao.wos$abstract<-NULL

qiao.wos<-qiao.wos[document_type %in% c("Article", "Article | Early Access", "Article | Data Paper")]

dawson<-dawson[journal_group!="Others"]
dawson$journal_group<-toupper(dawson$journal_group)
yearly_qiao<-qiao.wos[,.(N.qiao=length(unique(uid))), by=list(journal, year)]
yearly_dawson<-dawson[,.(N.dawson=length(unique(ut))), by=list(journal_group, year)]

yearly<-merge(yearly_qiao, yearly_dawson, 
              by.x=c("journal", "year"),
              by.y=c("journal_group", "year"),
              all=T)

yearly[is.na(N.qiao)]
yearly[is.na(N.dawson)]


dawson[journal_group=="GLOBAL ECOLOGY AND BIOGEOGRAPHY" & year==1997]

dawson[ut=="WOS:A1997XL65200001"]


wos.test<-readRDS(sprintf("../Data/BIOGEOGRAPHY/WOS/%s.rda", 
                          "GLOBAL ECOLOGY AND BIOGEOGRAPHY"))
wos.test[uid=="WOS:000834268800001"]


dim(qiao.wos)
dim(dawson)
unique(dawson$doc_type)

qiao_items<-unique(qiao.wos[year<=2022, 
                            c("uid", "journal", "year", "title", "fixed_title")])[, in_qiao := TRUE]
dawson_items<-unique(dawson[year<=2022, c("ut", "journal_group", "year", "title", "fixed_title")])[, in_dawson := TRUE]
colnames(qiao_items)<-c("uid", "journal", "year_qiao", "title_qiao", "fixed_title_qiao", "in_qiao")
colnames(dawson_items)<-c("uid", "journal", "year_dawson", "title_dawson", "fixed_title_dawson", "in_dawson")

article_merged <- merge(qiao_items, dawson_items, by = c("uid", "journal"), all = TRUE)
article_merged[is.na(in_qiao) & (fixed_title_dawson %in% qiao$fixed_title), in_qiao:=T]
article_merged[is.na(in_qiao)]

article_merged[, category := fcase(
  !is.na(in_qiao) & !is.na(in_dawson), "Overlap",
  !is.na(in_qiao) & is.na(in_dawson),  "Unique to Qiao",
  is.na(in_qiao) & !is.na(in_dawson),  "Unique to Dawson"
)]

article_merged[is.na(in_qiao) &
                 year_dawson>1000 & 
                 journal=="ECOGRAPHY" ]

test.title<-"Issue Information"
wos.test[grepl(test.title, title)]
qiao[grepl(test.title, title)]$fixed_title
dawson[grepl(test.title, title)]


N.yearly<-article_merged[,.(N=.N), by=list(category, year_qiao, year_dawson, journal)]

N.yearly[, year:=ifelse(is.na(year_qiao), year_dawson, year_qiao)]

ggplot(N.yearly[category!="Overlap"])+
  geom_bar(aes(x=year, y=N, fill=category), stat = "identity")+
  scale_fill_manual(values = c(
    "Unique to Qiao" = "#E69F00",
    "Overlap"     = "#56B4E9",
    "Unique to Dawson" = "#009E73"
  ))+
  facet_wrap(~journal)+
  theme_bw() +
  labs(
    title = "Comparison between Qiao and Dawson",
    subtitle = "Group-wise Overlap Analysis",
    x = NULL,
    y = "Count "
  ) +
  theme(
    legend.position = "right",
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 10)
  )

dawson[ut=="WOS:000240149100003"]$title

qiao[uid=="WOS:000398571000022"]
dawson[ut=="WOS:000398571000022"]

plot_data <- article_merged[, .N, by = .(journal, category)]

plot_data[, category := factor(category, 
                               levels = c("Unique to Qiao", "Overlap", "Unique to Dawson"))]

print(plot_data)

ggplot(plot_data, aes(x = category, y = N, fill = category)) +
  geom_col(width = 0.7, color = "black", alpha = 0.8) +
  geom_text(aes(label = N), vjust = -0.5, size = 4) +
  facet_wrap(~ journal, scales = "free_y") + 
  scale_fill_manual(values = c(
    "Unique to Qiao" = "#E69F00",
    "Overlap"     = "#56B4E9",
    "Unique to Dawson" = "#009E73"
  )) +
  theme_bw() +
  labs(
    title = "Comparison between Qiao and Dawson",
    subtitle = "Group-wise Overlap Analysis",
    x = NULL,
    y = "Count "
  ) +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 10)
  )
