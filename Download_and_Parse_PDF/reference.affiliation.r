library(data.table)
library(ggpubr)
library(ggplot2)
setwd("/path_to_your_project")
reference.affiliation<-readRDS("../Data/BIOGEOGRAPHY/reference.affiliation.rda")
reference.affiliation<-unique(reference.affiliation[, c("article_DOI", "country_iso3")])
colnames(reference.affiliation)<-c("ref_DOI", "ref_country_iso3")
references.full<-readRDS("../Data/BIOGEOGRAPHY/reference.crossref.rda")
references.full<-unique(references.full[,c("article_DOI", "ref_DOI")])
references.full<-merge(references.full, reference.affiliation,
                       by="ref_DOI")


head(references.full)
articles<-readRDS("../Data/BIOGEOGRAPHY/articles.rda")
articles<-articles[, c("doi.raw", "journal", "year", "doi.prefix", "doi.suffix")]
colnames(articles)[1]<-"article_DOI"

fullset<-merge(references.full, articles, by="article_DOI")

df.N.ref<-fullset[, .(N_ref=length(unique(article_DOI))), by=list(journal, year)]
df.N.articles<-articles[, .(N_article=length(unique(article_DOI))), by=list(journal, year)]
df.N<-merge(df.N.ref, df.N.articles, by=c("journal", "year"), all=T)

length(unique(fullset$article_DOI))


get_lm_stats <- function(d) {
  m <- lm(N_article ~ N_ref, data = d)
  s <- summary(m)
  list(
    slope = coef(m)[2],
    p_value = s$coefficients[2, 4],
    r_squared = s$r.squared
  )
}

lm_results <- df.N[year<=2024, get_lm_stats(.SD), by = journal]
lm_results[, p_signif := ifelse(p_value < 0.05, "*", "")]


p2 <- ggplot(df.N[year<=2024], aes(x = N_ref, y = N_article)) +
  geom_point(aes(color = journal), size = 3, alpha = 0.6) +
  geom_smooth(method = "lm", color = "black", size = 0.8) +
  facet_wrap(~ journal, scales = "free") +
  stat_cor(method = "pearson", label.x.npc = "left", label.y.npc = "top") + 
  theme_classic() +
  labs(title = "",
       x = "Number of References",
       y = "Number of Articles")

p2
global.south<-fread("../Data/BIOGEOGRAPHY/global.south.csv")

fullset$global_sn<-"Global North"
fullset[fullset$ref_country_iso3]