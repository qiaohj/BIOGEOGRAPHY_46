library(data.table)
library(ggplot2)
library(forcats)
library(ggrepel)

setwd("/path_to_your_project")
articles<-readRDS("../Data/BIOGEOGRAPHY/articles.rda")
authors.df.full.gpd<-readRDS("../Data/BIOGEOGRAPHY/authors.fixed.rda")

table(articles$year)
articles.N<-articles[,.(N=length(unique(doi))), by=list(journal, year)]
articles.N$journal.abbr<-factor(articles.N$journal, 
                                levels=c("DIVERSITY AND DISTRIBUTIONS",
                                         "ECOGRAPHY",
                                         "GLOBAL ECOLOGY AND BIOGEOGRAPHY",
                                         "JOURNAL OF BIOGEOGRAPHY"),
                                labels=c("DDI", "ECOG", "GEB", "JBI"))

custom_colors <- c("#E69F00", "#56B4E9", "#009E73", "#CC79A7")
journal_values <- c("DDI", "ECOG", "GEB", "JBI")
color_map <- setNames(custom_colors, journal_values)
fwrite(articles.N, "../Figures/BIOGEOGRAPHY/Figure.N.article.year/N_paper_per_year.csv")
p<-ggplot(articles.N[between(year, 2010, 2025)], 
          aes(x=year, y=N, color=journal.abbr))+geom_point()+geom_line()+
  labs(color="Journal", x="Year", y="Number of paper per year")+
  scale_color_manual(values=color_map)+
  theme_bw()+
  theme(axis.title.x = element_blank())
p
ggsave(p, filename="../Figures/BIOGEOGRAPHY/Figure.N.article.year/N_paper_per_year.pdf", width = 7, height = 4)


N.country<-authors.df.full.gpd[between(year, 2010, 2025) & (is_corresponding_author==T), 
                               .(N.articles=length(unique(doi))), 
                               by=c("country.group", "year", "journal.abbr")]
N.country$country.group<-factor(N.country$country.group, 
                                levels=c("Australia", "Brazil","Canada","China","France","Germany",
                                         "Spain",
                                         "United Kingdom","United States",
                                         "Others - H","Others - UM","Others - L"))

p<-ggplot(N.country, 
          aes(x=year, y=N.articles, color=journal.abbr))+
  geom_smooth(method="loess", se=F, alpha=0.2, linewidth=0.5)+
  geom_point()+
  labs(color="Journal", x="Year", y="Number of paper per year")+
  scale_color_manual(values=color_map)+
  theme_bw()+
  facet_wrap(~country.group, scale="free", ncol=3)+
  theme(strip.text = element_text(face = "bold", size = 12))
p
fwrite(N.country, "../Figures/BIOGEOGRAPHY/Figure.N.article.year/N_paper_per_year_country.csv")
ggsave(p, filename="../Figures/BIOGEOGRAPHY/Figure.N.article.year/N_paper_per_year_country.pdf", width = 10, height = 9)
