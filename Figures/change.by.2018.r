library(data.table)
library(ggplot2)
library(forcats)
library(ggrepel)

setwd("/path_to_your_project")
authors<-readRDS("../Data/BIOGEOGRAPHY/authors.rda")

authors[country=="Taiwan", country_iso3:="CNH"]
authors[country=="Taiwan", country:="China"]
authors[country_iso3=="GBR, CHN", country_iso3:="GBR"]
authors[country_iso3 %in% c("HKG", "TWN", "CNH"), country_iso3:="CHN"]
authors[country_iso3 %in% c("GUM"), country_iso3:="USA"]
authors[country_iso3 %in% c("IMN", "JEY"), country_iso3:="GBR"]
authors[country_iso3 %in% c("FRS"), country_iso3:="FRA"]
authors[country_iso3 %in% c("ROU"), country_iso3:="ROM"]
authors[country_iso3 %in% c("SUN"), country_iso3:="RUS"]
authors[country_iso3 %in% c("SUI"), country_iso3:="CHE"]
authors[country_iso3 %in% c("SRB"), country_iso3:="SEB"]
authors[country_iso3 %in% c("CSK"), country_iso3:="CZE"]
authors[country_iso3 %in% c("MNE"), country_iso3:="SEB"]

authors$journal_abbr<-"DDI"
authors[journal=="GLOBAL ECOLOGY AND BIOGEOGRAPHY", journal_abbr:="GEB"]
authors[journal=="JOURNAL OF BIOGEOGRAPHY", journal_abbr:="JBI"]
authors[journal=="ECOGRAPHY", journal_abbr:="ECOGRAPHY"]

authors$title<-NULL
authors$abstract<-NULL
authors$global.gn<-""
authors[country_iso3=="ARG"]
globalns<-readRDS("../Data/BIOGEOGRAPHY/globalns.rda")
authors[country_iso3 %in% globalns[global.gn=="Global North"]$SOC, global.gn:="GN"]
authors[country_iso3 %in% globalns[global.gn=="Global South"]$SOC, global.gn:="GS"]
authors[country_iso3 %in% globalns[global.gn=="Global South - BCS"]$SOC, global.gn:="BCS"]

colnames(authors)[34]<-"global_ns"

authors$country.group<-authors$country_iso3

article.N.Test<-authors[between(year, 2010, 2030), 
                                 .(N=length(unique(doi))), by=list(country_iso3)]
setorderv(article.N.Test, "N", -1)
top10<-article.N.Test[1:10]$country_iso3
top10<-c(top10, "ZAF")
authors[!country.group %in% top10, country.group:=sprintf("Others-%s",
                                                          authors[!country.group %in% top10]$global_ns)]
table(authors$global.gn)
table(authors$country.group)

item<-authors[is_corresponding_author==T]
table(item$global_ns)

author.N<-item[, .(N=length(unique(doi))), by=c("year", "country.group", "journal_abbr")]
article.N<-authors[, .(N.article=length(unique(doi))),
                               by=c("year", "journal_abbr")]
article.N.full<-merge(author.N, article.N, by=c("year", "journal_abbr"))


unique(article.N.full$country.group)
article.N.full$country.group<-factor(article.N.full$country.group, 
                                     levels=c("GBR", "USA", "CAN", "AUS", "ZAF",      
                                              "CHE", "ESP", "FRA", "DEU", "CHN", "BRA",
                                              "Others-GN", "Others-GS"))
article.N.full$journal.abbr<-factor(article.N.full$journal, 
                                    levels=c("DDI",
                                             "ECOGRAPHY",
                                             "GEB",
                                             "JBI"),
                                    labels=c("DDI", "ECOGRAPHY", "GEB", "JBI"))

article.N.full$per<-article.N.full$N/article.N.full$N.article
article.N.full$period <- ifelse(article.N.full$year < 2018, "Pre-2018", "Post-2018")

article.N.full[, period := factor(period, levels = c("Pre-2018", "Post-2018"))]
coms<-article.N.full[,.(N=.N), by=list(country.group, journal.abbr)]
i=4
item.list<-list()
for (i in c(1:nrow(coms))){
  com<-coms[i]
  item<-article.N.full[country.group==com$country.group & journal.abbr==com$journal.abbr]
  test_result <- wilcox.test(per ~ period, data = item, alternative = "two.sided", exact=T)
  p_val <- test_result$p.value
  item.N<-item[, .(mean.v=mean(per), N.record=.N, p_val=p_val),
               by=list(period, journal.abbr, country.group)]
  item.flat<-data.table(p_value=p_val, 
                        mean_pre=item.N[period=="Pre-2018"]$mean.v,
                        mean_post=item.N[period=="Post-2018"]$mean.v,
                        N_pre=item.N[period=="Pre-2018"]$N.record,
                        N_Post=item.N[period=="Post-2018"]$N.record,
                        country.group=com$country.group,
                        journal.abbr=com$journal.abbr)
  item.list[[i]]<-item.flat
}
analysis_dt<-rbindlist(item.list)
analysis_dt[, mean_diff := mean_post - mean_pre]
analysis_dt[, Significance_Star := cut(p_value, 
                                       breaks = c(0, 0.001, 0.01, 0.05, 2),
                                       labels = c("***", "**", "*", ""),
                                       right = FALSE)]


analysis_dt[, Result_Category := fcase(
  is.na(p_value), "N/A",
  p_value < 0.05 & mean_diff > 0, paste0("⬆", Significance_Star, ""),
  p_value < 0.05 & mean_diff < 0, paste0("⬇", Significance_Star, ""),
  default = ""
)]

plot_dt <- melt(analysis_dt, 
                id.vars = c("journal.abbr", "country.group", "Result_Category", "Significance_Star", "mean_diff"),
                measure.vars = c("mean_pre", "mean_post"),
                variable.name = "period_name",
                value.name = "per_mean")

plot_dt[, period := factor(gsub("mean_", "", period_name), levels = c("pre", "post"), 
                           labels = c("Pre-2018", "Post-2018"))]

plot_dt[, facet_label := paste0(journal.abbr, " - ", country.group)]

plot_dt[, facet_label := fct_reorder(facet_label, mean_diff)]


p <- ggplot(plot_dt, aes(x = period, y = per_mean, group = facet_label, color = Result_Category)) +
  geom_line(aes(), linewidth = 1.2) +
  geom_point(aes(shape = period), size = 3) +
  geom_text_repel(data=plot_dt[period=="Post-2018"],
                  aes(x = 2, y = per_mean, 
                      label = paste0("", sprintf("%.2f%%", mean_diff * 100), 
                                     Result_Category, sprintf("(%s)", country.group))),
                  hjust = -1, vjust=1, direction = "y", size = 3.5) +
  scale_color_manual(values = c(
    "⬆*" = "#e31a1c",
    "⬆**" = "#e31a1c",
    "⬆***" = "#e31a1c",
    "⬇*" = "#33a02c",
    "⬇**" = "#33a02c",
    "⬇***" = "#33a02c",
    "X" = "gray50",
    "N/A" = "#1f78b4"
  )) +
  scale_y_sqrt(labels = scales::percent) +
  scale_x_discrete(
    expand = expansion(mult = c(0.05, 0.7)) 
  )+
  facet_wrap(~ journal.abbr, scales = "free_y", ncol = 2) +
  labs(
    #title = "Percentage Change Comparison: Pre-2018 vs. Post-2018",
    #subtitle = "Wilcoxon Rank-Sum Test used for significance (p < 0.05)",
    y = "Mean Percentage (per year)",
    x = "Period",
    color = "Statistical Outcome"
  ) +
  guides(color = guide_legend(
    override.aes = list(
      linetype = 0,
      shape = 19,
      size = 3 
    )
  ))+
  theme_bw() +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 14),
    axis.title.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    
  )

p

fwrite(plot_dt, "../Figures/BIOGEOGRAPHY/Figure.change.by.2018/change.by.2018.csv")
cairo_pdf("../Figures/BIOGEOGRAPHY/Figure.change.by.2018/change.by.2018.pdf", width = 10, height = 10) 

print(p) 
dev.off()
