library(data.table)
library(ggplot2)
library(sf)
library(segmented)
library(strucchange)
library(trend)
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
authors[country_iso3 %in% globalns[global.gn=="Global North"]$area_country_iso3, global.gn:="GN"]
authors[country_iso3 %in% globalns[global.gn=="Global South"]$area_country_iso3, global.gn:="GS"]
authors[country_iso3 %in% globalns[global.gn=="Global South - BCS"]$area_country_iso3, global.gn:="BCS"]

colnames(authors)[34]<-"global_ns"

authors$country.group<-authors$country_iso3

colnames(authors)[1]<-"author_country_iso3"

all.loc<-readRDS("../Data/BIOGEOGRAPHY/all.loc.paper.rda")
length(unique(all.loc$doi))
all.loc[country_code=="GRL", country_code:="DNK"]
all.loc[country_code=="GUF", country_code:="SUR"]

all.loc[country_code %in% c("HKG", "TWN", "MAC"), country_code:="CHN"]
colnames(all.loc)[4]<-"area_country_iso3"

authors_short<-authors[is_corresponding_author==T, c("author_country_iso3", "global_ns", "doi", "name", "year")]
authors_short<-authors_short[author_country_iso3!=""]
authors_short<-unique(authors_short)
all.loc_short<-all.loc[, c("area_country_iso3", "doi")]
all.loc_short<-all.loc_short[area_country_iso3!=""]
all.loc_short<-unique(all.loc_short)

df<-merge(authors_short, all.loc_short, by="doi")

s.a<-strsplit("MMR|THA|LAO|VNM|KHM|BGD|BTN|IND|MDV|NPL|LKA|BRN|IDN|MYS|PHL|SGP|TLS|FJI|KIR|MHL|FSM|NRU|PLW|PNG|WSM|SLB|TON|TUV|VUT",
              "\\|")[[1]]
m.w.a<-strsplit("PAK|AFG|ARM|AZE|BHR|CYP|GEO|IRN|IRQ|ISR|JOR|KWT|LBN|OMN|QAT|SAU|SYR|TUR|ARE|YEM|KAZ|KGZ|TJK|TKM|UZB|MNG",
                "\\|")[[1]]
m.a<-strsplit("ATG|BHS|BRB|CUB|DMA|DOM|GRD|HTI|JAM|KNA|LCA|VCT|TTO|BLZ|CRI|SLV|GTM|HND|NIC|PAN",
              "\\|")[[1]]
africa_iso3 <- c("AGO", "BDI", "BEN", "BFA", "BWA", "CAF", "CIV", "CMR", "COD", "COG", 
                 "COM", "CPV", "DJI", "DZA", "EGY", "ERI", "ESH", "ETH", "GAB", "GHA", 
                 "GIN", "GMB", "GNB", "GNQ", "IOT", "KEN", "LBR", "LBY", "LSO", "MAR", 
                 "MDG", "MLI", "MOZ", "MRT", "MUS", "MWI", "MYT", "NAM", "NER", "NGA", 
                 "REU", "RWA", "SDN", "SEN", "SHN", "SLE", "SOM", "STP", "SWZ", "SYC", 
                 "TCD", "TGO", "TUN", "TZA", "UGA", "ZAF", "ZMB", "ZWE")
df$group<-""
df[area_country_iso3 %in% s.a, group:="South, Southeast Asia and Pacific Island"]
df[area_country_iso3 %in% m.w.a, group:="Western and Central Asia"]
df[area_country_iso3 %in% m.a, group:="Caribbean and Central America"]
df[area_country_iso3 %in% africa_iso3, group:="Africa"]
setorderv(df.N, "N", -1)
head(df.N, 20)

target<-df.N[1:10]
df$author_group<-sprintf("%s - Others", df$global_ns)
df[author_country_iso3 %in% target$author_country_iso3, author_group:=
     df[author_country_iso3 %in% target$author_country_iso3]$author_country_iso3]


table(df$author_group)
df<-df[group!=""]
df[, c("doi", "group", "author_group")]
base_colors <- c("South, Southeast Asia and Pacific Island" = "#0072B2", 
                 "Caribbean and Central America" = "#DF536B", 
                 "Western and Central Asia" = "#F5C710",
                 "Africa" = "#009E73",
                 "white" = "#DDDDDD"
)

plot_dt <- unique(df[, .(doi, author_group, group)])

plot_dt <- plot_dt[, .(value = .N), by = .(author_group, group)]

total_n <- sum(plot_dt$value)
plot_dt[, prop_label := percent(value / total_n, accuracy = 0.1)]

plot_dt.N<-plot_dt[,.(N=sum(value)), by=list(author_group)]
setorderv(plot_dt.N, "N")
levels<-c(c("GS - Others", "GN - Others"), 
          plot_dt.N[!author_group %in% c("GN - Others", "GS - Others")]$author_group)
plot_dt$author_group<-factor(plot_dt$author_group, 
                      levels=levels,
                      ordered = T)

plot_dt$group<-factor(plot_dt$group, 
                      levels=rev(c("Africa", "Caribbean and Central America",
                                   "South, Southeast Asia and Pacific Island", 
                                   "Western and Central Asia")),
                      ordered = T)
legend_breaks <- setdiff(names(base_colors), "white")
p<-ggplot(plot_dt, aes(y = value, axis1 = group, axis2 = author_group)) +
  geom_alluvium(aes(fill = group), 
                width = 1/10, 
                alpha = 0.7, 
                color = "white", 
                linewidth = 0.2) +
  coord_flip() +
  geom_stratum(aes(fill = ifelse(after_stat(x) == 2, "white", 
                                 after_stat(as.character(stratum)))),
               width = 1/10) +
  
  geom_text_repel(
    stat = "stratum", 
    aes(label = ifelse(after_stat(x) == 2, 
                       after_stat(as.character(stratum)),
                       "")),
    nudge_x = 0.1, 
    hjust = 0.5,
    vjust = -0.5,
    size = 4, 
    color = "black", 
    fontface = "bold"
  )+
  
  scale_fill_manual(values = base_colors,
                    breaks = legend_breaks) +
  scale_x_discrete(limits = c("Author Groups", "Regions"), expand = c(.1, .1)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(y = "Number of Papers") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(), 
    axis.title = element_blank(),
    legend.title = element_blank(),
    legend.box.margin = margin(t = -30, unit = "pt"), 
    legend.margin = margin(t = 0, b = 0, unit = "pt"),
    legend.position = "bottom"
  )
p
fwrite(plot_dt, "../Figures/BIOGEOGRAPHY/Figure.Geo.Sankey/Geo.Sankey.csv")
ggsave(p, filename="../Figures/BIOGEOGRAPHY/Figure.Geo.Sankey/Figure.Geo.Sankey.pdf",
       width=12, height=5)
