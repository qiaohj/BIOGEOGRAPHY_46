library(data.table)
library(sf)
library(rnaturalearth)
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
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggh4x)
setwd("/path_to_your_project")

spdf_world<-read_sf("../Data/BIOGEOGRAPHY/world.shp")


authors<-readRDS("../Data/BIOGEOGRAPHY/authors.rda")

authors[country=="Taiwan", country_iso3:="CNH"]
authors[country=="Taiwan", country:="China"]

authors$journal_abbr<-"DDI"
authors[journal=="GLOBAL ECOLOGY AND BIOGEOGRAPHY", journal_abbr:="GEB"]
authors[journal=="JOURNAL OF BIOGEOGRAPHY", journal_abbr:="JBI"]
authors[journal=="ECOGRAPHY", journal_abbr:="ECOGRAPHY"]

authors$title<-NULL
authors$abstract<-NULL
colnames(authors)[34]<-"global_ns"

country.N<-authors[,.(N=length(unique(doi))), by=list(country_iso3)]

spdf_world.N<-merge(spdf_world, country.N, by.x="SOC", by.y="country_iso3", all.x=T)
spdf_world.N[which(is.na(spdf_world.N$N)),]$N<-0
write_sf(spdf_world.N, "../Figures/BIOGEOGRAPHY/Figure.Map/Shape/n.paper.from.country.kml")
p<-ggplot(spdf_world.N)+geom_sf(aes(fill=N))+
  scale_fill_gradient(
    low = "#0072B2",
    #mid = "#33a02c",
    high = "#DF536B",
    trans = "sqrt",
    name = "N Papers (sqrt Scale)",
    #midpoint = 20,
    breaks = c(1, 100, 400, 900, 1600, 2500, 3600), 
    labels = c("1", "100", "400", "900", "1600", "2500", "3600"),
    guide = guide_colorbar(
      barwidth = unit(10, "cm"), 
      barheight = unit(0.5, "cm")
    )
  ) +
  geom_sf(aes(fill = N), color = "gray80", size = 0.1) + 
  coord_sf()+
  theme_bw() +
  theme(legend.position = "bottom")
p

all.loc<-readRDS("../Data/BIOGEOGRAPHY/all.loc.paper.rda")
length(unique(all.loc$doi))
all.loc[country_code=="GRL", country_code:="DNK"]
all.loc[country_code=="GUF", country_code:="SUR"]

all.loc[country_code %in% c("HKG", "TWN", "MAC"), country_code:="CHN"]

all.loc.N<-all.loc[,.(N=length(unique(doi))), by=list(country_code)]
hist(all.loc.N$N)

spdf_world.N.loc<-merge(spdf_world, all.loc.N, by.x="SOC", by.y="country_code", all.x=T)
spdf_world.N.loc[which(is.na(spdf_world.N.loc$N)),]$N<-0
spdf_world.N.loc<-spdf_world.N.loc[which(spdf_world.N.loc$SOC !="ATA"),]

spdf_world.N.loc<-spdf_world.N.loc[which(spdf_world.N.loc$N>0),]
spdf_world.N.loc[which(spdf_world.N.loc$SOC=="GTM"),]
write_sf(spdf_world.N.loc, "../Figures/BIOGEOGRAPHY/Figure.Map/Shape/n.paper.by.country.kml")
hist(spdf_world.N.loc$N)
p2<-ggplot(spdf_world.N.loc[which(spdf_world.N.loc$N>=0),])+
  geom_sf(aes(fill=N))+
  scale_fill_gradient(
    low = "#0072B2",
    #mid = "#33a02c",
    high = "#DF536B",
    trans = "sqrt",
    name = "N Papers (sqrt scale)",
    #midpoint = 20,
    breaks = c(100, 400, 900, 1600, 2500, 3600, 4900), 
    labels = c("100", "400","900", "1600", "2500", "3600", "4900"),
    guide = guide_colorbar(
      barwidth = unit(10, "cm"), 
      barheight = unit(0.5, "cm")
    )
  ) +
  geom_sf(aes(fill = N), color = "gray80", size = 0.1) + 
  #geom_sf(data=spdf_world.N.loc[which(spdf_world.N.loc$N<300),], 
  #        fill="#1f78b4")+
  coord_sf()+
  theme_bw()+
  theme(legend.position = "bottom")
p2

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
spdf_world.N.loc$group<-NA
spdf_world.N.loc[which(spdf_world.N.loc$SOC %in% s.a), "group"]<-"South, Southeast Asia and Pacific Island"
spdf_world.N.loc[which(spdf_world.N.loc$SOC %in% m.w.a), "group"]<-"Western and Central Asia"
spdf_world.N.loc[which(spdf_world.N.loc$SOC %in% m.a), "group"]<-"Caribbean and Central America"
spdf_world.N.loc[which(spdf_world.N.loc$SOC %in% africa_iso3), "group"]<-"Africa"
base_colors <- c("South, Southeast Asia and Pacific Island" = "#0072B2", 
                 "Caribbean and Central America" = "#DF536B", 
                 "Western and Central Asia" = "#F5C710",
                 "Africa" = "#009E73"
                 )

pxa<-ggplot()+
  geom_sf(data=spdf_world.N.loc, fill=NA, color="grey60")+
  geom_sf(data=spdf_world.N.loc[!is.na(spdf_world.N.loc$group),], aes(fill=group))+
  scale_fill_manual(values=base_colors)+
  theme_minimal()+
  theme(legend.title = element_blank(),
        legend.position = "bottom")
pxa
unique(spdf_world.N.loc$group)
ggsave(pxa, filename="../Figures/BIOGEOGRAPHY/Figure.Map/hotareas.pdf",
       width=10, height=7)
range(spdf_world.N.loc$N)

#geographical_scope[doi=="DDI.12025", c("scope_name", "scope_abbr")]

authors_area<-unique(authors[is_corresponding_author==T, 
                             c("doi", "global_ns", "journal_abbr")])
colnames(authors_area)<-c("doi", "author.country", "journal")
locations<-unique(all.loc[, c("doi", "country_code")])
colnames(locations)<-c("doi", "loc.country")
locations<-locations[loc.country!=""]
unique(locations)
location.group<-unique(spdf_world.N.loc[, c("group", "SOC")])
location.group$geometry<-NULL
location.group<-data.table(location.group)
colnames(location.group)<-c("Area", "loc.country")
location.group<-location.group[!is.na(Area)]
all.sj<-merge(authors_area, merge(locations, location.group, by="loc.country"), by="doi")

all.sj.N<-all.sj[, .(N=length(unique(doi))), by=list(author.country, Area)]
all.sj.N<-all.sj.N[Area %in% c("South, Southeast Asia and Pacific Island",
                               "Western and Central Asia",
                               "Caribbean and Central America",
                               "Africa")]


ppp<-ggplot(all.sj.N, aes(x = author.country, y = Area, fill = N)) +
  geom_tile(color = "white") +
  geom_text(aes(label = N), color = "black", size = 5) +
  scale_fill_gradient(low = "white", high = "#e31a1c") +
  labs(#title = "Influence intensity of countries on regions",
       x = "Author country", y = "Target region", fill = "Number of papers") +
  theme_minimal() +
  theme(#axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.x = element_text(size = 10, face="bold"),
    axis.text.y = element_text(size = 12, face="bold"),
        title=element_text(face = "bold", size = 12),
    axis.title = element_blank(),
    legend.position = "bottom")

ppp
fwrite(all.sj.N, "../Figures/BIOGEOGRAPHY/Figure.heatmap.Influence/heatmap.Influence.csv")
ggsave(ppp, filename="../Figures/BIOGEOGRAPHY/Figure.heatmap.Influence/heatmap.Influence.pdf", width=16, height=4)



authors_area<-unique(authors[is_corresponding_author==T, c("doi", "country_iso3")])

colnames(authors_area)<-c("doi", "author.country")
locations<-unique(all.loc[, c("doi", "country_code")])
colnames(locations)<-c("doi", "loc.country")
locations<-locations[loc.country!=""]

location.group<-spdf_world.N.loc[, c("group", "SOC")]
location.group$geometry<-NULL
location.group<-data.table(location.group)
colnames(location.group)<-c("Area", "loc.country")
location.group<-location.group[!is.na(Area)]

all.sj<-merge(authors_area, merge(locations, location.group, by="loc.country"), by="doi")

all.sj.N<-all.sj[, .(N=length(unique(doi))), by=list(author.country, Area)]
all.sj.N<-all.sj.N[Area %in% c("South, Southeast Asia and Pacific Island",
                               "Western and Central Asia",
                               "Caribbean and Central America",
                               "Africa")]
all.sj.N[Area=="Africa"]
all.sj.N.sf<-merge(spdf_world.N.loc[, c("SOC")], all.sj.N, 
                   by.y="author.country", by.x="SOC")
spdf_world.mask<-spdf_world.N.loc
spdf_world.mask$Area<-spdf_world.mask$group
spdf_world.mask<-spdf_world.mask[which(spdf_world.mask$Area %in% c("South, Southeast Asia and Pacific Island",
                                                                   "Western and Central Asia",
                                                                   "Caribbean and Central America",
                                                                   "Africa")),]
all.sj.N.sf[which(all.sj.N.sf$Area=="Africa"),]

fig.df<-all.sj.N.sf
plist<-list()
a =fig.df[1,]$Area
for (a in rev(c("South, Southeast Asia and Pacific Island",
            "Western and Central Asia",
            "Caribbean and Central America",
            "Africa"))){
  p_if<-ggplot()+
    geom_sf(data=fig.df[which(fig.df$Area==a),], aes(fill=N), color=NA)+
    geom_sf(data=spdf_world.mask, fill="grey80", color=NA, alpha=0.8)+
    labs(title=a)+
    scale_fill_gradient(
      low = "white",
      #mid = "#33a02c",
      high = "#e31a1c",
      name = "N Papers",
      #midpoint = 20,
      #breaks = c(1, 100, 400, 900, 1600, 2500, 3600), 
      #labels = c("1", "100", "400", "900", "1600", "2500", "3600")
    ) +
    geom_sf(data=spdf_world, fill=NA, color="grey80", size=0.1, alpha=0.2)+
    #coord_cartesian()+
    theme_bw()+
    theme(legend.position = "bottom",
          title = element_text(face = "bold", size = 14))
  plist[[a]]<-p_if
}

fig.df[which(fig.df$N>200), "N"]<-200
p3<-ggplot()+
  geom_sf(data=fig.df, aes(fill=N), color=NA)+
  geom_sf(data=spdf_world.mask, fill="grey80", color=NA, alpha=0.8)+
  scale_fill_gradient(
    low = "white",
    #mid = "#33a02c",
    high = "#e31a1c",
    name = "N Papers",
    #midpoint = 20,
    breaks = c(50, 100, 150, 200), 
    labels = c("50", "100", "150", ">200")
  ) +
  geom_sf(data=spdf_world, fill=NA, color="grey80", size=0.1)+
  #coord_cartesian()+
  theme_bw()+
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold", size = 14))+
  facet_wrap(~Area)

p
p2
write_sf(fig.df, "../Figures/BIOGEOGRAPHY/Figure.Map/Shape/n.paper.by.area.kml")
p.final<-(p | p2) / p3+
  plot_layout(heights = c(1, 2))+
  plot_annotation(tag_levels = 'A')& 
  theme(plot.margin = margin(t = 0, b = 0, l = 5, r = 5))
p.final
ggsave(p.final, filename="../Figures/BIOGEOGRAPHY/Figure.Map/n.paper.by.area.pdf", 
       width=15, height=10)


