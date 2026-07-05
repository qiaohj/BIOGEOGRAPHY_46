library(data.table)
library(sf)
setwd("/path_to_your_project")

if (F){
  map<-read_sf("../Data/BIOGEOGRAPHY/World.China.Standard/China.Standard.shp")
  
  #Global North List
  #Definition: Includes North America, Western Europe, EU members, Developed Asia-Pacific (Japan, Korea, Australia, NZ), and Eastern European industrial/transition economies (Russia, Ukraine).
  global_north <- c(
    # --- North America ---
    "USA", "CAN", "BMU", "GRL", "SPM",
    
    # --- Western / Northern / Southern Europe (Developed/EU) ---
    "GBR", "FRA", "DEU", "ITA", "ESP", "PRT", "NLD", "BEL", "LUX", "CHE", "AUT", "IRL",
    "NOR", "SWE", "DNK", "FIN", "ISL", "LIE", "MCO", "AND", "SMR", "VAT", "GRC", "CYP", "MLT",
    
    # --- Eastern Europe (EU Members) ---
    "POL", "HUN", "CZE", "SVK", "EST", "LVA", "LTU", "SVN", "HRV", "ROU", "BGR",
    
    # --- Eastern Europe / CIS (Industrialized Transition Economies) ---
    # Note: Russia and Ukraine are often geographically North, though politically distinct.
    "RUS", "UKR", "BLR", "MDA", "ROM", "SEB", "MKD", "ALB", "YUG", "BIH",
    
    # --- Asia-Pacific (Developed Economies) ---
    "JPN", "KOR", "AUS", "NZL", "ISR",
    
    # --- Overseas Territories (Linked to Global North nations) ---
    "FLK", "NCL", "PYF", "ATF"
  )
  map$global.gn<-"Global South"
  map[which(map$SOC %in% global_north),]$global.gn<-"Global North"
  table(map$global.gn)
  map_sf<-st_simplify(map, dTolerance=0.1)
  map_sf <- st_set_crs(map_sf, 4326)
  eqearth_crs <- "+proj=eqearth +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
  map_sf <- st_transform(map_sf, crs = eqearth_crs)
  map_sf <- map_sf[which(!is.na(map_sf$SOC)),]
  map_sf[which(map_sf$SOC %in% c("CHN", "BRA", "ZAF")), ]$global.gn <- "Global South - BCS"
  p<-ggplot(map_sf)+geom_sf(aes(fill=global.gn))+
    scale_fill_manual(
      name = "",
      values = c("#0072B2", "#DF536B", "#F5C710"),
      labels = c("Global North", "Global South", "Global South - BCS")
    )+
    coord_sf()+theme_minimal()+theme(legend.position = "bottom")
  p
  ggsave(p, filename="../Figures/BIOGEOGRAPHY/global.ns/global.ns.pdf", width=10, height=7)
  write_sf(map_sf, "../Data/BIOGEOGRAPHY/world.shp")
  
  globalns<-unique(map_sf[, c("NAME", "SOC")])
  globalns$geometry<-NULL
  globalns<-data.table(globalns)
  
  authors<-readRDS("../Data/BIOGEOGRAPHY/authors.rda")
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
  
  
  all.abbr<-unique(authors$country_iso3)
  all.abbr[! (all.abbr %in% globalns$SOC)]
  
  globalns<-unique(map_sf[, c("NAME", "SOC", "global.gn")])
  globalns$geometry<-NULL
  globalns<-data.table(globalns)
  authors$global_ns<-NA
  authors[country_iso3 %in% globalns[global.gn=="Global North"]$SOC, global.gn:="GN"]
  authors[country_iso3 %in% globalns[global.gn=="Global South"]$SOC, global.gn:="GS"]
  authors[country_iso3 %in% globalns[global.gn=="Global South - BCS"]$SOC, global.gn:="BCS"]
  table(authors$global.gn)
  authors$global_ns<-NULL
  authors<-authors[!is.na(country_iso3)]
  saveRDS(globalns, "../Data/BIOGEOGRAPHY/globalns.rda")
  saveRDS(authors, "../Data/BIOGEOGRAPHY/authors.rda")
  
}


map<-data.table(map[, c("SOVEREIGNT", "ISO_A3_EH")])
map$geometry<-NULL



global.south$ISO3
