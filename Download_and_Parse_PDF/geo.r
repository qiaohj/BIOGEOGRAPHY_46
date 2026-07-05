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
setwd("/path_to_your_project")
spdf_world <- read_sf("../Data/BIOGEOGRAPHY/world.shp")
authors<-readRDS("../Data/BIOGEOGRAPHY/authors.rda")

country.N<-authors[,.(N=length(unique(doi))), by=list(country_iso3)]

spdf_world.N<-merge(spdf_world, country.N, by.x="SOC", by.y="country_iso3", all.x=T)
spdf_world.N[which(is.na(spdf_world.N$N)),]$N<-0
spdf_world.N<-spdf_world.N[which(spdf_world.N$SOC !="ATA"),]
p<-ggplot(spdf_world.N)+geom_sf(aes(fill=N))+
  scale_fill_gradient(
    low = "#0072B2",
    #mid = "#33a02c",
    high = "#DF536B",
    trans = "sqrt",
    name = "N Papers (sqrt Scale)",
    #midpoint = 20,
    breaks = c(1, 100, 400, 900, 1600, 2500, 3600), 
    labels = c("1", "100", "400", "900", "1600", "2500", "3600")
  ) +
  geom_sf(aes(fill = N), color = "gray80", size = 0.1) + 
  coord_sf()+
  theme_bw() 
p
if (F){
  geographical_scope<-readRDS("../Data/BIOGEOGRAPHY/geographical_scope.rda")
  unique(geographical_scope$source)
  
  unknown<-unique(geographical_scope[is.na(scope_abbr), 
                                     c("doi", "scope_name")])
  
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
  
  
  models<-c("gemini-2.5-pro", "gemini-flash-latest", "gemini-2.0-flash")
  mstr<-models[2]
  
  system_instruction<-read_file("BIOGEOGRAPHY/geo_2_country.md")
  
  i=1
  for (i in seq(1, 68985, by=1e3)){
    item<-unknown[i:(i+1e3-1)]
    target.file<-sprintf("../Data/BIOGEOGRAPHY/geographical_scope/geographical_scope_%d.rda", i)
    if (file.exists(target.file)){
      next()
    }
    f<-sprintf("../Data/BIOGEOGRAPHY/geographical_scope/geographical_scope_%d.csv", i)
    if (file.exists(f)){
      next()
    }
    fwrite(item, f)
    print(f)
    tryCatch({
      api.index <- sample(1:nrow(apis), 1)
      gemini.key<-apis[api.index]$gemini.api
      google_genai$configure(api_key = gemini.key)
      model <- google_genai$GenerativeModel(mstr,
                                            generation_config=gen_config,
                                            safety_settings = safety_settings,
                                            system_instruction = system_instruction)
      
      
      uploaded_file <- google_genai$upload_file(path = f, 
                                                display_name = f)
      message(sprintf("File uploaded successfully. Name: %s", uploaded_file$name))
      print(system.time({
        message(sprintf("2. Generating content with Gemini (%s)...", mstr))
        response <- model$generate_content(uploaded_file)
        
      }))
      rrr<-py_to_r(response$to_dict())
      
      saveRDS(rrr, target.file)
      extracted_text <- response$text
      saveRDS(extracted_text, 
              sprintf("../Data/BIOGEOGRAPHY/geographical_scope/geographical_scope_%d.txt.rda", i))
      write_file(extracted_text, 
                 sprintf("../Data/BIOGEOGRAPHY/geographical_scope/geographical_scope_%d.llm.csv", i))
      
    },
    error = function(e) {
      message("Error: ", e$message)
      
      if (grepl("429", e$message, ignore.case = TRUE) || grepl("exceeded your current quota", e$message, ignore.case = TRUE)){
        apis<<-apis[-api.index]
        print(sprintf("remove api index %d from apis, %d api.keys left.", api.index, nrow(apis)))
        file.remove(target.file)
        if (nrow(apis)==0){
          stop("no api left. stop");
        }
      }
      if (grepl("he document has no pages", e$message)){
        file.remove(pdf_path)
        file.remove(xml_path)
        file.remove(target.file)
      }
    },
    warning = function(w) {
      message("Warning: ", w$message)
    },
    finally = {
      
    })
  }
  alldone<-list.files("../Data/BIOGEOGRAPHY/20251201/geographical_scope/", pattern="*.llm.csv")
  
  
  xlist<-list()
  for (i in alldone){
    f<-sprintf("../Data/BIOGEOGRAPHY/20251201/geographical_scope/%s", i)
    x<-fread(f)
    colnames(x)<-c("doi", "country_code")
    xlist[[length(xlist)+1]]<-x
  }
  x.df<-rbindlist(xlist, fill=T)
  dt_expanded <- x.df %>%
    as_tibble() %>%
    separate_rows(country_code, sep = "\\|") %>%
    setDT()
  dt_expanded$country_code<-gsub("\\\\", "", dt_expanded$country_code)
  dt_expanded$scope_name<-""
  colnames(dt_expanded)<-c("doi", "scope_abbr", "scope_name")
  unknown[doi=="DDI.12002"]
  known<-unique(geographical_scope[!is.na(scope_abbr), 
                                   c("doi", "scope_abbr", "scope_name")])
  xx<-unique(known[(!scope_abbr %in% spdf_world$SOC) & scope_name!="Global", 
                   c("scope_name")])
  xx$doi<-str_c("doi.", c(1:nrow(xx)))
  fwrite(xx, "../Data/BIOGEOGRAPHY/20251201/geographical_scope/others.csv")
  xx.llm<-fread("../Data/BIOGEOGRAPHY/20251201/geographical_scope/others.llm.csv")
  xx.llm <- xx.llm %>%
    as_tibble() %>%
    separate_rows(country_code, sep = "\\|") %>%
    setDT()
  xx.llm<-xx.llm[country_code!=""]
  xx<-merge(xx, xx.llm, by="doi")
  xx$doi<-NULL
  known.full<-merge(known, xx, by="scope_name", all.x=T, allow.cartesian=TRUE)
  known.full[is.na(country_code), 
             country_code:=known.full[is.na(country_code)]$scope_abbr]
  known.full<-known.full[scope_abbr!="global"]
  known.full<-unique(known.full)
  dt_expanded$country_code<-dt_expanded$scope_abbr
  all.loc<-rbindlist(list(known.full, dt_expanded), use.names=T)
  all.loc<-unique(all.loc)
  all.loc[which(all.loc$country_code %in% c("HKG", "TWN", "MAC")),]$country_code<-"CHN"
  dim(all.loc)
  dim(unique(all.loc))
  all.loc<-unique(all.loc)
  saveRDS(all.loc, "../Data/BIOGEOGRAPHY/all.loc.paper.rda")
}



all.loc<-readRDS("../Data/BIOGEOGRAPHY/all.loc.paper.rda")
all.loc.N<-all.loc[,.(N=length(unique(doi))), by=list(country_code)]
all.loc.N[country_code=="SOL"]




spdf_world.N.loc<-merge(spdf_world, all.loc.N, by.x="SOC", by.y="country_code", all.x=T)
spdf_world.N.loc[which(is.na(spdf_world.N.loc$N)),]$N<-0
spdf_world.N.loc<-spdf_world.N.loc[which(spdf_world.N.loc$SOC !="ATA"),]

spdf_world.N.loc<-spdf_world.N.loc[which(spdf_world.N.loc$N>0),]
spdf_world.N.loc[which(spdf_world.N.loc$ISO_A3=="GTM"),]

mex<-all.loc[country_code=="MEX"]$doi
gtm<-all.loc[country_code=="GTM"]$doi

x1<-all.loc[doi %in% mex & !(doi %in% gtm) & scope_name!=""]
#View(x1[scope_abbr=="MEX"])

all.loc[doi=="GEB.70000" & country_code=="MEX"]

p2<-ggplot(spdf_world.N.loc[which(spdf_world.N.loc$N>=400),])+
  geom_sf(aes(fill=N))+
  scale_fill_gradient(
    low = "#1f78b4",
    #mid = "#33a02c",
    high = "#e31a1c",
    trans = "sqrt",
    name = "N Papers (sqrt scale)",
    #midpoint = 20,
    breaks = c(404, 900, 1600, 2500, 3600), 
    labels = c("<400", "900", "1600", "2500", "3600")
  ) +
  geom_sf(aes(fill = N), color = "gray80", size = 0.1) + 
  geom_sf(data=spdf_world.N.loc[which(spdf_world.N.loc$N<400),], 
          fill="#1f78b4")+
  coord_sf()+
  theme_bw()
p2

s.a<-strsplit("MMR|THA|LAO|VNM|KHM|BGD|BTN|IND|MDV|NPL|LKA|BRN|IDN|MYS|PHL|SGP|TLS|FJI|KIR|MHL|FSM|NRU|PLW|PNG|WSM|SLB|TON|TUV|VUT",
              "\\|")[[1]]
m.w.a<-strsplit("PAK|AFG|ARM|AZE|BHR|CYP|GEO|IRN|IRQ|ISR|JOR|KWT|LBN|OMN|QAT|SAU|SYR|TUR|ARE|YEM|KAZ|KGZ|TJK|TKM|UZB|MNG",
                "\\|")[[1]]
m.a<-strsplit("ATG|BHS|BRB|CUB|DMA|DOM|GRD|HTI|JAM|KNA|LCA|VCT|TTO|BLZ|CRI|SLV|GTM|HND|NIC|PAN",
              "\\|")[[1]]
spdf_world.N.loc$group<-spdf_world.N.loc$CONTINENT
spdf_world.N.loc[which(spdf_world.N.loc$SOC %in% s.a), "group"]<-"South, Southeast Asia and Pacific Island"
spdf_world.N.loc[which(spdf_world.N.loc$SOC %in% m.w.a), "group"]<-"Western and Central Asia"
spdf_world.N.loc[which(spdf_world.N.loc$SOC %in% m.a), "group"]<-"Caribbean and Central America"
p<-ggplot(spdf_world.N.loc, aes(fill=group))+geom_sf()
range(spdf_world.N.loc$N)

#geographical_scope[doi=="DDI.12025", c("scope_name", "scope_abbr")]

spdf_world.N.loc[which(spdf_world.N.loc$N<100), c("NAME_ZH", "SOC")]
spdf_world.N.loc$SOC

st_write(spdf_world.N.loc, "../Data/BIOGEOGRAPHY/spdf_world.N.loc.shp", append=F)


authors<-unique(authors.df.full.gpd[is_corresponding_author==T, c("doi", "country.group", "journal.abbr")])
colnames(authors)<-c("doi", "author.country", "journal")
locations<-unique(all.loc[, c("doi", "country_code")])
colnames(locations)<-c("doi", "loc.country")
location.group<-unique(spdf_world.N.loc[, c("group", "SOC")])
location.group$geometry<-NULL
location.group<-data.table(location.group)
colnames(location.group)<-c("Area", "loc.country")
all.sj<-merge(authors, merge(locations, location.group, by="loc.country"), by="doi")

all.sj.N<-all.sj[, .(N=length(unique(doi))), by=list(author.country, Area)]
if (F){
  all.sj.N<-all.sj.N[Area %in% c("South, Southeast Asia and Pacific Island",
                                 "Western and Central Asia",
                                 "Caribbean and Central America",
                                 "Africa")]
  all.sj.N<-all.sj.N[author.country %in% c("Germany", "Canada", 
                                           "United Kingdom", "United States",
                                           "Australia", "Spain",
                                           "Brazil", "France", "China")]
}
all.sj.N<-all.sj.N[Area %in% c("South, Southeast Asia and Pacific Island",
                               "Western and Central Asia",
                               "Caribbean and Central America",
                               "Africa")]
ggplot(all.sj.N,
       aes(axis1 = author.country, axis2 = Area, y = N)) +
  geom_alluvium(aes(fill = author.country), width = 1/12, alpha = 0.8) +
  geom_stratum(width = 1/12, fill = "grey90", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3) +
  scale_x_discrete(limits = c("Region", "Country"), expand = c(.05, .05)) +
  labs(title = "Guineo–Congolian rainforest: Regions → Countries (ISO3 shown)",
       y = "Flow (example weights)", x = "") +
  theme_minimal()


ggplot(all.sj.N, aes(x = author.country, y = Area, fill = N)) +
  geom_tile(color = "white") +
  geom_text(aes(label = N), color = "black", size = 5) +
  scale_fill_gradient(low = "#1f78b4", high = "#e31a1c") +
  labs(title = "Influence intensity of countries on regions",
       x = "Author country", y = "Target region", fill = "Influence (N)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


all.sj.N[, country_total := sum(N), by = author.country]
all.sj.N[, area_total := sum(N), by = Area]

library(igraph)
library(ggraph)

g <- graph_from_data_frame(
  d = all.sj.N, 
  vertices = data.frame(name = unique(c(all.sj.N$author.country, all.sj.N$Area))),
  directed = TRUE
)

# 添加节点类型标签（国家 vs 地区）------------------------------------
V(g)$type <- V(g)$name %in% all.sj.N$Area
# 节点大小（总影响力或被影响力）--------------------------------------
V(g)$weight <- ifelse(
  V(g)$type == "Country",
  sapply(V(g)$name, function(x) sum(all.sj.N[author.country == x, N])),
  sapply(V(g)$name, function(x) sum(all.sj.N[Area == x, N]))
)

# 可视化 ---------------------------------------------------------------
set.seed(123)  # 固定布局

ggraph(g, layout = "bipartite") +
  geom_edge_link(aes(width = N), color = "gray70", alpha = 0.7) +
  geom_node_point(aes(size = weight, color = type)) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3, fontface = "bold") +
  scale_color_manual(values = c("Country" = "#0072B2", "Area" = "#E69F00")) +
  scale_size(range = c(3, 12)) +
  scale_edge_width(range = c(0.5, 3)) +
  theme_void() +
  theme(legend.position = "bottom") +
  labs(title = "Country–Region Influence Network",
       color = "Node Type", size = "Total Influence", edge_width = "Influence Intensity")



if (F){
  all.sj.N<-all.sj.N[Area %in% c("South, Southeast Asia and Pacific Island",
                                 "Western and Central Asia",
                                 "Caribbean and Central America",
                                 "Africa")]
  all.sj.N<-all.sj.N[author.country %in% c("Germany", "Canada", 
                                           "United Kingdom", "United States",
                                           "Australia", "Spain",
                                           "Brazil", "France", "China")]
}
all.sj.N<-all.sj.N[Area %in% c("South, Southeast Asia and Pacific Island",
                               "Western and Central Asia",
                               "Caribbean and Central America",
                               "Africa")]
ggplot(all.sj.N,
       aes(axis1 = author.country, axis2 = Area, y = N)) +
  geom_alluvium(aes(fill = author.country), width = 1/12, alpha = 0.8) +
  geom_stratum(width = 1/12, fill = "grey90", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3) +
  scale_x_discrete(limits = c("Region", "Country"), expand = c(.05, .05)) +
  labs(title = "Guineo–Congolian rainforest: Regions → Countries (ISO3 shown)",
       y = "Flow (example weights)", x = "") +
  theme_minimal()


ggplot(all.sj.N, aes(x = author.country, y = Area, fill = N)) +
  geom_tile(color = "white") +
  geom_text(aes(label = N), color = "black", size = 5) +
  scale_fill_gradient(low = "#1f78b4", high = "#e31a1c") +
  labs(title = "Influence intensity of countries on regions",
       x = "Author country", y = "Target region", fill = "Influence (N)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


all.sj.N[, country_total := sum(N), by = author.country]
all.sj.N[, area_total := sum(N), by = Area]

library(igraph)
library(ggraph)

g <- graph_from_data_frame(
  d = all.sj.N, 
  vertices = data.frame(name = unique(c(all.sj.N$author.country, all.sj.N$Area))),
  directed = TRUE
)

# 添加节点类型标签（国家 vs 地区）------------------------------------
V(g)$type <- V(g)$name %in% all.sj.N$Area
# 节点大小（总影响力或被影响力）--------------------------------------
V(g)$weight <- ifelse(
  V(g)$type == "Country",
  sapply(V(g)$name, function(x) sum(all.sj.N[author.country == x, N])),
  sapply(V(g)$name, function(x) sum(all.sj.N[Area == x, N]))
)

# 可视化 ---------------------------------------------------------------
set.seed(123)  # 固定布局

ggraph(g, layout = "bipartite") +
  geom_edge_link(aes(width = N), color = "gray70", alpha = 0.7) +
  geom_node_point(aes(size = weight, color = type)) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3, fontface = "bold") +
  scale_color_manual(values = c("Country" = "#0072B2", "Area" = "#E69F00")) +
  scale_size(range = c(3, 12)) +
  scale_edge_width(range = c(0.5, 3)) +
  theme_void() +
  theme(legend.position = "bottom") +
  labs(title = "Country–Region Influence Network",
       color = "Node Type", size = "Total Influence", edge_width = "Influence Intensity")





authors<-unique(authors.df.full.gpd[is_corresponding_author==T, c("doi", "country_iso3")])

authors[which(authors$author.country %in% c("HKG", "TWN", "MAC")),]$author.country<-"CHN"

colnames(authors)<-c("doi", "author.country")
locations<-unique(all.loc[, c("doi", "country_code")])
colnames(locations)<-c("doi", "loc.country")
location.group<-spdf_world.N.loc[, c("group", "SOC")]
location.group$geometry<-NULL
location.group<-data.table(location.group)
colnames(location.group)<-c("Area", "loc.country")
all.sj<-merge(authors, merge(locations, location.group, by="loc.country"), by="doi")

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


ggplot()+
  
  geom_sf(data=all.sj.N.sf[all.sj.N.sf$SOC!="USA",], aes(fill=N), color=NA)+
  geom_sf(data=spdf_world.mask, fill="grey80", color=NA, alpha=0.8)+
  scale_fill_gradient(
    low = "white",
    #mid = "#33a02c",
    high = "#e31a1c",
    name = "N Papers",
    #midpoint = 20,
    #breaks = c(1, 100, 400, 900, 1600, 2500, 3600), 
    #labels = c("1", "100", "400", "900", "1600", "2500", "3600")
  ) +
  geom_sf(data=spdf_world, fill=NA, color="grey80", size=0.1)+
  coord_sf()+
  theme_bw() +
  facet_wrap(~Area, nrow=2)

  
  