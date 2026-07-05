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
library(patchwork)


setwd("/path_to_your_project")
authors.df.full.gpd<-readRDS("../Data/BIOGEOGRAPHY/authors.fixed.rda")

authors.df.full.gpd[country_iso3=="TWN"]
generate.pie.data<-function(N.country, label, title){
  plot_data <- N.country[, {
    sorted_group <- .SD[order(-N.articles)]
    top3 <- sorted_group[1:min(3, .N)]
    if (.N > 3) {
      others_sum <- sum(sorted_group[4:.N, N.articles])
      others_row <- data.table(
        country_name = "Others",
        N.articles = others_sum
      )
      result <- rbind(top3[, .(country_name, N.articles)], others_row)
    } else {
      result <- top3[, .(country_name, N.articles)]
    }
    result
  }, by = gdp.type]
  plot_data[country_name=="Others", country_name:=sprintf("%s - %s", country_name, gdp.type)]
  plot_data[, `:=`(
    percentage = N.articles / sum(N.articles),
    label = scales::percent(N.articles / sum(N.articles), accuracy = 0.1)
  ), by = gdp.type]
  
  plot_data <- plot_data[order(gdp.type, -N.articles)]
  plot_data[, label_pos := cumsum(N.articles) - 0.5 * N.articles, by = gdp.type]
  
  #print(plot_data)
  
  gdp_data <- plot_data[, .(total_articles = sum(N.articles)), by = gdp.type]
  setorder(gdp_data, gdp.type)
  gdp_data[, xmax := cumsum(total_articles)]
  gdp_data[, xmin := xmax - total_articles]
  gdp_data[, ymin := 2]; gdp_data[, ymax := 3.5]
  gdp_data[, label_pos := (xmin + xmax) / 2]
  #gdp_data[, label_text := paste0(gdp.type, " (", 
  #                                scales::percent(total_articles / sum(total_articles), 0.1), ")")]
  gdp_data[, label_text := paste0(gdp.type, "\n(", 
                                  scales::percent(total_articles / sum(total_articles), 0.1), ")")]
  gdp_data[gdp.type=="L", label_text:=gsub("\n", " ", label_text)]
  setorder(plot_data, gdp.type, -N.articles)
  plot_data[, xmax := cumsum(N.articles)]
  plot_data[, xmin := xmax - N.articles]
  plot_data[, ymin := 3.5]; plot_data[, ymax := 5]
  plot_data[, label_pos := (xmin + xmax) / 2]
  plot_data[, label_text := paste0(country_name, "\n", N.articles)]
  
  plot_data[, label_y_pos := (ymin + ymax) / 2]
  
  
  base_colors <- c("H" = "#1f78b4", "L" = "#33a02c", "UM" = "#e31a1c")
  country_colors <- plot_data[, {
    base_color <- base_colors[.BY$gdp.type]
    color_palette <- colorRampPalette(c(lighten(base_color, 0.3), darken(base_color, 0.3)))(.N)
    .(country_name = country_name, color = color_palette)
  }, by = gdp.type]
  all_colors <- setNames(
    c(unname(base_colors), country_colors$color),
    c(names(base_colors), country_colors$country_name)
  )
  plot_data$tab<-label
  gdp_data$tab<-label
  fwrite(plot_data, sprintf("../Figures/BIOGEOGRAPHY/Figure.Pie.Overview/overview.pie.plot_data.%s.csv", label))
  fwrite(gdp_data, sprintf("../Figures/BIOGEOGRAPHY/Figure.Pie.Overview/overview.pie.gdp_data.%s.csv", label))
  p <- ggplot() +
    geom_rect(data = plot_data, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = country_name), color = "white", linewidth = 0.5) +
    geom_rect(data = gdp_data, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = gdp.type), color = "white", linewidth = 0.5) +
    geom_text(data = gdp_data, aes(x = label_pos, y = (ymin + ymax) / 2, label = label_text), size = 4, color = "white", fontface = "bold") +
    geom_text_repel(
      data = plot_data,
      aes(x = label_pos, y = label_y_pos, label = label_text),
      size = 3.5,
      nudge_y = 2.5,
      segment.color = "grey50",
      segment.size = 0.5,
      box.padding = 0.3,
      point.padding = 0.3,
      min.segment.length = 0,
      force = 5, 
      show.legend = FALSE
    ) +
    ggtitle(title)+
    scale_fill_manual(values = all_colors) +
    coord_polar(theta = "x", start = 0) +
    ylim(0, 7) +
    #theme_minimal()+
    theme_void() +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold", size = 14, margin = margin(t=5, b = 0, l=2)),
      plot.subtitle = element_blank(),
      plot.margin = unit(c(2, 0, 0, 0), "pt"),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.line = element_blank(),
      plot.background = element_rect(
        color = "black",
        fill = NA, 
        linewidth = 0.5,
        linetype = "solid"
      )
    )
  
  p
  
}

N.country<-authors.df.full.gpd[between(year, 2010, 2025) & (is_first_author==T | 
                                                              is_corresponding_author==T | 
                                                              is_co_first_author==T), 
                               .(N.articles=length(unique(doi)),
                                 N.yeaer=length(unique(year))), 
                               by=c("country_iso3", "country_name", "gdp.type")]
sum(N.country[gdp.type=="H"]$N.articles)/sum(N.country$N.articles)

sum(N.country[country_iso3=="USA"]$N.articles)/sum(N.country$N.articles)


p1<-generate.pie.data(N.country, "first.co.author",
                      "(A) First, co-first and corresponding authors")
p1

N.country<-authors.df.full.gpd[between(year, 2010, 2025) & (is_first_author==T), 
                               .(N.articles=length(unique(doi)),
                                 N.yeaer=length(unique(year))), 
                               by=c("country_iso3", "country_name", "gdp.type")]

p2<-generate.pie.data(N.country, "first",
                      "(B) First author only")
p2

N.country<-authors.df.full.gpd[between(year, 2010, 2025) & (is_corresponding_author==T), 
                               .(N.articles=length(unique(doi)),
                                 N.yeaer=length(unique(year))), 
                               by=c("country_iso3", "country_name", "gdp.type")]

p3<-generate.pie.data(N.country, "corresponding",
                      "(C) Corresponding author only")
p3

N.country<-authors.df.full.gpd[between(year, 2010, 2025) & (is_first_author==F & is_co_first_author==T), 
                               .(N.articles=length(unique(doi)),
                                 N.yeaer=length(unique(year))), 
                               by=c("country_iso3", "country_name", "gdp.type")]

p4<-generate.pie.data(N.country, "cofirst",
                      "(D) Co-first author only")
p4

final_plot_patchwork <- (p1 + p2) / (p3 + p4) +
  #plot_annotation(tag_levels = 'A')+
  theme(plot.margin = unit(c(0, 0, 0, 0), "pt"))
N.country<-authors.df.full.gpd[between(year, 2010, 2025) & 
                                 (is_first_author==T | is_co_first_author==T | is_corresponding_author==T), 
                               .(N.articles=length(unique(doi)),
                                 N.yeaer=length(unique(year))), 
                               by=c("country_iso3", "country_name", "gdp.type",
                                    "is_first_author", "is_co_first_author",
                                    "is_corresponding_author")]
fwrite(N.country, "../Figures/BIOGEOGRAPHY/Figure.Pie.Overview/overview.pie.csv")
ggsave(final_plot_patchwork, filename="../Figures/BIOGEOGRAPHY/Figure.Pie.Overview/overview.pie.pdf",
       width=13, height=10)

