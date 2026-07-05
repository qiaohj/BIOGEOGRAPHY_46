library(data.table)
library(ggplot2)
library(sf)
library(segmented)
library(strucchange)
library(trend)
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

articles<-readRDS("../Data/BIOGEOGRAPHY/articles.rda")

articles$journal_abbr<-"DDI"
articles[journal=="GLOBAL ECOLOGY AND BIOGEOGRAPHY", journal_abbr:="GEB"]
articles[journal=="JOURNAL OF BIOGEOGRAPHY", journal_abbr:="JBI"]
articles[journal=="ECOGRAPHY", journal_abbr:="ECOGRAPHY"]


first_co_author<-authors[is_first_author==T | is_corresponding_author==T | is_co_first_author==T]
global_ns.N<-first_co_author[, .(N=length(unique(doi))), by=list(year, global_ns)]
global_ns_journal.N<-first_co_author[, .(N=length(unique(doi))), by=list(year, global_ns, journal_abbr)]

setorderv(global_ns.N, c("global_ns", "year"))
setorderv(global_ns_journal.N, c("global_ns", "journal_abbr", "year"))


table(global_ns.N$global_ns)

table(articles$year)
table(authors$year)


arcitles.N<-articles[,.(N=length(unique(doi))), by=list(year, journal_abbr)]
setorderv(arcitles.N, c("journal_abbr", "year"))

fwrite(arcitles.N, "../Data/BIOGEOGRAPHY/articles.N.csv")
fwrite(global_ns.N, "../Data/BIOGEOGRAPHY/global_ns.N.csv")


min_segment_length <- 5



arcitles.all<-arcitles.N
arcitles.all$journal_abbr<-"ALL"
arcitles.all<-arcitles.all[,.(N=sum(N)), by=list(year, journal_abbr)]
journal_abbrs <- unique(arcitles.all$journal_abbr)
setorderv(arcitles.all, "year")
processed_data_list <- list()
stats_list <- list()
breakpoints_list <- list()

for (j in journal_abbrs) {
  sub_dt <- arcitles.all[journal_abbr == j]
  
  if (nrow(sub_dt) < min_segment_length * 2) {
    bp_idx <- NA
  } else {
    bp_model <- tryCatch({
      breakpoints(N ~ year, data = sub_dt, h = min_segment_length)
    }, error = function(e) { NULL })
    
    if (is.null(bp_model)) {
      bp_idx <- NA
    } else {
      bp_idx <- bp_model$breakpoints
    }
  }
  
  if (all(is.na(bp_idx))) {
    sub_dt[, segment := "Seg_1"]
  } else {
    break_years <- sub_dt$year[bp_idx]
    breakpoints_list[[j]] <- data.table(journal_abbr = j, vline_year = break_years)
    
    sub_dt[, segment := paste0("Seg_", breakfactor(bp_model))]
  }
  
  for (seg in unique(sub_dt$segment)) {
    seg_data <- sub_dt[segment == seg]
    lm_fit <- lm(N ~ year, data = seg_data)
    
    if (nrow(seg_data) >= 3) {
      coef_summary <- summary(lm_fit)$coefficients
      slope <- coef_summary[2, 1]
      p_val <- coef_summary[2, 4]
    } else {
      slope <- 0; p_val <- 1 
    }
    sub_dt[segment == seg, fitted_N := predict(lm_fit)]
    
    trend_type <- fcase(
      p_val >= 0.05, "Stable",
      p_val < 0.05 & slope > 0, "Increase",
      p_val < 0.05 & slope < 0, "Decrease"
    )
    
    stats_list[[paste(j, seg)]] <- data.table(
      journal_abbr = j,
      segment = seg,
      mid_year = mean(seg_data$year),
      y_pos = max(seg_data$N) ,
      slope = slope,
      p_val = p_val,
      trend_type = trend_type
    )
  }
  
  processed_data_list[[j]] <- sub_dt
}

final_dt <- rbindlist(processed_data_list)
final_stats <- rbindlist(stats_list)
final_breaks <- if(length(breakpoints_list) > 0) 
  rbindlist(breakpoints_list) else data.table(journal_abbr=character(), vline_year=numeric())

final_stats[, label := sprintf("Slope: %.1f\np %s", 
                               slope, 
                               ifelse(p_val < 0.001, "< 0.001", paste0("= ", round(p_val, 3))))]

final_dt <- merge(final_dt, final_stats[, .(journal_abbr, segment, trend_type)], by = c("journal_abbr", "segment"))


trend_colors <- c(
  "Increase" = "#DF536B", 
  "Decrease" = "#0072B2", 
  "Stable" = "#000000"
)
saveRDS(final_stats, "../Data/BIOGEOGRAPHY/final_stats.rda")
saveRDS(final_dt, "../Data/BIOGEOGRAPHY/final_dt.rda")

final_stats<-readRDS("../Data/BIOGEOGRAPHY/final_stats.rda")
final_dt<-readRDS("../Data/BIOGEOGRAPHY/final_dt.rda")

final_stats[, y_pos:=y_pos-40]
p3 <- ggplot() +
  geom_point(data = final_dt, aes(x = year, y = N), color = "grey", alpha = 0.6) +
  geom_line(data = final_dt, aes(x = year, y = N), color = "grey", alpha = 0.6) +
  
  geom_line(data = final_dt, aes(x = year, y = fitted_N, group = segment, color = trend_type), 
            linewidth = 1) +
  
  {if(nrow(final_breaks) > 0) geom_vline(data = final_breaks, aes(xintercept = vline_year), 
                                         linetype = "dashed", color = "#999999", alpha = 0.6)} +
  
  geom_text(data = final_stats, aes(x = mid_year, y = y_pos, label = label, color = trend_type), 
            size = 3, fontface = "bold", show.legend = FALSE, vjust = 0) +
  
  facet_wrap(~ journal_abbr, scales = "free", ncol = 4) +
  
  scale_color_manual(values = trend_colors) +
  labs(
    x = "Year",
    y = "Number of papers",
    color = "Trend"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 11, face = "bold"),
    legend.position = "none",
    plot.title = element_text(size = 14, face = "bold"),
    panel.spacing = unit(1, "lines"),
    axis.title.x = element_blank()
  )

print(p3)



processed_data_list <- list()
stats_list <- list()
breakpoints_list <- list()

journal_abbrs <- unique(arcitles.N$journal_abbr)

for (j in journal_abbrs) {
  sub_dt <- arcitles.N[journal_abbr == j]
  
  index<-c(1:nrow(sub_dt))
  bp_idx<-index[which(sub_dt$year %in% final_breaks$vline_year)]
  
  sub_dt$segment<-"Seg"
  sub_dt[1:bp_idx[1], segment:="Seq_1"]
  sub_dt[bp_idx[1]:bp_idx[2], segment:="Seq_2"]
  sub_dt[bp_idx[2]:nrow(sub_dt), segment:="Seq_3"]
  if (j=="DDI"){
    sub_dt[1, segment:="Seq_2"]
  }
  
  break_years <- sub_dt$year[bp_idx]
  breakpoints_list[[j]] <- data.table(journal_abbr = j, vline_year = break_years)
  
  
  for (seg in unique(sub_dt$segment)) {
    seg_data <- sub_dt[segment == seg]
    lm_fit <- lm(N ~ year, data = seg_data)
    
    if (nrow(seg_data) >= 3) {
      coef_summary <- summary(lm_fit)$coefficients
      slope <- coef_summary[2, 1]
      p_val <- coef_summary[2, 4]
    } else {
      slope <- 0; p_val <- 1 
    }
    sub_dt[segment == seg, fitted_N := predict(lm_fit)]
    
    trend_type <- fcase(
      p_val >= 0.05, "Stable",
      p_val < 0.05 & slope > 0, "Increase",
      p_val < 0.05 & slope < 0, "Decrease"
    )
    
    stats_list[[paste(j, seg)]] <- data.table(
      journal_abbr = j,
      segment = seg,
      mid_year = mean(seg_data$year),
      y_pos = max(seg_data$N) ,
      slope = slope,
      p_val = p_val,
      trend_type = trend_type
    )
  }
  
  processed_data_list[[j]] <- sub_dt
}

final_dt <- rbindlist(processed_data_list)
final_stats <- rbindlist(stats_list)

final_stats[, label := sprintf("Slope: %.1f\np %s", 
                               slope, 
                               ifelse(p_val < 0.001, "< 0.001", paste0("= ", round(p_val, 3))))]

final_dt <- merge(final_dt, final_stats[, .(journal_abbr, segment, trend_type)], by = c("journal_abbr", "segment"))

final_stats[, y_pos:=y_pos-40]
final_breaks$journal_abbr<-NULL


p1 <- ggplot() +
  geom_point(data = final_dt, aes(x = year, y = N), color = "grey", alpha = 0.6) +
  geom_line(data = final_dt, aes(x = year, y = N), color = "grey", alpha = 0.6) +
  
  geom_line(data = final_dt, aes(x = year, y = fitted_N, group = segment, color = trend_type), 
            linewidth = 1) +
  
  {if(nrow(final_breaks) > 0) geom_vline(data = final_breaks, aes(xintercept = vline_year), 
                                         linetype = "dashed", color = "#999999", alpha = 0.6)} +
  
  geom_text(data = final_stats, aes(x = mid_year, y = y_pos, label = label, color = trend_type), 
            size = 3, fontface = "bold", show.legend = FALSE, vjust = 0) +
  
  facet_wrap(~ journal_abbr, scales = "free", ncol = 4) +
  
  scale_color_manual(values = trend_colors) +
  labs(
    x = "Year",
    y = "Number of papers",
    color = "Trend"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 11, face = "bold"),
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold"),
    panel.spacing = unit(1, "lines"),
    axis.title.x = element_blank()
  )

print(p1)


#-----------------------------------------------------------

processed_data_list <- list()
stats_list <- list()
breakpoints_list <- list()

global_nses <- unique(global_ns.N[!is.na(global_ns)]$global_ns)

for (j in global_nses) {
  sub_dt <- global_ns.N[global_ns == j]
  
  index<-c(1:nrow(sub_dt))
  bp_idx<-index[which(sub_dt$year %in% final_breaks$vline_year)]
  
  sub_dt$segment<-"Seg"
  sub_dt[1:bp_idx[1], segment:="Seq_1"]
  sub_dt[bp_idx[1]:bp_idx[2], segment:="Seq_2"]
  sub_dt[bp_idx[2]:nrow(sub_dt), segment:="Seq_3"]
  
  break_years <- sub_dt$year[bp_idx]
  breakpoints_list[[j]] <- data.table(global_ns = j, vline_year = break_years)
  
  
  for (seg in unique(sub_dt$segment)) {
    seg_data <- sub_dt[segment == seg]
    
    lm_fit <- lm(N ~ year, data = seg_data)
    
    if (nrow(seg_data) >= 3) {
      coef_summary <- summary(lm_fit)$coefficients
      slope <- coef_summary[2, 1]
      p_val <- coef_summary[2, 4]
    } else {
      slope <- 0; p_val <- 1 
    }
    
    sub_dt[segment == seg, fitted_N := predict(lm_fit)]
    
    trend_type <- fcase(
      p_val >= 0.05, "Stable",
      p_val < 0.05 & slope > 0, "Increase",
      p_val < 0.05 & slope < 0, "Decrease"
    )
    
    stats_list[[paste(j, seg)]] <- data.table(
      global_ns = j,
      segment = seg,
      mid_year = mean(seg_data$year),
      y_pos = max(seg_data$N) - max(global_ns.N$N)*0.05,
      slope = slope,
      p_val = p_val,
      trend_type = trend_type
    )
  }
  
  processed_data_list[[j]] <- sub_dt
}

final_dt <- rbindlist(processed_data_list)
final_stats <- rbindlist(stats_list)

final_stats[, label := sprintf("Slope: %.1f\np %s", 
                               slope, 
                               ifelse(p_val < 0.001, "< 0.001", paste0("= ", round(p_val, 3))))]

final_dt <- merge(final_dt, final_stats[, .(global_ns, segment, trend_type)], 
                  by = c("global_ns", "segment"))



final_stats[global_ns=="GN", y_pos:=y_pos-15]
final_stats[global_ns=="GS", y_pos:=y_pos+25]
final_stats[global_ns=="BCS", y_pos:=y_pos+30]
final_dt$global_ns<-factor(final_dt$global_ns, levels=c("GN", "GS", "BCS"), ordered=T)
final_stats$global_ns<-factor(final_stats$global_ns, levels=c("GN", "GS", "BCS"), ordered=T)

p2 <- ggplot() +
  geom_point(data = final_dt, aes(x = year, y = N), color = "grey", alpha = 0.6) +
  geom_line(data = final_dt, aes(x = year, y = N), color = "grey", alpha = 0.6) +
  
  geom_line(data = final_dt, aes(x = year, y = fitted_N, group = segment, color = trend_type), 
            linewidth = 1) +
  
  {if(nrow(final_breaks) > 0) geom_vline(data = final_breaks, aes(xintercept = vline_year), 
                                         linetype = "dashed", color = "#999999", alpha = 0.6)} +
  
  geom_text(data = final_stats, aes(x = mid_year, y = y_pos, label = label, color = trend_type), 
            size = 3, fontface = "bold", show.legend = FALSE, vjust = 0) +
  
  facet_wrap(~ global_ns, scales = "free_y", ncol = 3) +
  
  scale_color_manual(values = trend_colors) +
  labs(
    x = "Year",
    y = "Number of papers",
    color = "Trend"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 11, face = "bold"),
    legend.position = "none",
    plot.title = element_text(size = 14, face = "bold"),
    panel.spacing = unit(1, "lines"),
    axis.title.x = element_blank()
  )

print(p2)


p<-ggpubr::ggarrange(plotlist=list(p3, p2, p1), nrow=3)
p
ggsave(p, filename="../Figures/BIOGEOGRAPHY/Figure.Yearly.Trends/Figure.Yearly.Trends.pdf",
       width=12, height=10)



#-----------------------------------------------------------

processed_data_list <- list()
stats_list <- list()
breakpoints_list <- list()
global_ns_journal.N$gjlabel<-sprintf("%s,%s", 
                                     global_ns_journal.N$journal_abbr, 
                                     global_ns_journal.N$global_ns)
gjlabels <- unique(global_ns_journal.N[!is.na(global_ns)]$gjlabel)

for (j in gjlabels) {
  sub_dt <- global_ns_journal.N[gjlabel == j]
  if (min(sub_dt$year)>1998){
    item<-sub_dt[1]
    item$year<-1998
    item$N<-0
    sub_dt<-rbindlist(list(sub_dt, item))
  }
  index<-c(1:nrow(sub_dt))
  bp_idx<-index[which(sub_dt$year %in% final_breaks$vline_year)]
  
  sub_dt$segment<-"Seg"
  sub_dt[1:bp_idx[1], segment:="Seq_1"]
  sub_dt[bp_idx[1]:bp_idx[2], segment:="Seq_2"]
  sub_dt[bp_idx[2]:nrow(sub_dt), segment:="Seq_3"]
  
  break_years <- sub_dt$year[bp_idx]
  breakpoints_list[[j]] <- data.table(gjlabel = j, vline_year = break_years)
  
  
  for (seg in unique(sub_dt$segment)) {
    seg_data <- sub_dt[segment == seg]
    
    lm_fit <- lm(N ~ year, data = seg_data)
    
    if (nrow(seg_data) >= 3) {
      coef_summary <- summary(lm_fit)$coefficients
      slope <- coef_summary[2, 1]
      p_val <- coef_summary[2, 4]
    } else {
      slope <- 0; p_val <- 1 
    }
    
    sub_dt[segment == seg, fitted_N := predict(lm_fit)]
    
    trend_type <- fcase(
      p_val >= 0.05, "Stable",
      p_val < 0.05 & slope > 0, "Increase",
      p_val < 0.05 & slope < 0, "Decrease"
    )
    
    stats_list[[paste(j, seg)]] <- data.table(
      gjlabel = j,
      segment = seg,
      mid_year = mean(seg_data$year),
      y_pos = max(seg_data$N) - max(global_ns_journal.N$N)*0.05,
      slope = slope,
      p_val = p_val,
      trend_type = trend_type
    )
  }
  
  processed_data_list[[j]] <- sub_dt
}

final_dt <- rbindlist(processed_data_list)
final_stats <- rbindlist(stats_list)

final_stats[, label := sprintf("Slope: %.1f\np %s", 
                               slope, 
                               ifelse(p_val < 0.001, "< 0.001", paste0("= ", round(p_val, 3))))]

final_dt <- merge(final_dt, final_stats[, .(gjlabel, segment, trend_type)], 
                  by = c("gjlabel", "segment"))


final_dt$global_ns<-factor(final_dt$global_ns, levels=c("GN", "GS", "BCS"), ordered=T)


final_stats[, c("journal_abbr", "global_ns") := tstrsplit(gjlabel, ",", fixed=TRUE)]
final_stats$global_ns<-factor(final_stats$global_ns, levels=c("GN", "GS", "BCS"), ordered=T)
final_stats[global_ns=="GS", y_pos:=y_pos+10]

p4 <- ggplot() +
  geom_point(data = final_dt, aes(x = year, y = N), color = "grey", alpha = 0.6, size=1) +
  
  geom_line(data = final_dt, aes(x = year, y = N), color = "grey", alpha = 0.6, linewidth = 0.8) +
  
  geom_line(data = final_dt, aes(x = year, y = fitted_N, group = segment, color = trend_type), 
            linewidth = 0.8) +
  
  {if(nrow(final_breaks) > 0) geom_vline(data = final_breaks, aes(xintercept = vline_year), 
                                         linetype = "dashed", color = "#999999", alpha = 0.6)} +
  
  geom_text(data = final_stats, aes(x = mid_year, y = y_pos, label = label, color = trend_type), 
            size = 2, fontface = "bold", show.legend = FALSE, vjust = 0) +
  
  facet_grid(global_ns~ journal_abbr, scale="free") +
  
  scale_color_manual(values = trend_colors) +
  labs(
    x = "Year",
    y = "Number of papers",
    color = "Trend"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 11, face = "bold"),
    legend.position = "none",
    plot.title = element_text(size = 14, face = "bold"),
    panel.spacing = unit(1, "lines"),
    axis.title.x = element_blank()
  )

print(p4)


ggsave(p4, filename="../Figures/BIOGEOGRAPHY/Figure.Yearly.Trends/Figure.Yearly.Trends.Details.pdf",
       width=10, height=6)

