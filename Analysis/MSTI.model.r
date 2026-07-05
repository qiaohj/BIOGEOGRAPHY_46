library(data.table)
library(ggplot2)
library(broom)
library(car)
library(glmmTMB)
library(sjPlot)
library(performance)
library(ggcorrplot)
library(DHARMa)
library(emmeans)

authors<-readRDS("../Data/BIOGEOGRAPHY/authors_MSTI.rda")

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
authors$global_gn<-""
authors[country_iso3=="ARG"]
globalns<-readRDS("../Data/BIOGEOGRAPHY/globalns.rda")
authors[country_iso3 %in% globalns[global.gn=="Global North"]$SOC, global_gn:="GN"]
authors[country_iso3 %in% globalns[global.gn=="Global South"]$SOC, global_gn:="GS"]
authors[country_iso3 %in% globalns[global.gn=="Global South - BCS"]$SOC, global_gn:="BCS"]
table(authors$global_gn)

colnames(authors)[38]<-"global_ns"


authors<-authors[!is.na(country_iso3)]


table(authors$global_ns)

table(authors$GERD_Status)
table(authors$GDP_Status)

colnames(authors)
first_co_author<-authors[is_first_author==T | is_corresponding_author==T | is_co_first_author==T]

first_co_author[is.na(GERD_VALUE) & !(is.na(GERD_VALUE_FILLED)), 
                GERD_VALUE:=first_co_author[is.na(GERD_VALUE) & !(is.na(GERD_VALUE_FILLED))]$GERD_VALUE_FILLED]
first_co_author[is.na(GDP) & !(is.na(GDP_FILLED)), 
                GDP:=first_co_author[is.na(GDP) & !(is.na(GDP_FILLED))]$GDP_FILLED]
first_co_author[is.na(H_RS_VALUE) & !(is.na(H_RS_VALUE_FILLED)), 
                H_RS_VALUE:=first_co_author[is.na(H_RS_VALUE) & !(is.na(H_RS_VALUE_FILLED))]$H_RS_VALUE_FILLED]
first_co_author[is.na(GERD_REST_VALUE) & !(is.na(GERD_REST_VALUE_FILLED)), 
                GERD_REST_VALUE:=first_co_author[is.na(GERD_REST_VALUE) & 
                                                   !(is.na(GERD_REST_VALUE_FILLED))]$GERD_REST_VALUE_FILLED]
colnames(first_co_author)[c(23, 26, 21)]<-c("H_RS", "GERD_REST", "GERD")
cols_to_log <- c("H_RS", "GERD_REST", "GDP", "GERD")
first_co_author[, paste0("log_", cols_to_log) := lapply(.SD, log1p), 
              .SDcols = cols_to_log]
first_co_author$GERD_GDP<-first_co_author$GERD/first_co_author$GDP
val_authors.N<-first_co_author[,.(N=length(unique(doi))),
                                         by=list(global_ns, country_iso3, year,
                                                 H_RS, GERD_REST, GDP, GERD,GERD_GDP,
                                                 log_H_RS, log_GERD_REST, log_GDP, log_GERD)]


vars_to_check <- c("log_H_RS", "log_GERD_REST", 
                   "log_GERD", "log_GDP", "GERD_GDP")
corr_matrix <- cor(val_authors.N[, ..vars_to_check], use = "complete.obs")
colnames(corr_matrix)<-c("log(H_RS)", "log(GERD_I)",
                         "log(GERD_D)", "log(GDP)",
                         "GERD_D/GDP")
rownames(corr_matrix)<-c("log(H_RS)", "log(GERD_I)",
                         "log(GERD_D)", "log(GDP)",
                         "GERD_D/GDP")
corr_plot <- ggcorrplot(corr_matrix, 
                         hc.order = TRUE, 
                         type = "lower",
                         lab = TRUE, 
                         tl.cex = 9,
                         tl.srt = 0,
                         title = "Correlation Matrix of Predictors",
                         colors = c("#0072B2", "white", "#D55E00"))
corr_plot


val_authors.N[, period := ifelse(year < 2018, "Pre-2018", "Post-2018")]
val_authors.N[, period := factor(period, levels = c("Pre-2018", "Post-2018"))]


model_final <- glmmTMB(N ~  global_ns * period + 
                         (1 | country_iso3)+ (1 | year) , 
                       family = poisson(), 
                       data = val_authors.N[year>=2000])
summary(model_final)

setorderv(val_authors.N, c("global_ns", "country_iso3", "year"))

val_authors.N$GERD_GDP<-val_authors.N$GERD/val_authors.N$GDP*10e6


vif_model <- lm(N ~ global_ns + log_H_RS + log_GERD_REST + 
                  log_GDP + log_GERD +
                  GERD_GDP, data = val_authors.N)
vif_res <- vif(vif_model)

vif_df <- as.data.frame(vif_res)
setDT(vif_df, keep.rownames = "Variable")

if("GVIF^(1/(2*Df))" %in% names(vif_df)){
  setnames(vif_df, "GVIF^(1/(2*Df))", "Adjusted_VIF")
} else {
  vif_df[, Adjusted_VIF := vif_res]
}

print("VIF Summary Table:")
print(vif_df)

trend_colors <- c(
  "Increase" = "#D55E00", 
  "Decrease" = "#0072B2", 
  "Stable" = "#999999"
)

vif_plot <- ggplot(vif_df, aes(x = reorder(Variable, Adjusted_VIF), 
                               y = Adjusted_VIF)) +
  geom_bar(stat = "identity", fill = ifelse(vif_df$Adjusted_VIF > 5, 
                                            "#D55E00", "#0072B2"),
           width=0.6) +
  geom_hline(yintercept = 5, linetype = "dashed", color = "darkred") + # 
  geom_hline(yintercept = 10, linetype = "dotted", color = "darkred") + #
  coord_flip() +
  scale_x_discrete(labels=c("Regions", "log(GERD_I)","log(H_RS)",
                            "GERD_D/GDP", "log(GERD_D)", "log(GDP)"
                            ))+
  labs(title = "VIF Diagnostics for Predictors",
       #subtitle = "Bars in red exceed the threshold of 5",
       x = "Variables",
       y = "Adjusted VIF (or GVIF scale)") +
  theme_minimal()+
  theme(aspect.ratio = 0.5,
        axis.title.y=element_blank())

print(vif_plot)

p<-ggpubr::ggarrange(plotlist=list(corr_plot, vif_plot), 
                     align = "h", nrow=1, labels="AUTO")

p
ggsave(p, filename="../Figures/BIOGEOGRAPHY/GLM/Cor.VIF.pdf", width=10.5, height=5)

val_authors.N$log_GERD_GDP<-log1p(val_authors.N$GERD_GDP * 100)
val_authors.N[, year_centered := year - 2018]


model_overall <- glmmTMB(N ~  
                         log_H_RS + log_GERD_REST + log_GERD_GDP + 
                         (1 | country_iso3),
                       family = poisson(link = "log"), 
                       data = val_authors.N)

summary(model_overall)


model_trend <- glmmTMB(N ~ global_ns * year_centered + 
                         log_H_RS + log_GERD_REST + log_GERD_GDP + 
                         (1 | country_iso3),
                       family = poisson(link = "log"), 
                       data = val_authors.N)

summary(model_trend)

###useless.
simulationOutput <- simulateResiduals(fittedModel = model_overall)
plot(simulationOutput)
testDispersion(simulationOutput)

