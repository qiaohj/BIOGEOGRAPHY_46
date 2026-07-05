library(data.table)
library(ggplot2)
library(sf)
setwd("/path_to_your_project")
#Main Science and Technology Indicators 
#https://data-explorer.oecd.org/vis?df[ds]=dsDisseminateFinalDMZ&df[id]=DSD_MSTI%40DF_MSTI&df[ag]=OECD.STI.STP&vw=tb&dq=.A....&pd=%2C&to[TIME_PERIOD]=false
df<-fread("../Data/BIOGEOGRAPHY/MSTI.all.csv")
df[REF_AREA=="CHN"]
df$STRUCTURE<-NULL
df$STRUCTURE_ID<-NULL
df$STRUCTURE_NAME<-NULL
df$ACTION<-NULL
df$`Observation value`<-NULL
df$ACTION<-NULL
df$ACTION<-NULL
unique(df$Measure)
table(df$Measure)
df_N<-df[,.(N=.N), by=list(Measure)]

GERD<-df[Measure %in% c("Civil GBARD for Health and Environment programmes",
                        "Civil GBARD for Non-oriented Research programmes",
                        "Higher Education Expenditure on R&D (HERD)",
                        "Civil GBARD for General University Funds (GUF)",
                        "Basic research expenditure")]
table(GERD$`Unit of measure`)
table(GERD$Measure)

xxx<-GERD[TIME_PERIOD==2022 & REF_AREA=="USA"]
table(GERD$`Unit multiplier`)
setorderv(xxx, "Measure")
GERD<-GERD[`Unit of measure`=="US dollars, PPP converted"]
#GERD<-GERD[`Price base`=="Constant prices"]
GERD<-GERD[`Unit multiplier`=="Millions"]
table(GERD$`Price base`)
table(GERD$`Unit of measure`)
table(GERD$Measure)
unique(GERD$REF_AREA)
CHN<-GERD[REF_AREA=="USA"]
setorderv(CHN, "TIME_PERIOD")
GERD[REF_AREA=="BRA"]
GERD<-GERD[, c("Unit of measure", "OBS_VALUE", "REF_AREA", "Unit multiplier", "TIME_PERIOD")]
colnames(GERD)<-c("GERD_Unit", "GERD_VALUE", "REF_AREA", "GERD_Unit_multiplier", "TIME_PERIOD")
GERD[is.na(GERD_VALUE), GERD_VALUE:=0]
GERD<-GERD[,.(GERD_VALUE=sum(GERD_VALUE, na.rm = T)), 
           by=c("GERD_Unit", "REF_AREA", "GERD_Unit_multiplier", 
                "TIME_PERIOD")]


GERD_REST<-df[Measure=="GERD financed by the rest of the world"]

GERD_REST<-GERD_REST[`Unit of measure`=="US dollars, PPP converted"]
GERD_REST<-GERD_REST[`Price base`=="Constant prices"]
setorderv(GERD_REST, "TIME_PERIOD")
GERD_REST[REF_AREA=="CHN"]
unique(GERD_REST$REF_AREA)
table(GERD_REST$`Unit of measure`)
GERD_REST<-GERD_REST[, c("Unit of measure", "OBS_VALUE", "REF_AREA", "Unit multiplier", "TIME_PERIOD")]
colnames(GERD_REST)<-c("GERD_REST_Unit", "GERD_REST_VALUE", "REF_AREA", "GERD_REST_Unit_multiplier", "TIME_PERIOD")


H_RS<-df[Measure=="Higher education sector researchers"]

table(H_RS$`Price base`)
H_RS<-H_RS[`Unit of measure`=="Full time equivalent unit"]
H_RS<-H_RS[Transformation=="Not applicable"]
setorderv(H_RS, "TIME_PERIOD")
H_RS[REF_AREA=="USA"]
unique(H_RS$REF_AREA)
table(H_RS$`Unit of measure`)
H_RS<-H_RS[, c("Unit of measure", "OBS_VALUE", "REF_AREA", "Unit multiplier", "TIME_PERIOD")]
colnames(H_RS)<-c("H_RS_Unit", "H_RS_VALUE", "REF_AREA", "H_RS_Unit_multiplier", "TIME_PERIOD")
GERD[REF_AREA=="USA"]
MSTI<-merge(GERD, H_RS, by=c("REF_AREA", "TIME_PERIOD"), all=T)
MSTI<-merge(MSTI, GERD_REST, by=c("REF_AREA", "TIME_PERIOD"), all=T)
#https://data.worldbank.org/indicator/NY.GDP.MKTP.CD
#"Data Source","World Development Indicators",

#"Last Updated Date","2026-01-28",


gdp<-fread("../Data/BIOGEOGRAPHY/API_NY.GDP.MKTP.CD_DS2_en_csv_v2_155/API_NY.GDP.MKTP.CD_DS2_en_csv_v2_155.csv",
           header=TRUE)

gdp_long <- melt(gdp,
                id.vars = c("Country Name", "Country Code"),
                measure.vars = patterns("^\\d{4}$"),
                variable.name = "year",
                value.name = "gdp",
                na.rm = FALSE)
colnames(gdp_long)<-c("country_name", "REF_AREA", "TIME_PERIOD", "GDP")
gdp_long$TIME_PERIOD<-as.numeric(as.character(gdp_long$TIME_PERIOD))
table(gdp_long$TIME_PERIOD)
MSTI<-merge(MSTI, gdp_long, by=c("REF_AREA", "TIME_PERIOD"), all=T)


MSTI[is.na(GERD_VALUE)]
MSTI[is.na(H_RS_VALUE)]
range(MSTI$TIME_PERIOD)
CHN<-MSTI[REF_AREA=="USA"]
ggplot(CHN[!is.na(H_RS_VALUE)])+geom_line(aes(x=TIME_PERIOD, y=H_RS_VALUE))+
  geom_line(data=CHN[!is.na(GERD_VALUE)], aes(x=TIME_PERIOD, y=GERD_VALUE), color="red")+
  geom_line(data=CHN[!is.na(GDP)], aes(x=TIME_PERIOD, y=GDP/1e8), color="blue")

ggplot(CHN)+geom_line(aes(x=TIME_PERIOD, y=GERD_VALUE))


min_year <- min(MSTI$TIME_PERIOD)
max_year <- max(MSTI$TIME_PERIOD)
full_grid <- CJ(REF_AREA = unique(MSTI$REF_AREA), 
                TIME_PERIOD = min_year:max_year)


interpolate_vals <- function(vals, years) {
  valid_idx <- !is.na(vals)
  
  if (sum(valid_idx) < 2) {
    return(vals)
  }
  filled <- approx(x = years[valid_idx], 
                   y = vals[valid_idx], 
                   xout = years, 
                   method = "linear", 
                   rule = 1)$y 
  return(filled)
}

calc_imputation <- function(vals, years) {
  valid_idx <- !is.na(vals)
  
  if (sum(valid_idx) < 2) {
    return(list(fit_val = vals, r2 = NA_real_, pval = NA_real_))
  }
  
  model <- tryCatch({
    lm(vals ~ years)
  }, error = function(e) NULL)
  
  if (is.null(model)) {
    return(list(fit_val = vals, r2 = NA_real_, pval = NA_real_))
  }
  mod_sum <- summary(model)
  r_sq <- mod_sum$r.squared
  p_val <- NA_real_
  if (!is.null(mod_sum$fstatistic)) {
    f <- mod_sum$fstatistic
    p_val <- pf(f[1], f[2], f[3], lower.tail = FALSE)
  }
  preds <- predict(model, newdata = data.frame(years = years))
  out_vals <- vals 
  out_vals[is.na(out_vals)] <- preds[is.na(out_vals)]
  
  out_vals[out_vals < 0] <- 0
  
  return(list(fit_val = out_vals, r2 = r_sq, pval = p_val))
}
MSTI_full <- MSTI[full_grid, on = .(REF_AREA, TIME_PERIOD)]

MSTI_full[, GERD_VALUE_FILLED := 
            interpolate_vals(GERD_VALUE, TIME_PERIOD), 
          by = REF_AREA]
MSTI_full[, GERD_Status := fcase(
  !is.na(GERD_VALUE), "Original",
  is.na(GERD_VALUE) & !is.na(GERD_VALUE_FILLED), "Filled",
  default = "Missing"
)]

MSTI_full[, GERD_REST_VALUE_FILLED := 
            interpolate_vals(GERD_REST_VALUE, TIME_PERIOD), 
          by = REF_AREA]
MSTI_full[, GERD_REST_Status := fcase(
  !is.na(GERD_REST_VALUE), "Original",
  is.na(GERD_REST_VALUE) & !is.na(GERD_REST_VALUE_FILLED), "Filled",
  default = "Missing"
)]

table(MSTI_full$GERD_REST_Status)
MSTI_full[, H_RS_VALUE_FILLED := 
            interpolate_vals(H_RS_VALUE, TIME_PERIOD), 
          by = REF_AREA]
MSTI_full[, H_RS_Status := fcase(
  !is.na(H_RS_VALUE), "Original",
  is.na(H_RS_VALUE) & !is.na(H_RS_VALUE_FILLED), "Filled",
  default = "Missing"
)]

MSTI_full[, GDP_FILLED := 
            interpolate_vals(GDP, TIME_PERIOD), 
          by = REF_AREA]
MSTI_full[, GDP_Status := fcase(
  !is.na(GDP), "Original",
  is.na(GDP) & !is.na(GDP_FILLED), "Filled",
  default = "Missing"
)]
table(MSTI_full$GDP_Status)

map_sf<-read_sf("../Data/BIOGEOGRAPHY/world.shp")
map_sf<-data.table(map_sf[, c("SOC", "global_gn")])
map_sf$geometry<-NULL
map_sf<-map_sf[!is.na(SOC)]
map_sf<-unique(map_sf)
colnames(MSTI_full)[1]<-"SOC"

MSTI_full<-merge(MSTI_full, map_sf, by="SOC", all.x=T)

countries<-c("USA", "AUS", "CHN", "GBR")

df<-MSTI_full[SOC %in% countries]
ggplot(df)+geom_line(aes(x=TIME_PERIOD, y=GERD_VALUE_FILLED), color="red")+
  facet_wrap(~SOC, nrow=2)

saveRDS(MSTI_full, "../Data/BIOGEOGRAPHY/MSTI.rda")
