library(data.table)
library(ggplot2)
library(zoo)
library(segmented)
setwd("/path_to_your_project")
articles<-readRDS("../Data/BIOGEOGRAPHY/articles.rda")
authors<-readRDS("../Data/BIOGEOGRAPHY/authors.rda")
authors[country=="Taiwan", country_iso3:="CNH"]
authors[country=="Taiwan", country:="China"]
authors$type<-NULL
authors$gdp.type<-NULL
authors$gdp.score<-NULL
authors$country<-NULL
authors$country_name<-NULL

global.south<-fread("../Data/BIOGEOGRAPHY/global.south.csv")

authors$global_sn<-"Global South"
authors[!country_iso3 %in% global.south$ISO3, global_sn:="Global North"]
table(authors$global_sn)


authors$is_leader<-F
authors[is_corresponding_author==T | is_first_author==T | is_co_first_author==T, is_leader:=T]

ggplot(authors)+geom_bar(aes(x=global_sn, fill=is_leader))

authors[country_iso3 %in% c("CHN", "BRA", "ZAF"), global_sn:="Global South (*)"]

table(authors$global_sn)

articles.N<-articles[,.(N=.N), by=list(journal, year)]


setorder(articles.N, journal, year)

is_valid_fit <- function(model, min_year, max_year) {
  if (is.null(model)) return(FALSE)
  
  # 获取估计的拐点
  tryCatch({
    bps <- model$psi[, "Est."]
    
    # 规则 1: 拐点必须在 [start + 2, end - 2] 范围内
    if (any(bps < (min_year + 5)) || any(bps > (max_year - 5))) return(FALSE)
    
    # 规则 2: 如果有多个拐点，它们之间的距离不能小于 2 年
    if (length(bps) > 1) {
      sorted_bps <- sort(bps)
      if (any(diff(sorted_bps) < 5)) return(FALSE)
    }
    
    return(TRUE)
  }, error = function(e) FALSE)
}

# ==============================================================================
# Analysis Function (Robust Version)
# ==============================================================================

analyze_max3_breaks <- function(d) {
  # 数据量太少直接返回 NULL
  if (nrow(d) < 10) return(NULL)
  
  min_y <- min(d$year)
  max_y <- max(d$year)
  
  # 控制参数：增加 n.boot 尝试更多随机起点，避免局部最优
  my_control <- seg.control(n.boot = 50, random = FALSE, display = FALSE) 
  
  # --- Step 1: Fit Candidate Models ---
  
  # Model 0: Linear
  fit_0 <- lm(N ~ year, data = d)
  bic_0 <- BIC(fit_0)
  
  # Model 1: 1 Breakpoint
  # 使用 suppressWarnings 避免 "outdistanced" 刷屏，我们通过 is_valid_fit 来处理结果
  fit_1 <- tryCatch({
    suppressWarnings(segmented(fit_0, seg.Z = ~year, npsi = 1, control = my_control))
  }, error = function(e) NULL)
  
  if (!is_valid_fit(fit_1, min_y, max_y)) fit_1 <- NULL
  bic_1 <- if (!is.null(fit_1)) BIC(fit_1) else Inf
  
  # Model 2: 2 Breakpoints
  fit_2 <- tryCatch({
    suppressWarnings(segmented(fit_0, seg.Z = ~year, npsi = 2, control = my_control))
  }, error = function(e) NULL)
  
  if (!is_valid_fit(fit_2, min_y, max_y)) fit_2 <- NULL
  bic_2 <- if (!is.null(fit_2)) BIC(fit_2) else Inf
  
  # --- Step 2: Select Best Model ---
  
  bics <- c(Linear = bic_0, Seg1 = bic_1, Seg2 = bic_2)
  best_model_name <- names(which.min(bics))
  
  # --- Step 3: Extract Statistics ---
  
  results_list <- list()
  
  if (best_model_name == "Linear") {
    coefs <- summary(fit_0)$coefficients
    results_list[[1]] <- data.table(
      Best_Model = "Linear (0 Breakpoints)",
      Segment_ID = 1L, 
      Start_Year = as.numeric(min_y),
      End_Year = as.numeric(max_y),
      Slope = as.numeric(coefs["year", "Estimate"]),
      P_Value = as.numeric(coefs["year", "Pr(>|t|)"])
    )
    
  } else {
    best_fit <- switch(best_model_name, "Seg1"=fit_1, "Seg2"=fit_2, "Seg3"=fit_3)
    
    bps <- sort(summary(best_fit)$psi[, "Est."])
    slopes <- slope(best_fit)$year
    boundaries <- c(min_y, bps, max_y)
    
    for (i in 1:nrow(slopes)) {
      s_est <- slopes[i, "Est."]
      t_val <- slopes[i, "t value"]
      df <- df.residual(best_fit)
      p_val <- 2 * pt(abs(t_val), df, lower.tail = FALSE)
      
      results_list[[i]] <- data.table(
        Best_Model = paste0("Segmented (", length(bps), " Breakpoints)"),
        Segment_ID = as.integer(i),
        Start_Year = as.numeric(round(boundaries[i], 1)),
        End_Year = as.numeric(round(boundaries[i+1], 1)),
        Slope = as.numeric(s_est),
        P_Value = as.numeric(p_val)
      )
    }
  }
  
  res <- rbindlist(results_list)
  res[, BIC_Score := as.numeric(min(bics))]
  return(res)
}

# ==============================================================================
# 3. Execution
# ==============================================================================

target<-articles.N
# Run analysis by group
final_results <- target[, analyze_max3_breaks(.SD), by = journal]

articles.N# Add descriptive labels
final_results[, Trend_Description := fcase(
  P_Value < 0.05 & Slope > 0, "Significant Rise",
  P_Value < 0.05 & Slope < 0, "Significant Fall",
  default = "Stable / No Sig. Trend"
)]

# Print numeric results
print(final_results)

# ==============================================================================
# 4. Visualization
# ==============================================================================

# We need to re-calculate predictions for plotting (including the 3-breakpoint model)
target[, Predicted_N := {
  fit_0 <- lm(N ~ year)
  ctrl <- seg.control(n.boot = 20, random=FALSE)
  
  fit_1 <- tryCatch(segmented(fit_0, seg.Z=~year, npsi=1, control=ctrl), error=function(e) NULL)
  fit_2 <- tryCatch(segmented(fit_0, seg.Z=~year, npsi=2, control=ctrl), error=function(e) NULL)
  
  bics <- c(BIC(fit_0), 
            if(!is.null(fit_1)) BIC(fit_1) else Inf, 
            if(!is.null(fit_2)) BIC(fit_2) else Inf)
  
  winner_idx <- which.min(bics)
  
  if(winner_idx == 1) predict(fit_0)
  else if(winner_idx == 2) predict(fit_1)
  else if(winner_idx == 3) predict(fit_2)
  else predict(fit_3)
}, by = journal]


# Plot
g <- ggplot(target, aes(x = year, y = N)) +
  geom_point(color = "gray60", alpha = 0.6) +
  geom_line(aes(y = Predicted_N), color = "darkblue", linewidth = 1) +
  facet_wrap(~journal, scales = "free") +
  theme_bw() +
  labs(title = "Trend Analysis (Up to 3 Breakpoints)",
       subtitle = "Model selection via BIC. Journal A should show 4 distinct segments.",
       y = "Number of Articles",
       x = "Year")

print(g)
