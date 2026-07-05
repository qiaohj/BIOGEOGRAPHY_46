library(data.table)
library(ggplot2)
library(forcats)
library(ggrepel)
library(readr)
library(stringr)
library(tm)
library(SnowballC)
library(topicmodels)
library(tidytext)
library(wordcloud)
library(Rtsne)
library(dplyr)
setwd("/path_to_your_project")

if (F){
  keywords<-readRDS("../Data/BIOGEOGRAPHY/keywords.rda")
  df<-list()
  for (d in unique(keywords$doi)){
    item<-keywords[doi==d]
    
    keyword<-str_c(item$keyword, collapse = ", ")
    title<-item[1]$title
    abstract<-item[1]$abstract
    
    item.df<-data.table(doi=item[1]$doi, journal=item[1]$journal.x,
                        year=item[1]$year, title=title, abstract=abstract,
                        keyword=keyword, 
                        full.text=str_c(c(title, abstract, keyword), collapse = "\n"))
    df[[d]]<-item.df
  }
  dt<-rbindlist(df)
  
  
  dt<-dt[title!="Issue Information"]
  dt<-dt[title!="Author Index"]
  dt<-dt[title!="An Attractive and Accessible Text"]
  dt<-dt[title!="Book Reviews"]
  dt<-dt[title!="Book reviews"]
  dt<-dt[title!="Cover Image"]
  dt<-dt[title!="Front Cover"]
  dt<-dt[title!="Cover page"]
  dt<-dt[title!="Conclusions"]
  dt<-dt[toupper(title)!="ERRATUM"]
  dt<-dt[title!="List of Referees"]
  dt<-dt[title!="Contents"]
  dt<-dt[!grepl("Contents of", title)]
  dt<-dt[!grepl("CORRIGENDUM", toupper(title))]
  dt<-dt[!grepl("EDITORIAL", toupper(title))]
  dt<-dt[!grepl("Correction to", title)]
  saveRDS(dt, "../Data/BIOGEOGRAPHY/full.text.rda")
  
}
full.text<-readRDS("../Data/BIOGEOGRAPHY/full.text.rda")
authors.df.full.gpd<-readRDS("../Data/BIOGEOGRAPHY/authors.fixed.rda")
#keywords<-keywords[doi %in% unique(authors.df.full.gpd$doi)]

#keywords$keyword<-toupper(keywords$keyword)
#write_file(str_c(unique(keywords$keyword), collapse = ", "),

# *** CRITICAL PARAMETER: The number of topics (k) to discover ***
# This is a parameter you should experiment with. A good starting point is 30.
full.text<-full.text[between(year, 2010, 2025)]
range<-"2010"
num_topics <- 10

for (num_topics in seq(10, 100, by=10)){
  print(num_topics)
  target<-sprintf("../Data/BIOGEOGRAPHY/LDA.Since.%s/lda_model_n_topics_%d.rda", range, num_topics)
  if (file.exists(target)){
    next()
  }
  # Create a data.table, which is ideal for efficient processing
  keywords_dt <- full.text[, c("doi", "full.text")]
  keywords_dt$doc_id<-as.character(seq_along(full.text$full.text))
  # Create a Corpus, the starting point for text processing with the 'tm' package
  corpus <- Corpus(VectorSource(keywords_dt$full.text))
  
  if (F){
    corpus_original <- corpus 
    corpus_after <- tm_map(corpus_original, content_transformer(tolower)) 
    ids_before <- names(corpus_original)
    ids_after <- names(corpus_after)
    dropped_documents <- setdiff(ids_before, ids_after)
    
    if (length(dropped_documents) > 0) {
      cat("documents were deleted:\n")
      print(dropped_documents)
    } else {
      cat("no document was deleted\n")
    }
  }
  
  # Begin the text cleaning pipeline
  corpus <- tm_map(corpus, content_transformer(tolower)) # 1. Convert to lowercase
  corpus <- tm_map(corpus, removeNumbers)                # 2. Remove numbers
  corpus <- tm_map(corpus, removePunctuation)            # 3. Remove punctuation
  corpus <- tm_map(corpus, removeWords, stopwords("english")) # 4. Remove common English stop words
  corpus <- tm_map(corpus, stripWhitespace)             # 5. Remove excess whitespace
  corpus <- tm_map(corpus, stemDocument)                 # 6. Perform stemming (very important!)
  # e.g., "modelling" -> "model", "species" -> "speci"
  
  # --- 3. Create Document-Term Matrix (DTM) ---
  
  # The DTM is the input for the LDA model: rows are documents (keyword phrases),
  # columns are terms (stems), and values are term frequencies.
  dtm <- DocumentTermMatrix(corpus)
  
  # Many terms that appear only once can add noise. We can remove very sparse terms.
  # For this large dataset, we will keep all terms for now, but you can uncomment
  # the line below to make the DTM smaller and potentially faster.
  # dtm <- removeSparseTerms(dtm, 0.995) # Keeps terms that appear in at least 0.5% of docs
  
  # The LDA model requires that every document (row) has at least one term.
  # Remove empty rows that might result from preprocessing.
  row_totals <- apply(dtm, 1, sum)
  dtm <- dtm[row_totals > 0, ]
  # Also, update our original keywords data.table to keep them in sync
  keywords_dt_filtered <- keywords_dt[doc_id %in% dtm$dimnames$Docs]
  keywords_dt[doc_id=="8833"]
  
  
  cat("Preprocessing complete. DTM dimensions:", nrow(dtm), "documents,", ncol(dtm), "terms.\n")
  
  # --- 4. Run the LDA Model ---
  
  cat("Running LDA model to find", num_topics, "topics... (This may take a few minutes)\n")
  
  # Set a random seed for reproducibility
  set.seed(12345)
  
  # Run the LDA algorithm
  lda_model <- LDA(dtm, k = num_topics, control = list(seed = 12345))
  saveRDS(lda_model, target)
  cat("LDA model training complete.\n")
  
  
  # --- 5. Interpret and Use the Results ---
  
  # === 5.1 Understand Topic Composition (Beta values) ===
  
  # Use tidytext's tidy() function to extract the per-topic-per-term probabilities (beta)
  topics_beta <- tidy(lda_model, matrix = "beta")
  setDT(topics_beta) # Convert to data.table for efficiency
  saveRDS(topics_beta, sprintf("../Data/BIOGEOGRAPHY/LDA.Since.%s/topics_beta_n_topics_%d.rda", 
                               range, num_topics))
  
  # Find the top 100 terms within each topic
  top_terms_per_topic <- topics_beta[, .SD[order(-beta)][1:100], by = topic]
  fwrite(top_terms_per_topic, sprintf("../Data/BIOGEOGRAPHY/LDA.Since.%s/topic.term_n_topics_%d.csv", 
                                      range, num_topics))
  
  
  # Extract the per-document-per-topic probabilities (gamma)
  documents_gamma <- tidy(lda_model, matrix = "gamma")
  setDT(documents_gamma) # Convert to data.table
  saveRDS(documents_gamma, sprintf("../Data/BIOGEOGRAPHY/LDA.Since.%s/documents_gamma_n_topics_%d.rda", 
                                   range, num_topics))
  
}
num_topics<-10

lda_model<-readRDS(sprintf("../Data/BIOGEOGRAPHY/LDA.Since.%s/lda_model_n_topics_%d.rda", range, num_topics))
doc_topic_matrix <- posterior(lda_model)$topics 
print(dim(doc_topic_matrix))

tsne_result <- Rtsne(
  doc_topic_matrix,
  dims = 2,
  perplexity = 10,
  max_iter = 500,
  check_duplicates = FALSE
)

tsne_df <- as.data.frame(tsne_result$Y)
colnames(tsne_df) <- c("X", "Y")

dominant_topic_index <- apply(doc_topic_matrix, 1, which.max)
tsne_df$Dominant_Topic <- factor(dominant_topic_index)

print(head(tsne_df))
full.text$X<-tsne_df$X
full.text$Y<-tsne_df$Y
keywords_dt <- full.text[, c("doi", "full.text", "X", "Y")]
keywords_dt$doc_id<-as.character(seq_along(full.text$full.text))

documents_gamma<-readRDS(sprintf("../Data/BIOGEOGRAPHY/LDA.Since.%s/documents_gamma_n_topics_%d.rda", 
                                 range, num_topics))
topics_beta<-readRDS(sprintf("../Data/BIOGEOGRAPHY/LDA.Since.%s/topics_beta_n_topics_%d.rda", 
                             range, num_topics))

documents_gamma.N<-documents_gamma[, .(N=.N), by=list(document)]
topics_beta.N<-topics_beta[,.(N=.N), by=list(topic)]

# For each document, find the topic with the highest gamma probability
top_topic_for_doc <- documents_gamma[, .SD[which.max(gamma)], by = document]
top_topic_for_doc[gamma==min(top_topic_for_doc$gamma)]
threshold<-min(top_topic_for_doc$gamma)
top_topic_for_doc[document=="1383"]

topic_for_doc<-documents_gamma[gamma>threshold]
# Merge the results with the original keywords
# The 'document' column format is 'doc_id', which we use for the join
results_dt <- keywords_dt[topic_for_doc, on = .(doc_id = document)]
setnames(results_dt, c("doi","original_keyword", "X", "Y", "doc_id", "topic", "probability"))

setorder(results_dt, topic, -probability)

final.result<-merge(results_dt, full.text, by=c("doi", "X", "Y"))

topics<-fread(sprintf("../Data/BIOGEOGRAPHY/LDA.Since.%s/topics_%d.csv", range, num_topics))
final.result<-merge(final.result, topics, by="topic")

saveRDS(final.result, sprintf("../Data/BIOGEOGRAPHY/LDA.Since.%s/final.topic.result_%d.rda", range, num_topics))
final.result.author<-merge(final.result, authors.df.full.gpd[is_corresponding_author==T],
                           by=c("doi", "journal", "year"))
final.result$title<-NULL
final.result$full.text<-NULL
final.result$Interpretation<-NULL
final.result$keyword<-NULL
final.result$original_keyword<-NULL
final.result$abstract<-NULL


r=1

dt.list<-list()
for (r in c(1:10)){
  dt_sampled <- final.result.author[, .SD[sample(.N, size = min(.N, 100))], by = list(country.group, year)]
  dt_sampled.N<-dt_sampled[,.(N=.N, rep=r), by=list(year, country.group, Topic_Name)]
  dt.list[[r]]<-dt_sampled.N
}
dt.all.author<-rbindlist(dt.list)


ggplot(dt.all.author, aes(x=Topic_Name, y=N, color=country.group))+
  geom_boxplot()+
  labs(y="Number of papers")+
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.background = element_rect(fill = "white", colour = NA),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
    panel.grid.major = element_line(colour = "grey92", linewidth = 0.5),
    panel.grid.minor = element_line(colour = "grey92", linewidth = 0.25),
    axis.line = element_blank(),
    plot.background = element_rect(fill = "white", colour = NA),
    axis.ticks = element_line(colour = "black")
  )

ggplot(final.result.author, aes(x=X, y=Y))+
  stat_density_2d_filled(
    aes(alpha = after_stat(level), fill = country.group),
    contour_var = "density",
    show.legend = FALSE # 隐藏密度的图例
  )+facet_wrap(~country.group)


ggplot(final.result, aes(x=X, y=Y, color=journal))+
  stat_density_2d_filled(
    aes(alpha = after_stat(level), fill = journal),
    contour_var = "density",
    show.legend = FALSE # 隐藏密度的图例
  )+facet_grid(year~journal)

final.result.N.year<-final.result[,.(N=.N), by=list(year, journal)]
r=1

dt.list<-list()
for (r in c(1:10)){
  dt_sampled <- final.result[, .SD[sample(.N, size = min(.N, 100))], by = list(journal, year)]
  dt_sampled.N<-dt_sampled[,.(N=.N, rep=r), by=list(year, journal, Topic_Name)]
  dt.list[[r]]<-dt_sampled.N
}
dt.all<-rbindlist(dt.list)


all.topic<-dt.all[,.(N=sum(N)), by=list(Topic_Name, rep)]
ggplot(dt.all, aes(x=Topic_Name, y=N, color=journal))+#geom_point()+
  geom_boxplot()+
  labs(y="Number of papers")+
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.background = element_rect(fill = "white", colour = NA),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
    panel.grid.major = element_line(colour = "grey92", linewidth = 0.5),
    panel.grid.minor = element_line(colour = "grey92", linewidth = 0.25),
    axis.line = element_blank(),
    plot.background = element_rect(fill = "white", colour = NA),
    axis.ticks = element_line(colour = "black")
  )

ggplot(all.topic, aes(x=Topic_Name, y=N))+geom_point()+geom_boxplot()+
  labs(y="Number of papers")+
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.background = element_rect(fill = "white", colour = NA),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
    panel.grid.major = element_line(colour = "grey92", linewidth = 0.5),
    panel.grid.minor = element_line(colour = "grey92", linewidth = 0.25),
    axis.line = element_blank(),
    plot.background = element_rect(fill = "white", colour = NA),
    axis.ticks = element_line(colour = "black")
  )


final.result.N<-final.result[,.(N=.N), by=list(year, journal, Topic_Name)]
ggplot(final.result.N)+geom_line(aes(x=year, y=N, color=Topic_Name))+
  facet_wrap(~journal, ncol=1, scale="free")+
  theme()
final.result.N.all<-final.result[,.(N=.N), by=list(journal,Topic_Name)]
ggplot(final.result)+geom_histogram(aes(x=Topic_Name), stat="count")+facet_wrap(~journal, ncol=1)

single.topic<-final.result[, .(N=.N), by=list(doi)]
min(final.result[doi %in% single.topic[N==1]$doi]$probability)
final.result[doc_id=="8833"]

final.result[doi=="ECOG.02849"]
wordcloud(words = final.result.N.all[journal=="JOURNAL OF BIOGEOGRAPHY"]$Topic_Name,
          freq = final.result.N.all[journal=="JOURNAL OF BIOGEOGRAPHY"]$N,
          min.freq = 10,          # Minimum frequency for a word to be plotted
          max.words = 100,       # Maximum number of words to be plotted
          random.order = FALSE,  # Plot words in decreasing frequency
          rot.per = 0.3,         # Proportion of words rotated 90 degrees
          colors = RColorBrewer::brewer.pal(8, "Dark2"),
          scale = c(3, 0.5))  
wordcloud(words = final.result.N.all[journal=="JOURNAL OF BIOGEOGRAPHY"]$Topic_Name,
          freq = final.result.N.all[journal=="JOURNAL OF BIOGEOGRAPHY"]$N,
          min.freq = 10,
          max.words = 100,
          random.order = FALSE,
          rot.per = 0.3,
          colors = RColorBrewer::brewer.pal(8, "Dark2"),
          scale = c(3, 0.5))

wordcloud(words = final.result.N.all[journal=="ECOGRAPHY"]$Topic_Name,
          freq = final.result.N.all[journal=="ECOGRAPHY"]$N,
          min.freq = 1,
          max.words = 100,
          random.order = FALSE,
          rot.per = 0.3,
          colors = RColorBrewer::brewer.pal(8, "Dark2"),
          scale = c(3, 0.5))
wordcloud(words = final.result.N.all[journal=="DIVERSITY AND DISTRIBUTIONS"]$Topic_Name,
          freq = final.result.N.all[journal=="DIVERSITY AND DISTRIBUTIONS"]$N,
          min.freq = 1,
          max.words = 100,
          random.order = FALSE,
          rot.per = 0.3,
          colors = RColorBrewer::brewer.pal(8, "Dark2"),
          scale = c(3, 0.5))
wordcloud(words = final.result.N.all[journal=="GLOBAL ECOLOGY AND BIOGEOGRAPHY"]$Topic_Name,
          freq = final.result.N.all[journal=="GLOBAL ECOLOGY AND BIOGEOGRAPHY"]$N,
          min.freq = 1,
          max.words = 100,
          random.order = FALSE,
          rot.per = 0.3,
          colors = RColorBrewer::brewer.pal(8, "Dark2"),
          scale = c(3, 0.5))

