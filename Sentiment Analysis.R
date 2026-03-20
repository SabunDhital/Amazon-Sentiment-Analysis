# ============================================================================
# DESCRIPTIVE ANALYSIS - Amazon Kindle Reviews
# Sabun Dhital - University of South Dakota
# Using tidytext + tm approaches
# ============================================================================

# STEP 1: LOAD PACKAGES
library(tidytext)
library(dplyr)
library(ggplot2)
library(stringr)
library(wordcloud)
library(RColorBrewer)
library(tm)
library(readr)
library(syuzhet)
library(textdata)
library(reshape2)

# STEP 2: LOAD DATA
data <- read_csv(
  "C:/Users/lenovo/Desktop/Fall 2025/all_kindle_review.csv",
  show_col_types = FALSE
)

cat("Data loaded successfully!\n")
cat("Total rows:", nrow(data), "\n")
cat("Total columns:", ncol(data), "\n\n")

# ============================================================================
# ROW FREQUENCIES - Rating Distribution
# ============================================================================
cat(rep("=", 60), "\n")
cat("ROW FREQUENCIES - Rating Distribution\n")
cat(rep("=", 60), "\n\n")

rating_table <- table(data$rating)
rating_percent <- prop.table(rating_table) * 100

cat("Total Records (Reviews):", nrow(data), "\n\n")
cat("Rating Distribution:\n\n")
for (i in names(rating_table)) {
  cat(i, "star:", rating_table[i], "reviews (",
      round(rating_percent[i], 1), "%)\n")
}
cat("\n")

# ============================================================================
# COLUMN FREQUENCIES (TOKENIZATION - TIDYTEXT)
# ============================================================================
cat(rep("=", 60), "\n")
cat("COLUMN FREQUENCIES - Token Analysis (tidytext)\n")
cat(rep("=", 60), "\n\n")

data <- data %>%
  mutate(review_id = row_number())

tokens_words <- data %>%
  select(review_id, asin, rating, reviewText) %>%
  unnest_tokens(word, reviewText)

cat("Total word tokens (with stop words):", nrow(tokens_words), "\n")
cat("Unique words (with stop words):", n_distinct(tokens_words$word), "\n\n")

data("stop_words")

tokens_clean <- tokens_words %>%
  anti_join(stop_words, by = "word")

cat("Total tokens (stop words removed):", nrow(tokens_clean), "\n")
cat("Unique tokens (stop words removed):", n_distinct(tokens_clean$word), "\n\n")

word_freq <- tokens_clean %>%
  count(word, sort = TRUE)

# TOP 20 TOKENS
cat(rep("=", 60), "\n")
cat("TOP 20 TOKEN FREQUENCIES\n")
cat(rep("=", 60), "\n\n")

top_20 <- head(word_freq, 20)

cat("Rank | Token          | Frequency\n")
cat("-----|----------------|----------\n")
for (i in 1:nrow(top_20)) {
  cat(sprintf("%4d | %-14s | %9d\n", i, top_20$word[i], top_20$n[i]))
}
cat("\n")

# ============================================================================
# DESCRIPTIVE STATISTICS
# ============================================================================
cat(rep("=", 60), "\n")
cat("DESCRIPTIVE STATISTICS\n")
cat(rep("=", 60), "\n\n")

review_stats <- data %>%
  mutate(
    char_count     = nchar(reviewText),
    word_count     = str_count(reviewText, "\\S+"),
    sentence_count = str_count(reviewText, "[.!?]+")
  )

summary_stats <- review_stats %>%
  summarise(
    total_reviews = n(),
    mean_words    = round(mean(word_count, na.rm = TRUE), 2),
    median_words  = median(word_count, na.rm = TRUE),
    min_words     = min(word_count, na.rm = TRUE),
    max_words     = max(word_count, na.rm = TRUE),
    sd_words      = round(sd(word_count, na.rm = TRUE), 2)
  )

cat("Overall Review Statistics:\n")
print(summary_stats)
cat("\n")

stats_by_rating <- review_stats %>%
  group_by(rating) %>%
  summarise(
    count        = n(),
    mean_words   = round(mean(word_count, na.rm = TRUE), 2),
    median_words = median(word_count, na.rm = TRUE),
    .groups = "drop"
  )

cat("Statistics by Rating:\n")
print(stats_by_rating)
cat("\n")

tokens_by_rating <- tokens_clean %>%
  group_by(rating) %>%
  summarise(
    total_tokens  = n(),
    unique_tokens = n_distinct(word),
    .groups = "drop"
  )

cat("Token Statistics by Rating:\n")
print(tokens_by_rating)
cat("\n")

# ============================================================================
# VISUALIZATIONS (DISPLAY ONLY - NO SAVING)
# ============================================================================
cat("Generating visualizations...\n\n")

# Plot 1: Rating Distribution
ggplot(data, aes(x = factor(rating))) +
  geom_bar(fill = "steelblue", color = "black") +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5) +
  labs(
    title = "Distribution of Kindle Book Ratings",
    x = "Star Rating",
    y = "Number of Reviews"
  ) +
  theme_minimal()

# Plot 2: Top 20 Most Frequent Tokens
ggplot(top_20, aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(
    title = "Top 20 Most Frequent Tokens",
    x = "Token",
    y = "Frequency"
  ) +
  theme_minimal()

# Plot 3: Word Cloud
set.seed(123)
wordcloud(
  words = word_freq$word,
  freq = word_freq$n,
  min.freq = 50,
  max.words = 100,
  random.order = FALSE,
  colors = brewer.pal(8, "Dark2")
)

# Plot 4: Distribution of Tokens per Review
token_stats <- tokens_clean %>%
  group_by(review_id) %>%
  summarise(tokens_per_review = n(), .groups = "drop")

ggplot(token_stats, aes(x = tokens_per_review)) +
  geom_histogram(bins = 50, fill = "coral", color = "black") +
  labs(
    title = "Distribution of Tokens per Review",
    x = "Number of Tokens per Review",
    y = "Frequency"
  ) +
  theme_minimal()

# Plot 5: Word Count by Rating
ggplot(review_stats, aes(x = factor(rating), y = word_count)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  labs(
    title = "Review Length by Rating",
    x = "Star Rating",
    y = "Number of Words"
  ) +
  theme_minimal()

# Plot 6: Total Tokens by Rating
ggplot(tokens_by_rating, aes(x = factor(rating), y = total_tokens)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = total_tokens), vjust = -0.5) +
  labs(
    title = "Total Tokens by Rating",
    x = "Star Rating",
    y = "Total Tokens"
  ) +
  theme_minimal()

# ============================================================================
# SAVE RESULTS TO CSV
# ============================================================================
rating_df <- data.frame(
  Rating     = names(rating_table),
  Count      = as.numeric(rating_table),
  Percentage = round(as.numeric(rating_percent), 2)
)
write.csv(rating_df, "rating_frequencies.csv", row.names = FALSE)
cat("Saved: rating_frequencies.csv\n")

token_df <- data.frame(
  Rank = 1:50,
  Token = head(word_freq$word, 50),
  Frequency = head(word_freq$n, 50)
)
write.csv(token_df, "top_50_tokens.csv", row.names = FALSE)
cat("Saved: top_50_tokens.csv\n")

write.csv(summary_stats, "summary_statistics.csv", row.names = FALSE)
cat("Saved: summary_statistics.csv\n")

write.csv(stats_by_rating, "statistics_by_rating.csv", row.names = FALSE)
cat("Saved: statistics_by_rating.csv\n\n")

# ============================================================================
# FINAL SUMMARY
# ============================================================================
cat(rep("=", 60), "\n")
cat("ANALYSIS COMPLETE - TIDYTEXT PORTION\n")
cat(rep("=", 60), "\n\n")

cat("RUBRIC CHECKLIST:\n\n")
cat("DATA SOURCE: Amazon Kindle Reviews\n")
cat("ROW FREQUENCIES: Rating distribution computed and saved\n")
cat("COLUMN FREQUENCIES: Tokens, top 20, top 50 saved\n")
cat("TEXTUAL DATA: reviewText used, tidytext tokenization, stopwords removed\n")
cat("DOCUMENT COUNT: Required >= 30, Actual:", nrow(data), "\n\n")

# ============================================================================
# tm / VCorpus CLEANING
# ============================================================================
cat(rep("=", 60), "\n")
cat("tm / VCorpus CLEANING\n")
cat(rep("=", 60), "\n\n")

TextDoc <- VCorpus(VectorSource(data$reviewText))

toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
TextDoc <- tm_map(TextDoc, toSpace, "/")
TextDoc <- tm_map(TextDoc, toSpace, "@")
TextDoc <- tm_map(TextDoc, toSpace, "\\|")

TextDoc <- tm_map(TextDoc, content_transformer(tolower))
TextDoc <- tm_map(TextDoc, removeNumbers)
TextDoc <- tm_map(TextDoc, removePunctuation)
TextDoc <- tm_map(TextDoc, removeWords, stopwords("english"))
TextDoc <- tm_map(TextDoc, stripWhitespace)
TextDoc <- tm_map(TextDoc, stemDocument)

cat("tm VCorpus cleaning complete.\n")

# Term Document Matrix
tdm <- TermDocumentMatrix(TextDoc)
cat("TDM created. Dimensions:\n")
print(dim(tdm))

term_freq <- rowSums(as.matrix(tdm))
term_freq <- sort(term_freq, decreasing = TRUE)

freq_df <- data.frame(
  word = names(term_freq),
  freq = term_freq,
  row.names = NULL
)

top20_tm <- freq_df %>%
  slice_head(n = 20)

ggplot(top20_tm, aes(x = reorder(word, freq), y = freq)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 20 Most Frequent Words",
    x = "Word",
    y = "Frequency"
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

# ============================================================================
# SENTIMENT ANALYSIS
# ============================================================================
bing_lex  <- get_sentiments("bing")
nrc_lex   <- get_sentiments("nrc")
afinn_lex <- get_sentiments("afinn")

syuzhet_vector <- get_sentiment(data$reviewText, method = "syuzhet")

cat("\nFirst few sentiment scores:\n")
print(head(syuzhet_vector))

cat("\nSummary of sentiment scores:\n")
print(summary(syuzhet_vector))

ggplot(data.frame(score = syuzhet_vector), aes(x = score)) +
  geom_histogram(bins = 40, fill = "steelblue", color = "black") +
  labs(
    title = "Distribution of Syuzhet Sentiment Scores",
    x = "Sentiment Score",
    y = "Frequency"
  ) +
  theme_classic()

x <- seq_along(syuzhet_vector)
syu_smooth <- loess(syuzhet_vector ~ x)$fitted

plot(
  x, syuzhet_vector,
  type = "l",
  col = "lightgray",
  lwd = 1,
  xlab = "Review Index",
  ylab = "Sentiment Score",
  main = "Emotional Arc Across Kindle Reviews"
)
lines(x, syu_smooth, col = "steelblue", lwd = 3)
abline(h = 0, col = "gray", lty = 2)

# ============================================================================
# WORD ASSOCIATIONS
# ============================================================================
TextDoc_dtm <- DocumentTermMatrix(TextDoc)

print_associations <- function(dtm, target_word, corlimit = 0.25) {
  assoc <- findAssocs(dtm, terms = target_word, corlimit = corlimit)
  
  if (length(assoc[[target_word]]) == 0) {
    cat("\nNo associations found for:", target_word, "\n")
    return(NULL)
  }
  
  assoc_df <- data.frame(
    term = names(assoc[[target_word]]),
    correlation = as.numeric(assoc[[target_word]]),
    row.names = NULL
  )
  
  cat("\n=============================================\n")
  cat("WORD ASSOCIATIONS FOR:", toupper(target_word), "\n")
  cat("Correlation limit:", corlimit, "\n")
  cat("=============================================\n\n")
  
  print(assoc_df)
}

print_associations(TextDoc_dtm, "book", 0.25)
print_associations(TextDoc_dtm, "read", 0.25)
print_associations(TextDoc_dtm, "stori", 0.25)

# ============================================================================
# WORD CORRELATION MATRIX
# ============================================================================
selected_words <- c("book", "read", "stori", "charact", "like",
                    "first", "just", "seri", "author", "dont", "one")

dtm_matrix <- as.matrix(TextDoc_dtm)
selected_words <- selected_words[selected_words %in% colnames(dtm_matrix)]

sub_mat <- dtm_matrix[, selected_words, drop = FALSE]
cor_mat <- cor(sub_mat)

cat("\nWord Correlation Matrix:\n")
print(round(cor_mat, 3))

cor_mat2 <- cor_mat
diag(cor_mat2) <- NA

heat_df <- melt(cor_mat2, na.rm = TRUE)

ggplot(heat_df, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "#d7191c",
    mid = "white",
    high = "#2c7bb6",
    midpoint = 0.2,
    limits = c(min(heat_df$value), max(heat_df$value)),
    name = "Correlation"
  ) +
  geom_text(aes(label = sprintf("%.2f", value)), size = 3, color = "black") +
  labs(
    title = "Word Correlation",
    x = "",
    y = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

