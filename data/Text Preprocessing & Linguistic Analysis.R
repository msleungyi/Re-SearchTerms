##############################################
## DATA PREPROCESSING & LINGUISTIC ANALYSIS ##
##############################################

## Load library ##
library(tidytext)
library(dplyr)
library(tm) # For removing stopwords
library(NLP)
library(stringr)
library(tidyverse)
library(text)
library(tibble)
library(plotly)
library(ggplot2)
library(ggrepel)
library(car)
library(Hmisc) # For correlation analysis
library(reticulate) # To interface with Python for Word2Vec 
library(stats) # For k-means clustering
library(mclust) # GMM clustering
library(cluster)
library(FactoMineR) # For PCA
library(umap) # For dimensionality reduction for word2vec
library(factoextra) # For visualising clustering
library(wordcloud) # For visualising the keywords per cluster
library(topicmodels)
library(ggwordcloud)
# install.packages("tm")
packageVersion("mclust")

## LOAD SAVED DATA ##
load("LinguisticAnalysis_AllResults.RData")

## Read data and preview ##
df_raw <- read.csv("forrt_cleaned.csv", header = TRUE)
head(df_raw)
colnames(df_raw)

## Assign unique ID for each definition
df <- df_raw %>%
  group_by(concept) %>%
  mutate(def_ID = paste0(concept, "_", row_number())) %>%
  ungroup()
head(df) # Preview

## Text preprocessing ##

# Define function
preprocess_text <- function(text, remove_stopwords = TRUE) {
  # Remove line breaks
  text <- str_replace_all(text, "[\r\n]", "")
  # Remove "Definition: " in FORRT terms
  text <- str_replace_all(text, "Definition: ", "")
  # Remove "Learn more in..." in IGI terms
  text <- str_replace_all(text, "Learn more in:.*", "")
  # Remove "et al" in all terms
  text <- str_replace_all(text, "et al", "")
  # Remove "e.g." in all terms
  text <- str_replace_all(text, "e.g.", "")
  # Remove citations in parentheses
  text <- str_replace_all(text, "\\([:alpha:]+(..)\\d{4}(..)", "")
  # Remove special characters and numbers
  text <- str_replace_all(text, "[^A-Za-z]+", " ")
  # Remove hyperlinks
  text <- str_replace_all(text, "http\\S+", "")
  
  if (remove_stopwords) {
    # 1. Tokenize
    tokens <- unlist(str_split(text, "\\s+"))
    # 2. Check if it is a stopword
    tokens <- tokens[!tolower(tokens) %in% stopwords("en")]
    # 3. Join tokens back together
    text <- paste(tokens, collapse = " ")
  }
  
  # Return text in lower case and strip off excess whitespaces
  text <- tolower(text)
  text <- str_squish(text)
  
  return(text)
}

# Apply function to clean "definition"
df <- df %>%
  mutate(def_clean = sapply(definition, preprocess_text, remove_stopwords = TRUE))

# Define a simpler function to clean the full definitions
preprocess_text_sim <- function(text, remove_stopwords = FALSE) {
  # Remove line breaks
  text <- str_replace_all(text, "[\r\n]", "")
  # Remove "Definition: " in FORRT terms
  text <- str_replace_all(text, "Definition: ", "")
  # # Remove "Learn more in..." in IGI terms
  # text <- str_replace_all(text, "Learn more in:.*", "")
  # # Remove "et al" in all terms
  # text <- str_replace_all(text, "et al", "")
  # # Remove "e.g." in all terms
  # text <- str_replace_all(text, "e.g.", "")
  # # Remove citations in parentheses
  # text <- str_replace_all(text, "\\([:alpha:]+(..)\\d{4}(..)", "")
  # Remove special characters and numbers
  text <- iconv(text, from = "UTF-8", to = "ASCII//TRANSLIT")
  # Remove hyperlinks
  text <- str_replace_all(text, "http\\S+", "")
  
  # Return text and strip off excess whitespaces
  # text <- str_squish(text)
  
  return(text)
}

# Apply function to clean "definition" for full display
df <- df %>%
  mutate(definition = sapply(definition, preprocess_text_sim, remove_stopwords = FALSE))

# Define a function to clean the terms
preprocess_text_term <- function(text, remove_stopwords = TRUE) {
  # Remove special characters and numbers
  text <- str_replace_all(text, "[^a-zA-Z0-9]+", " ")
  
  # Return text and strip off excess whitespaces
  text <- str_squish(text)
  
  return(text)
}

# Apply function to clean "term" for full display
df <- df %>%
  mutate(term = sapply(term, preprocess_text_sim, remove_stopwords = FALSE))

#########################################
## EMBEDDING-BASED SIMILARITY ANALYSIS ##
#########################################

## reticulate::py_config() # Identify the Python environment used by R, if needed
py_install("sentence_transformers")

## WORD2VEC vectorisation ##
# Load Python's sentence-transformers module
sentence_transformers <- import("sentence_transformers")

# Initialise the SBERT model
model <- sentence_transformers$SentenceTransformer('all-MiniLM-L6-v2')

# Embed the 'def_clean' column
df$embeddings <- lapply(df$def_clean, function(def) as.numeric(model$encode(def)))

# Define the function to compute cosine similarity
cosine_similarity <- function(v1, v2) {
  sum(v1 * v2) / (sqrt(sum(v1^2)) * sqrt(sum(v2^2)))
}

# Function to calculate pairwise cosine similarities and average similarity for each term
calculate_similarity_stats <- function(df) {
  # Embed all definitions
  embeddings <- lapply(df$def_clean, function(def) model$encode(def))
  
  # Create all pairwise combinations of def_ID
  combinations <- expand.grid(df$def_ID, df$def_ID, stringsAsFactors = FALSE) %>%
    filter(Var1 < Var2) %>%
    rename(def_ID1 = Var1, def_ID2 = Var2)
  
  # Calculate cosine similarities for each pair
  if (nrow(combinations) > 0) {
    combinations$cosine_similarity <- mapply(
      function(id1, id2) {
        idx1 <- which(df$def_ID == id1)
        idx2 <- which(df$def_ID == id2)
        cosine_similarity(embeddings[[idx1]], embeddings[[idx2]])
      },
      combinations$def_ID1,
      combinations$def_ID2
    )
    # Add concept name to combinations
    combinations <- combinations %>%
      mutate(concept = unique(df$concept))
    
    avg_similarity <- mean(combinations$cosine_similarity, na.rm = TRUE)
  } else {
    combinations <- tibble(def_ID1 = character(), def_ID2 = character(), cosine_similarity = numeric(), concept = character())
    avg_similarity <- NA  # Handle cases with no pairs
  }
  
  # Return pairwise similarities and average similarity
  list(
    pairwise = combinations,
    average = tibble(
      concept = unique(df$concept),
      avg_similarity = avg_similarity,
      total_definitions = nrow(df)
    )
  )
}

# Calculate pairwise similarities and average similarity for each concept
results <- df %>%
  group_by(concept) %>%
  group_map(~ calculate_similarity_stats(.x), .keep = TRUE)

# Combine results
pairwise_similarity <- bind_rows(lapply(results, `[[`, "pairwise"))
average_similarity_per_concept <- bind_rows(lapply(results, `[[`, "average"))

# Preview the results
head(pairwise_similarity)
head(average_similarity_per_concept)

#############################################
## CLUSTERING CONCEPTS BASED ON EMBEDDINGS ##
#############################################

# Convert list of embeddings to a matrix
embeddings_matrix <- do.call(rbind, df$embeddings)

# Combine with the original dataframe for aggregation
df <- cbind(df, embeddings_matrix)

# Compute the mean embedding for each concept
aggregated_embeddings <- df %>%
  group_by(concept) %>%
  summarise(across(matches("^[0-9]+$"), ~ mean(.x, na.rm = TRUE), .names = "mean_{col}")) %>%
  ungroup()

# Prepare matrix for clustering
embedding_matrix <- as.matrix(aggregated_embeddings %>% select(starts_with("mean")))

# Check if embedding matrix is normally distributed (suitable for GMM clustering?)
# Kolmogorov-Smirnov test
ks.test(embedding_matrix[, 1], "pnorm", 
        mean = mean(embedding_matrix[, 1]), 
        sd = sd(embedding_matrix[, 1]))

# QQ plot
qqnorm(embedding_matrix[, 1])
qqline(embedding_matrix[, 1], col = "red")

# Fit GMM to data for 1 to 10 components and multiple covariance structures to determine the most optimal model
gmm_result <- Mclust(embedding_matrix, G = 1:10, modelNames = c("EEE", "VII", "EEE"))

# Display summary of the GMM model
summary(gmm_result)

# Add cluster labels to the data
aggregated_embeddings <- aggregated_embeddings %>%
  mutate(cluster = as.factor(gmm_result$classification),  # Add cluster labels
         concept_name = concept)  # Keep term names for reference

# Add cluster membership probabilities to the data (soft clustering)
aggregated_embeddings$cluster_probability <- apply(gmm_result$z, 1, max)

# Tokenize term names and count word frequencies by cluster
tokenized_concepts <- aggregated_embeddings %>%
  unnest_tokens(word, concept_name) %>%  # Split concept names into individual words
  count(cluster, word, sort = TRUE) %>%  # Count word occurrences per cluster
  anti_join(stop_words, by = "word")     # Remove stopwords

# Visualise keywords as wordclouds
ggplot(tokenized_concepts, aes(label = word, size = n, color = as.factor(cluster))) +
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 10) +  # Reduce maximum size of words
  facet_wrap(~ cluster, scales = "free", ncol = 2) +
  theme_minimal() +
  theme(plot.margin = margin(20, 20, 20, 20))  # Add margins

# Assign labels to clusters
cluster_names <- c(
  "1" = "Research credit",
  "2" = "Replication and research challenges",
  "3" = "Statistical analysis and hypothesis testing",
  "4" = "Research design, validity, and theoretical frameworks",
  "5" = "Open science and collaborative practices",
  "6" = "Data sharing, reproducibility, and computational tools"
)

# Add cluster names to the data
aggregated_embeddings <- aggregated_embeddings %>%
  mutate(cluster_name = cluster_names[cluster])

# Plot the clustering result in 2D (using UMAP for dimension reduction)
umap_result <- umap(embedding_matrix, n_neighbors = 15, min_dist = 0.01)  # Perform UMAP reduction

# Extract UMAP embedding and convert to a dataframe
umap_df <- as.data.frame(umap_result$layout)  # Extract the 2D UMAP coordinates
colnames(umap_df) <- c("UMAP1", "UMAP2")  # Name the UMAP dimensions

# Add cluster and term labels
umap_df <- umap_df %>%
  mutate(
    cluster = aggregated_embeddings$cluster,
    concept_name = aggregated_embeddings$concept_name,
    cluster_name = cluster_names[cluster]
  )

## Export the clustering results for app visualisation
# write.csv(umap_df, "umap_df.csv")

# Plot with cluster names
ggplot(umap_df, aes(x = UMAP1, y = UMAP2, color = cluster_name, label = concept_name)) +
  geom_point(size = 3, alpha = 0.8) +  # Plot points
  geom_text_repel(hjust = 1, vjust = 1, size = 3, max.overlaps = 65) +  # Add non-overlapping labels
  labs(title = "GMM Clustering with Named Clusters",
       x = "UMAP Dimension 1", y = "UMAP Dimension 2", color = "Cluster") +
  theme_minimal() +
  theme(legend.position = "right")

# Store cluster memberships of terms
# Ensure aggregated_embeddings has cluster and concept columns
aggregated_embeddings <- aggregated_embeddings %>%
  mutate(cluster = as.factor(gmm_result$classification))  # Add cluster labels

# Add descriptive cluster names
aggregated_embeddings <- aggregated_embeddings %>%
  mutate(cluster_name = cluster_names[cluster])

## Export the embeddings for documentation
# write.csv(aggregated_embeddings, "by_term_embeddings.csv")

# Create a dataframe with term names, cluster IDs, and cluster names
concept_clusters <- aggregated_embeddings %>%
  select(concept, cluster, cluster_name)

# Merge the clustering results into the existing dataframe
by_concept_df <- average_similarity_per_concept %>%
  left_join(aggregated_embeddings %>% select(concept, cluster, cluster_name), by = "concept")

###########################
## TYPE & TOKEN ANALYSIS ##
###########################

# Aggregate all words in def_clean by concept (terms)
aggregated_words <- df %>%
  group_by(concept) %>%
  summarise(all_words = paste(def_clean, collapse = " ")) %>%  # Combine all definitions for each term
  ungroup()

# Tokenize and calculate types, tokens, and type-to-token ratio
type_token_stats <- aggregated_words %>%
  unnest_tokens(word, all_words) %>%  # Tokenize words
  group_by(concept) %>%
  summarise(
    tokens = n(),  # Count total words (tokens)
    types = n_distinct(word),  # Count unique words (types)
    type_to_token_ratio = types / tokens  # Calculate type-to-token ratio
  ) %>%
  ungroup()

# Merge with by_concept_df
by_concept_df <- by_concept_df %>%
  left_join(type_token_stats, by = "concept")

############################
## DESCRIPTIVE STATISTICS ##
############################

# Double-check the no. of concepts in the dataset
nrow(average_similarity_per_concept) # n = 261
mean(average_similarity_per_concept$total_definitions) # mean = 8.47
median(average_similarity_per_concept$total_definitions) # median = 2
min(average_similarity_per_concept$total_definitions) # min = 1
max(average_similarity_per_concept$total_definitions) # max = 266

# Count the number of terms per cluster
by_concept_df %>%
  group_by(cluster_name) %>%
  summarise(concept_count = n(), .groups = "drop")

# Remove terms that have only 1 definition
sim_concept_atleast2 <- by_concept_df %>%
  filter(!is.na(avg_similarity))

# Check how many terms with only 1 definition were removed
nrow(by_concept_df) - nrow(sim_concept_atleast2) # n = 129

# Check how many terms are left
nrow(sim_concept_atleast2) # n = 132

# 20 terms with the least similar definitions
least_sim_concept <- sim_concept_atleast2 %>%
  arrange(avg_similarity)
head(least_sim_concept, 20)

# 20 terms with the most similar definitions
most_sim_concept <- sim_concept_atleast2 %>%
  arrange(desc(avg_similarity))
head(most_sim_concept, 20)

# Wrap cluster names
by_concept_df <- by_concept_df %>%
  mutate(cluster_name_wrapped = str_wrap(cluster_name, width = 20))

## Comparing average definition similarity across clusters ##
# Plot with wrapped labels
ggplot(by_concept_df, aes(x = factor(cluster_name_wrapped), y = avg_similarity)) +
  geom_boxplot() +
  labs(title = "Distribution of Average Cosine Similarity by Cluster",
       x = "Cluster", y = "Average Pairwise Cosine Similarity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))  # Keep text horizontal

# Test for normality of avg_similarity within each cluster
by_concept_df %>%
  group_by(cluster) %>%
  summarise(p_value = shapiro.test(avg_similarity)$p.value)

# Levene's test for equal variances
leveneTest(avg_similarity ~ cluster, data = by_concept_df)

# Perform ANOVA to compare the average similarity across clusters
anova_result <- aov(avg_similarity ~ factor(cluster), data = by_concept_df)
summary(anova_result)

# Tukey's HSD test
tukey_result <- TukeyHSD(anova_result)
tukey_result

# Extract residual mean square from ANOVA
residual_mean_sq <- summary(anova_result)[[1]]["Residuals", "Mean Sq"]

# Extract group sizes
group_sizes <- table(by_concept_df$cluster)

# Convert Tukey results to a dataframe
tukey_df <- as.data.frame(tukey_result$`factor(cluster)`)

# Parse cluster pair names
tukey_df <- tukey_df %>%
  rownames_to_column("comparison") %>%
  mutate(cluster1 = as.numeric(sub("-.*", "", comparison)),  # Extract first cluster
         cluster2 = as.numeric(sub(".*-", "", comparison)))  # Extract second cluster

# Calculate the standard error for each comparison
tukey_df <- tukey_df %>%
  mutate(SE = sqrt(residual_mean_sq * (1 / group_sizes[as.character(cluster1)] +
                                         1 / group_sizes[as.character(cluster2)])))

# Add SE to the plot
ggplot(tukey_df, aes(x = comparison, y = diff)) +
  geom_point(size = 3) +  # Plot the mean difference
  geom_errorbar(aes(ymin = diff - SE, ymax = diff + SE), width = 0.2) +  # Add error bars
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Add a zero reference line
  geom_text(aes(label = significance, y = upr + 0.1), color = "blue", size = 5, hjust = 0) +  # Add significance stars
  coord_flip() +  # Flip coordinates for better readability
  labs(
    title = "Tukey's Pairwise Comparisons with Standard Errors",
    x = "Cluster Comparison",
    y = "Difference in Means of By-Concept Definition Similarity"
  ) +
  theme_minimal()

# Prepare Tukey's data for plotting
tukey_plot_data <- as.data.frame(tukey_result$`factor(cluster)`)
tukey_plot_data$comparison <- rownames(tukey_plot_data)

# Define cluster names (Shortened)
cluster_names_short <- c(
  "1" = "Research credit",
  "2" = "Open science",
  "3" = "Research challenges",
  "4" = "Theoretical frameworks",
  "5" = "Data sharing",
  "6" = "Statistical analysis"
)

# Map cluster names to comparison variable
tukey_plot_data <- tukey_plot_data %>%
  mutate(
    comparison = str_replace_all(comparison, cluster_names_short)  # Replace cluster numbers with names
  )

# Add significance levels based on p-values
tukey_plot_data <- tukey_plot_data %>%
  mutate(significance = case_when(
    `p adj` < 0.001 ~ "***",
    `p adj` < 0.01 ~ "**",
    `p adj` < 0.05 ~ "*",
    TRUE ~ "ns"  # Not significant
  ))

# Create the confidence interval plot
ggplot(tukey_plot_data, aes(x = comparison, y = diff)) +
  geom_point(size = 3) +  # Plot the mean difference
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.2) +  # Add confidence intervals
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Add a zero reference line
  geom_text(aes(label = significance, y = upr + 0.1), color = "blue", size = 5, hjust = 0) +  # Add significance stars
  coord_flip() +  # Flip coordinates for better readability
  labs(
    title = "Tukey's Pairwise Comparisons with Significance",
    x = "Cluster Comparison",
    y = "Difference in Means of Term Definition Similarity (95% CI)"
  ) +
  theme_minimal()

## Comparing no. of types across clusters ##
# Plot with wrapped labels
ggplot(by_concept_df, aes(x = factor(cluster_name_wrapped), y = types)) +
  geom_boxplot() +
  labs(title = "Distribution of No. of Types by Cluster",
       x = "Cluster", y = "No. of Types") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))  # Keep text horizontal

# Test for normality of avg_similarity within each cluster
by_concept_df %>%
  group_by(cluster) %>% 
  summarise(p_value = shapiro.test(types)$p.value)

# Levene's test for equal variances
leveneTest(types ~ cluster, data = by_concept_df) # Not normally distributed

# Perform K-W test to compare no. of types across clusters
kruskal_result <- kruskal.test(types ~ factor(cluster), data = by_concept_df)
print(kruskal_result)

# Pairwise comparisons with Wilcoxon test
pairwise.wilcox.test(by_concept_df$types,
                     by_concept_df$cluster,
                     p.adjust.method = "bonferroni",
                     exact = FALSE)

## Comparing type-to-token ratio across clusters ##

# Plot with wrapped labels
ggplot(by_concept_df, aes(x = factor(cluster_name_wrapped), y = type_to_token_ratio)) +
  geom_boxplot() +
  labs(title = "Distribution of Type-to-Token Ratio by Cluster",
       x = "Cluster", y = "Type-to-Token Ratio") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))  # Keep text horizontal

# Test for normality of avg_similarity within each cluster
by_concept_df %>%
  group_by(cluster) %>%
  summarise(p_value = shapiro.test(type_to_token_ratio)$p.value)

# Levene's test for equal variances
leveneTest(type_to_token_ratio ~ cluster, data = by_concept_df) # Not normally distributed

# Perform K-W test to compare no. of types across clusters
kruskal_result <- kruskal.test(type_to_token_ratio ~ factor(cluster), data = by_concept_df)
print(kruskal_result)

# Pairwise comparisons with Wilcoxon test
pairwise.wilcox.test(by_concept_df$type_to_token_ratio,
                     by_concept_df$cluster,
                     p.adjust.method = "bonferroni",
                     exact = FALSE)

## Correlation matrix between definition similarity, total no. of definitions, types, tokens, type-to-token ratio ##
# Select the relevant columns
by_concept_var <- by_concept_df[, c("avg_similarity", "total_definitions", "tokens", "types", "type_to_token_ratio")]

# Calculate Pearson correlation matrix
# Compute Pearson correlation matrix and p-values
pearson_result <- rcorr(as.matrix(by_concept_var), type = "pearson")

# Extract the correlation coefficients and p-values
pearson_corr <- pearson_result$r
pearson_p <- pearson_result$P

# Function to compute Kendall's Tau with p-values
kendall_results <- function(data) {
  vars <- colnames(data)
  n <- length(vars)
  corr_matrix <- matrix(NA, n, n)
  p_matrix <- matrix(NA, n, n)
  
  for (i in 1:n) {
    for (j in i:n) {
      if (i == j) {
        corr_matrix[i, j] <- 1
        p_matrix[i, j] <- NA
      } else {
        result <- cor.test(data[[i]], data[[j]], method = "kendall")
        corr_matrix[i, j] <- result$estimate
        corr_matrix[j, i] <- result$estimate
        p_matrix[i, j] <- result$p.value
        p_matrix[j, i] <- result$p.value
      }
    }
  }
  list(correlation = corr_matrix, p_values = p_matrix)
}

# Apply the function
kendall_result <- kendall_results(by_concept_var)

# Extract the correlation coefficients and p-values
kendall_corr <- kendall_result$correlation
kendall_p <- kendall_result$p_values

# Replace "A", "B", etc., with the actual variable names
colnames(kendall_corr) <- colnames(by_concept_var)
rownames(kendall_corr) <- colnames(by_concept_var)

# Re-convert to a long format
kendall_combined <- as.data.frame(as.table(round(kendall_corr, 3))) %>%
  rename(Var1 = Var1, Var2 = Var2, Kendall = Freq) %>%
  mutate(Kendall_P = as.vector(round(kendall_p, 3)))

# Replace "Var1" and "Var2" with actual variable names
kendall_combined$Var1 <- factor(kendall_combined$Var1, levels = colnames(by_concept_var))
kendall_combined$Var2 <- factor(kendall_combined$Var2, levels = colnames(by_concept_var))

# Print the updated Kendall results
print(kendall_combined)

# Prepare Pearson results
pearson_combined <- as.data.frame(as.table(round(pearson_corr, 3))) %>%
  rename(Var1 = Var1, Var2 = Var2, Pearson = Freq) %>%
  mutate(Pearson_P = as.vector(round(pearson_p, 3)))

# Merge results
correlation_results <- merge(pearson_combined, kendall_combined, by = c("Var1", "Var2"))

# View the results
print(correlation_results)

# Save outputs
write.csv(pairwise_similarity, "pairwise_cosine_similarity.csv", row.names = FALSE)
write.csv(by_concept_df, "df_byconcept.csv")
write.csv(df[,1:11], "df_bydefinition.csv")
write.csv(correlation_results, "by_concept_correlations.csv")

save.image(file = "LinguisticAnalysis_AllResults.RData")


