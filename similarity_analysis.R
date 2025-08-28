
setwd('')


# library(readxl)
 data_file <- read_excel("TransBack_translations_text_analysis.xlsx")
# View(translations_text_analysis)


####JACARD SIMILARITY Google VS original####
jaccard_similarity <- function(a, b) {
  a <- unique(a)
  b <- unique(b)
  intersection <- length(intersect(a, b))
  union <- length(union(a, b))
  return(intersection / union)
}

n <- nrow(data_file)
data_file$jaccard_similarity_P01 <- 0

for (i in 1:n) {
  official <- strsplit(tolower(data_file$offical_translation[i]), " ")[[1]]
  P_01 <- strsplit(tolower(data_file$P01[i]), " ")[[1]]
  
  similarity <- jaccard_similarity(official, P_01)
  data_file$jaccard_similarity_P01[i] <- similarity
  msg <- paste("Status", i/3400*100, "%")
  # Print message to console
  print(msg)
}


##### for P01-P36, chatgpt

# --- Helpers ---------------------------------------------------------------

jaccard_similarity <- function(a, b) {
  a <- unique(a); b <- unique(b)
  inter <- length(intersect(a, b))
  uni   <- length(union(a, b))
  if (uni == 0) return(NA_real_)
  inter / uni
}

tokenize_simple <- function(txt) {
  if (is.na(txt) || !nzchar(txt)) return(character(0))
  txt <- tolower(txt)
  txt <- gsub("[^a-zA-ZäöüÄÖÜß\\s]", " ", txt)  # keep letters (incl. German) + spaces
  txt <- gsub("\\s+", " ", txt)
  txt <- trimws(txt)
  if (!nzchar(txt)) return(character(0))
  strsplit(txt, " ", fixed = TRUE)[[1]]
}

# --- Columns to compare ----------------------------------------------------
# Adjust the name of the official column if needed (your example had 'offical_translation')
OFFICIAL_COL <- "offical_translation"

p_cols <- c(
  sprintf("P%02d", 1:16),
  sprintf("P%02d", 21:36),
  "ChatGPT"
)

# Keep only those that actually exist in your data:
p_cols <- p_cols[p_cols %in% names(data_file)]

# --- Compute Jaccard per row, per translator column -----------------------

# Pre-tokenize the official text once (faster)
official_tokens <- lapply(data_file[[OFFICIAL_COL]], tokenize_simple)

# For each translator column, make a new Jaccard column
for (col in p_cols) {
  newcol <- paste0("jaccard_", col)
  trans_tokens <- lapply(data_file[[col]], tokenize_simple)
  data_file[[newcol]] <- mapply(
    function(tok_off, tok_trans) jaccard_similarity(tok_off, tok_trans),
    official_tokens, trans_tokens,
    SIMPLIFY = TRUE
  )
}

# Optional: quick progress print
message(sprintf("Computed Jaccard for %d columns: %s",
                length(p_cols), paste(p_cols, collapse = ", ")))



#### select jaccard items
jaccard_data <- data_file %>% select(post_edit, order_item, item, 
                                     scale, format, page,    
                                     starts_with("jaccard"))

library(reshape2)
jaccard_long <- melt(jaccard_data, id.vars = c("post_edit","order_item",
                                               "item", "scale", "format", 
                                               "page"), measure.vars = c("jaccard_P01", "jaccard_P02", "jaccard_P03", "jaccard_P04", 
                                                                         "jaccard_P05","jaccard_P06", "jaccard_P07", "jaccard_P08", 
                                                                         "jaccard_P09","jaccard_P10", "jaccard_P11", "jaccard_P12", 
                                                                         "jaccard_P13","jaccard_P14", "jaccard_P15", "jaccard_P16", 
                                                                         "jaccard_P21","jaccard_P22", "jaccard_P23", "jaccard_P24", 
                                                                         "jaccard_P25","jaccard_P26", "jaccard_P27", "jaccard_P28", 
                                                                         "jaccard_P29","jaccard_P30", "jaccard_P31", "jaccard_P32", 
                                                                         "jaccard_P33","jaccard_P34", "jaccard_P35", "jaccard_P36", 
                                                                         "jaccard_ChatGPT"))

jacc_P01<- jaccard_long %>% filter(variable=='jaccard_P01')

a <- ggplot(jacc_P01, aes(variable, value)) +
  geom_boxplot(colour = "grey50") +
  geom_point(aes(colour = format), alpha = .4, position = position_jitter(width = .30, height = 0), size = 1.6, stroke = 0)+
  theme_minimal()
 

jacc_ChatGPT<- jaccard_long %>% filter(variable=='jaccard_ChatGPT')

b <- ggplot(jacc_ChatGPT, aes(variable, value)) +
  geom_boxplot(colour = "grey50") +
  geom_point(aes(colour = format), alpha = .4, position = position_jitter(width = .30, height = 0), size = 1.6, stroke = 0)+
  theme_minimal()

library(patchwork)
a + b

ggplot(jaccard_long, aes(variable, value)) +
  geom_boxplot(colour = "grey50") +
  geom_point(aes(colour = format), alpha = .4, position = position_jitter(width = .12, height = 0), size = 1.6, stroke = 0) 


jacc_item<- jaccard_long %>% filter(format=='item')

ggplot(jacc_item, aes(variable, value)) +
  geom_boxplot(colour = "grey50") +
  geom_point(, alpha = .4, position = position_jitter(width = .12, height = 0), size = 1.6, stroke = 0) 


jacc_scale<- jaccard_long %>% filter(format=='scale')

ggplot(jacc_scale, aes(variable, value)) +
  geom_boxplot(colour = "grey50") +
  geom_point(, alpha = .4, position = position_jitter(width = .12, height = 0), size = 1.6, stroke = 0) 


#####Levenshtein distance ####
library(stringdist)

n <- nrow(data_file)
data_file$levenshtein_distance_P01 <- 0

for (i in 1:n) {
  original <- data_file$offical_translation[i]
  P_01 <- data_file$P01[i]
  
  distance <- stringdist(original, P_01, method = "lv")
  similarity <- 1 - (distance / (nchar(original) + nchar(P_01)))
  
  data_file$levenshtein_distance_P01[i] <- similarity
  msg <- paste("Status", i/3400*100, "%")
  # Print message to console
  print(msg)
}



library(stringdist)

# Define official column (check spelling, you wrote "offical_translation" earlier)
OFFICIAL_COL <- "offical_translation"

# List of translation columns you want to compare
p_cols <- c(
  sprintf("P%02d", 1:16),
  sprintf("P%02d", 21:36),
  "ChatGPT"
)

# Keep only those that actually exist in your data
p_cols <- p_cols[p_cols %in% names(data_file)]

# Helper function: normalized Levenshtein similarity
lev_sim <- function(a, b) {
  if (is.na(a) || is.na(b)) return(NA_real_)
  d <- stringdist(a, b, method = "lv")
  denom <- nchar(a) + nchar(b)
  if (denom == 0) return(NA_real_)  # both empty
  1 - d / denom
}

# Compute for each translation column
for (col in p_cols) {
  newcol <- paste0("levenshtein_", col)
  data_file[[newcol]] <- mapply(
    lev_sim,
    data_file[[OFFICIAL_COL]],
    data_file[[col]],
    SIMPLIFY = TRUE
  )
  message("Finished ", col)
}



#### select levenshtein items
levenshtein_data <- data_file %>% select(post_edit, order_item, item, 
                                     scale, format, page,    
                                     starts_with("levenshtein"))













