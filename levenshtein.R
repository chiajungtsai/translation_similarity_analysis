library(readxl)
library(reshape2)
library(tidyverse)


data_file <- read_excel("TransBack_translations_text_analysis.xlsx")



#####Levenshtein distance ####
library(stringdist)

n <- nrow(data_file)
data_file$levenshtein_P01 <- 0

for (i in 1:n) {
  original <- data_file$offical_translation[i]
  P_01 <- data_file$P01[i]
  
  distance <- stringdist(original, P_01, method = "lv")
  similarity <- 1 - (distance / (nchar(original) + nchar(P_01)))
  
  data_file$levenshtein_P01[i] <- similarity
  msg <- paste("Status", i/3400*100, "%")
  # Print message to console
  print(msg)
}



# Define official column (check spelling, you wrote "offical_translation" earlier)
OFFICIAL_COL <- "offical_translation"

# List of translation columns you want to compare
p_cols <- c(
  sprintf("P%02d", 2:16),
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





levenshtein_long <- melt(levenshtein_data, id.vars = c("post_edit","order_item",
                                                       "item", "scale", "format", 
                                                       "page"), measure.vars = c("levenshtein_P01", "levenshtein_P02", "levenshtein_P03", "levenshtein_P04", 
                                                                                 "levenshtein_P05","levenshtein_P06", "levenshtein_P07", "levenshtein_P08", 
                                                                                 "levenshtein_P09","levenshtein_P10", "levenshtein_P11", "levenshtein_P12", 
                                                                                 "levenshtein_P13","levenshtein_P14", "levenshtein_P15", "levenshtein_P16", 
                                                                                 "levenshtein_P21","levenshtein_P22", "levenshtein_P23", "levenshtein_P24", 
                                                                                 "levenshtein_P25","levenshtein_P26", "levenshtein_P27", "levenshtein_P28", 
                                                                                 "levenshtein_P29","levenshtein_P30", "levenshtein_P31", "levenshtein_P32", 
                                                                                 "levenshtein_P33","levenshtein_P34", "levenshtein_P35", "levenshtein_P36", 
                                                                                 "levenshtein_ChatGPT"))





levenshtein_long <- levenshtein_long %>%
  group_by(variable) %>%
  mutate(mean_value = mean(value, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(variable = reorder(variable, mean_value))


ggplot(levenshtein_long, aes(x = variable, y = value, colour = format)) +
  # jittered raw points
  geom_point(alpha = 0.4,
             position = position_jitter(width = 0.3, height = 0),
             size = 1.6, stroke = 0) +
  # mean points
  stat_summary(fun = mean,
               geom = "point",
               size = 1,
               colour = "black",
               position = position_dodge(width = 0.3)) +
  # error bars: mean ± SD
  stat_summary(fun.data = mean_sdl,
               fun.args = list(mult = 1),
               geom = "errorbar",
               width = 0.2,
               colour = "black",
               position = position_dodge(width = 0.3))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))





levenshtein_item<- levenshtein_long %>% filter(format=='item')


# Reorder factor levels of variable by its mean (across all formats)
levenshtein_item <- levenshtein_item %>%
  group_by(variable) %>%
  mutate(mean_value = mean(value, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(variable = reorder(variable, mean_value))

ggplot(levenshtein_item, aes(x = variable, y = value, colour = format)) +
  # jittered raw points
  geom_point(alpha = 0.4,
             position = position_jitter(width = 0.3, height = 0),
             size = 1.6, stroke = 0) +
  # mean points
  stat_summary(fun = mean,
               geom = "point",
               size = 1,
               colour = "black",
               position = position_dodge(width = 0.3)) +
  # error bars: mean ± SD
  stat_summary(fun.data = mean_sdl,
               fun.args = list(mult = 1),
               geom = "errorbar",
               width = 0.2,
               colour = "black",
               position = position_dodge(width = 0.3))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))





levenshtein_item %>% 
     filter(is.na(value))
# A tibble: 9 × 9
post_edit order_item  item scale format page                    variable            value mean_value
<chr>          <dbl> <dbl> <dbl> <chr>  <chr>                   <fct>               <dbl>      <dbl>
  1 T1                 3     4     7 item   deprivation_sensitivity levenshtein_P04        NA      0.686
2 T1                 3     4     7 item   deprivation_sensitivity levenshtein_P07        NA      0.684
3 P2                 4     2     7 item   citizenship             levenshtein_P07        NA      0.684
4 T1                 9     1     3 item   police                  levenshtein_P08        NA      0.717
5 T1                 3     4     7 item   deprivation_sensitivity levenshtein_P22        NA      0.730
6 T1                 3     4     7 item   deprivation_sensitivity levenshtein_P28        NA      0.760
7 T1                 5     2     5 item   conscientiousness       levenshtein_P28        NA      0.760
8 T1                10     1    11 item   police_prevent_crime    levenshtein_P36        NA      0.749
9 T1                 8     1     5 item   doctor                  levenshtein_ChatGPT    NA      0.746

# really some text missing in the translation





levenshtein_scale<- levenshtein_long %>% filter(format=='scale')


# Reorder factor levels of variable by its mean (across all formats)
levenshtein_scale <- levenshtein_scale %>%
  group_by(variable) %>%
  mutate(mean_value = mean(value, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(variable = reorder(variable, mean_value))

ggplot(levenshtein_scale, aes(x = variable, y = value, colour = format)) +
  # jittered raw points
  geom_point(alpha = 0.4,
             position = position_jitter(width = 0.3, height = 0),
             size = 1.6, stroke = 0) +
  # mean points
  stat_summary(fun = mean,
               geom = "point",
               size = 1,
               colour = "black",
               position = position_dodge(width = 0.3)) +
  # error bars: mean ± SD
  stat_summary(fun.data = mean_sdl,
               fun.args = list(mult = 1),
               geom = "errorbar",
               width = 0.2,
               colour = "black",
               position = position_dodge(width = 0.3))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))





