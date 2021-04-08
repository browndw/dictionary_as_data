
library(tidyverse)

source("R/utils.R")

# Get the paths to the files containing Johnson's dictionary
johnson_files <- list.files("data/dictionary_data/johnson", pattern = "*.html", full.names = T)

# Extract the authors with quotations in the dictionary
cited_johnson <- johnson_cited(johnson_files[1])

# Clean the data a little
cited_johnson <- clean_entries_johnson(cited_johnson) %>%
  rownames_to_column("rank_johnson") %>%
  rename(n_johnson = n)

# Compare to the counts reported by Willinsky
willinsky <- read_csv("data/data_tables/willinsky_counts.csv") %>%
  left_join(cited_johnson) %>%
  mutate(error = (n_johnson - n_willinsky)/n_willinsky*100) %>%
  filter(author != "Wallace")

# Calculate the mean error (assuming Willisky's counts are accurate)
mean(willinsky$error)

# Get the paths to the files containing Webster's Unabridged dictionary
websters_files <- list.files("data/dictionary_data/websters/websters_1913/xml_files", pattern = "_\\w.xml", full.names = T)

# Extract the authors with quotations in the dictionary
cited_websters_1913 <- lapply(websters_files, websters_cited)

cited_websters_1913 <- bind_rows(cited_websters_1913) %>%
  group_by(author) %>%
  summarize(n_websters_2 = sum(n)) %>%
  arrange(-n_websters_2) %>%
  rownames_to_column("rank_websters_2")

# Format and tally citations
websters_1844 <- read_csv("data/dictionary_data/websters/websters_1841.csv")

cited_websters_1844 <- str_extract(websters_1841$definition, "– [A-Z].*?\\.$") %>%
  str_split(" – ") %>%
  unlist() %>%
  str_subset("Dict.|Cyc.|Encyc.", negate = T) %>%
  str_split("(?<=[a-z-][a-z-]\\.) (?=[A-Z][a-z-])") %>%
  unlist() %>%
  data.frame(author = .) %>%
  mutate(author = str_remove(author, "– ")) %>%
  mutate(author = ifelse(str_detect(author, " [i|v|x|l|c|d|m]+(\\.|,)"), "Bible", author)) %>%
  mutate(author = str_remove(author, "'s .*?$")) %>%
  mutate(author = str_remove(author, "\\.$")) %>%
  mutate(author = ifelse(str_detect(author, "Shak"), "Shakespeare", author)) %>%
  mutate(author = ifelse(str_detect(author, "^Brown$"), "Browne", author)) %>%
  filter(str_count(author, " ") < 4) %>%
  group_by(author) %>%
  tally() %>%
  arrange(-n) %>%
  rename(n_websters_1 = n) %>%
  rownames_to_column("rank_websters_1")

# Combine counts into a single dataframe
cited_all <- full_join(cited_johnson, cited_websters_1844, by = "author") %>%
  full_join(cited_websters_1913) %>%
  select(author, n_websters_1, n_websters_2, n_johnson, rank_websters_1, rank_websters_2, rank_johnson) %>%
  mutate(rank_johnson = as.numeric(rank_johnson)) %>%
  mutate(rank_websters_1 = as.numeric(rank_websters_1)) %>%
  mutate(rank_websters_2 = as.numeric(rank_websters_2)) %>%
  arrange(rank_websters_1)


