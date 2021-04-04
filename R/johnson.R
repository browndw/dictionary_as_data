
library(tidyverse)

source("R/utils.R")

# Get the paths to the files containing Johnson's dictionary
johnson_files <- list.files("data/dictionary_data/johnson", pattern = "*.html", full.names = T)

# Extract the authors with quotations in the dictionary
cited_johnson <- lapply(johnson_files, extract_names)

# Comine into a single dataframe
cited_johnson <- bind_rows(cited_johnson)

# Read in a table containing all of the books in the Bible
# in order to count not as separate books but as Biblical references
bible_books <- read_csv("data/data_tables/bible_list.csv")

# Reformat Biblical references
cited_johnson <- cited_johnson %>% 
  mutate(author = ifelse(!is.na(bible_books$source[match(cited_johnson$author, bible_books$book)]),
                          bible_books$source[match(cited_johnson$author, bible_books$book)], author))   

# Tally cited authors
cited_johnson <- cited_johnson %>%
  count(author) %>%
  arrange(-n) %>%
  rownames_to_column("rank_johnson") %>%
  rename(n_johnson = n)

# Get the paths to the files containing Webster's dictionary
websters_files <- list.files("data/dictionary_data/websters_1913/xml_files", pattern = "_\\w.xml", full.names = T)

# Extract the authors with quotations in the dictionary
cited_websters <- lapply(websters_files, extract_authors)

# Format and tally citations
cited_websters <- unlist(cited_websters) %>% unname() %>% table() %>% as_tibble() %>% 
  rename_with(~ gsub(".", "author", .x, fixed = TRUE)) %>% 
  mutate(author = str_remove(author, ".$")) %>%
  mutate(author = str_remove(author, "^[[:punct:]]")) %>%
  mutate(author = str_squish(author)) %>%
  mutate(author = ifelse(str_detect(author, "\\. \\d+"), "bible", author)) %>%
  mutate(author = ifelse(str_detect(author, "^\\d"), "bible", author)) %>%
  mutate(author = ifelse(str_detect(author, "^shak"), "shakespeare", author)) %>%
  mutate(author = textclean::replace_html(author)) %>%
  group_by(author) %>%
  summarize(n = sum(n)) %>%
  mutate(author = str_to_title(author)) %>%
  mutate(author = ifelse(author == "L'estrange", "L'Estrange", author)) %>%
  mutate(author = ifelse(author == "Sir T. Browne", "Browne", author)) %>%
  mutate(author = ifelse(author == "Sir P. Sidney", "Sidney", author)) %>%
  arrange(-n) %>%
  rownames_to_column("rank_websters") %>%
  rename(n_websters = n)

# Combine counts into a single dataframe
cited_all <- full_join(cited_johnson, cited_websters, by = "author") %>%
  select(author, n_johnson, n_websters, rank_johnson, rank_websters) %>%
  mutate(rank_johnson = as.numeric(rank_johnson)) %>%
  mutate(rank_websters = as.numeric(rank_websters))


