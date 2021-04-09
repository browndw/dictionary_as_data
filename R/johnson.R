
library(tidyverse)

source("R/utils.R")

# Get the paths to the files containing Johnson's dictionary
johnson_files <- list.files("data/dictionary_data/johnson", pattern = "*.html", full.names = T)

# Extract the authors with quotations in the dictionary
cited_johnson <- johnson_cited(johnson_files[1])

# Clean the data a little
# Note this uses some VERY blunt tools: string distance & stemming
# in order to partially account for error and variation.
# The resulting counts are approximations.
cited_johnson <- clean_entries_johnson(cited_johnson) %>%
  rename(n_johnson = n)

# Compare to the counts reported by Willinsky
willinsky <- read_csv("data/data_tables/willinsky_counts.csv") %>%
  left_join(cited_johnson) %>%
  mutate(error = (n_johnson - n_willinsky)/n_willinsky*100)

# Calculate the mean error (assuming Willisky's counts are accurate)
mean(willinsky$error, na.rm=TRUE)

# Check the row for Bible citations
cited_johnson[which(cited_johnson$author == "Bible"), ]

# Calculate an adjusted value for Bible citations
bible_adj <- (cited_johnson[which(cited_johnson$author == "Bible"), 2]*(1 + abs(mean(willinsky$error, na.rm=TRUE)/100))) %>% round() %>% as.numeric()

# Append the Bible counts to Willinsky's
willinsky <- willinsky %>%
  select(author, n_willinsky) %>%
  add_row(author = "Bible", n_willinsky = bible_adj)

# Replace our counts with those from Willinsky
cited_johnson <- cited_johnson %>% 
  mutate(n_johnson = ifelse(!is.na(willinsky$n_willinsky[match(cited_johnson$author, willinsky$author)]),
                         willinsky$n_willinsky[match(cited_johnson$author, willinsky$author)], n_johnson))

cited_johnson <- cited_johnson %>% 
  filter(!str_detect(author, ("Dict|Ency|Cyc"))) %>%
  filter(n_johnson > 5) %>%
  rownames_to_column("rank_johnson")

# Get the paths to the files containing Webster's Unabridged dictionary
websters_files <- list.files("data/dictionary_data/websters/websters_1913/xml_files", pattern = "_\\w.xml", full.names = T)

# Extract the authors with quotations in the dictionary
cited_websters_1913 <- lapply(websters_files, websters_cited_xml)

# Tally the counts
cited_websters_1913 <- bind_rows(cited_websters_1913) %>%
  group_by(author) %>%
  summarize(n_websters_2 = sum(n)) %>%
  arrange(-n_websters_2) %>%
  filter(!str_detect(author, ("Dict|Ency|Cyc"))) %>%
  filter(n_websters_2 > 5) %>%
  rownames_to_column("rank_websters_2")

# Extract citations
websters_1844 <- read_csv("data/dictionary_data/websters/websters_1844.csv")

# Tally them
cited_websters_1844 <- websters_cited_df(websters_1844) %>%
  group_by(author) %>%
  tally() %>%
  arrange(-n) %>%
  filter(!str_detect(author, ("Dict|Ency|Cyc"))) %>%
  rename(n_websters_1 = n) %>%
  filter(n_websters_1 > 5) %>%
  rownames_to_column("rank_websters_1")

# Combine counts into a single dataframe
cited_all <- full_join(cited_johnson, cited_websters_1844, by = "author") %>%
  full_join(cited_websters_1913) %>%
  select(author, n_websters_1, n_websters_2, n_johnson, rank_websters_1, rank_websters_2, rank_johnson) %>%
  mutate(rank_johnson = as.numeric(rank_johnson)) %>%
  mutate(rank_websters_1 = as.numeric(rank_websters_1)) %>%
  mutate(rank_websters_2 = as.numeric(rank_websters_2)) %>%
  arrange(rank_websters_1)


