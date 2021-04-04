
library(tidyverse)

websters_files <- list.files("/Users/davidwestbrown/SpiderOak Hive/Dictionary Chapter/dictionary_data/gcide_xml-0.51/xml_files",
                         pattern = "_\\w.xml", full.names = T)

extract_headwords <- function(x){
  
  doc <- read_file(x)
  doc<- doc %>% str_squish() %>% str_replace_all("</p>\\s+<p>", "</p><p>")
  doc <- doc %>% str_split("</p><p>") %>% unlist()
  
  nodes <-  doc[grepl("<source>1913 Webster</source>", doc)]
  
  words <- lapply(nodes, function(x) str_extract_all(x, "<ent>.*?</ent>"))
  words <- words %>% unlist()
  words <- words[!is.na(words)]
  words <- words %>% str_remove_all("<.*?>") %>% tolower() %>% unique()
  
  return(words)
}

head_words <- lapply(websters_files, extract_headwords)
head_words <- unlist(head_words)
  

extract_authors <- function(x){
  
  doc <- read_file(x)
  doc<- doc %>% str_squish() %>% str_replace_all("</p>\\s+<p>", "</p><p>")
  doc <- doc %>% str_split("</p><p>") %>% unlist()
  
  nodes <-  doc[grepl("<source>1913 Webster</source>", doc)]
  
  authors <- lapply(nodes, function(x) str_extract_all(x, "<q?au>.*?</q?au>"))
  authors <- authors %>% unlist()
  authors <- authors[!is.na(authors)]
  authors <- authors %>% str_remove_all("<.*?>") %>% tolower()
  return(authors)
}

cited_websters <- lapply(websters_files, extract_authors)

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

write_csv(cited_websters, "/Users/davidwestbrown/Downloads/cited_websters.csv")

cited_authors <- unlist(cited_authors) %>% unname() %>% table() %>% as_tibble() %>% 
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
  mutate(per = (n/sum(n))*100) %>%
  arrange(-n)

sum(cited_authors$per)

extract_labels <- function(x){
  
  doc <- read_file(x)
  doc<- doc %>% str_squish() %>% str_replace_all("</p>\\s+<p>", "</p><p>")
  doc <- doc %>% str_split("</p><p>") %>% unlist()
  
  nodes <-  doc[grepl("<mark>", doc)]
  nodes <-  doc[grepl("<source>1913 Webster</source>", doc)]
  
  labels <- lapply(nodes, function(x) str_extract(x, "<mark>.*?</mark>"))
  labels <- labels %>% unlist()
  
  words <- lapply(nodes, function(x) str_extract(x, "<ent>.*?</ent>"))
  words <- words %>% unlist()
  
  df <- data.frame(words = words, labels = labels, stringsAsFactors = F)
  
  df <- df %>%
    filter(!is.na(words)) %>%
    mutate(labels = str_remove_all(labels, "<.*?>")) %>%
    mutate(words = str_remove_all(words, "<.*?>")) %>%
    mutate(labels = str_remove_all(labels, "\\[")) %>%
    mutate(labels = str_remove_all(labels, "\\]"))
  
  return(df)
}


usage_labels <- lapply(websters_files, extract_labels)
usage_labels <- bind_rows(usage_labels)

usage_labels <- usage_labels %>%
  mutate(labels = textclean::replace_html(labels)) %>%
  mutate(labels = tolower(labels)) %>%
  mutate(words = tolower(words))

usage_labels <- usage_labels %>%
  filter(!is.na(labels))

usage_labels %>% count(labels) %>%
  arrange(-n) %>% head(10)

usage_labels %>% filter(str_detect(labels, "obs\\.|r\\.|arch|rare")) %>% count()
usage_labels %>% filter(str_detect(labels, "slang|colloq|vulgar|local")) %>% count()
18543/833

usage_labels %>% filter(str_detect(labels, "illiterate"))

table(usage_labels$labels) %>% data.frame(stringsAsFactors = F) %>% as_tibble() %>% arrange(-Freq)

cited_authors <- unlist(cited_authors) %>% unname() %>% table() %>% as_tibble() %>% 
  rename_with(~ gsub(".", "author", .x, fixed = TRUE)) %>% 
  mutate(author = str_squish(author)) %>%
  mutate(author = str_remove(author, ".$")) %>%
  arrange(-n)

coha_files <- list.files("/Users/davidwestbrown/Documents/COHA/text_1900s_mse", full.names = T)

coha_txt <- readtext::readtext(coha_files[1:3]) %>% data.frame(stringsAsFactors = F)

coha_txt <- coha_txt %>% 
  mutate(text = tolower(text)) %>%
  mutate(text = str_replace_all(text, " n't ", "n't ")) %>%
  mutate(text = str_replace_all(text, " 'm ", "'m ")) %>%
  mutate(text = str_replace_all(text, " 've ", "'ve ")) %>%
  mutate(text = str_replace_all(text, " 're ", "'re ")) %>%
  mutate(text = str_replace_all(text, " 'll ", "'ll ")) %>%
  mutate(text = str_replace_all(text, " 'd ", "'d ")) %>%
  mutate(text = str_replace_all(text, " 's ", "'s ")) %>%
  mutate(text = str_replace_all(text, "s ' ", "s' ")) %>%
  mutate(text = str_replace_all(text, " ' ([a-z])", " '\\1")) %>%
  mutate(text = str_replace_all(text, "([a-z]) - ([a-z])", "\\1-\\2")) %>%
  mutate(text = str_squish(text))
  

coha_txt <- coha_txt %>%
  mutate(text = lemmatize_strings(text))

text_vector <- as.vector(paste(coha_txt$text, collapse = " "))

df <- data.frame(head_word = head_words,
  in_text = sapply(head_words, function(x) str_detect(text_vector, pattern = paste("\\b", x, "\\b"))),
  stringsAsFactors = F)

table(df$in_text)

coha_totals <- read_csv("/Users/davidwestbrown/SpiderOak Hive/Dictionary Chapter/dictionary_data/coha_totals.csv")
coha_counts <- read_csv("/Users/davidwestbrown/SpiderOak Hive/Dictionary Chapter/dictionary_data/coha_counts.csv")

hw_df <- data.frame(feature = head_words, stringsAsFactors = F)

counts_df <- coha_counts %>%
  filter(year < 1911) %>%
  mutate(feature = textstem::lemmatize_strings(feature)) %>%
  group_by(feature) %>%
  summarize(feature = unique(feature),
            frequency = sum(frequency))


df <- anti_join(counts_df, hw_df) %>%
  arrange(-frequency)

write_csv(df, "/Users/davidwestbrown/Downloads/coca_notin_websters.csv")

websters_text <- readLines("/Users/davidwestbrown/SpiderOak Hive/Dictionary Chapter/dictionary_data/websters_1913.txt") %>%
  data.frame(text = ., stringsAsFactors = F)

websters_text <- websters_text %>%
  mutate(leading_blank = lag(text) == "") %>%
  mutate(all_caps = str_detect(text, "[a-z]", negate = T))

websters_text <- websters_text %>%
  filter(text != "") %>%
  filter(leading_blank == T & all_caps == T)

websters_text <- websters_text %>%
  separate_rows(text, sep = "; ")

head_words <- websters_text$text %>%
  str_replace_all("[[:punct:]]", " ") %>%
  str_squish() %>%
  tolower() %>%
  unique()

websters_mwe <- head_words[str_count(head_words, " ") > 0]

coha_files <- list.files("/Users/davidwestbrown/Desktop/COHAClean", 
                         pattern = "*.txt", full.names = T)

coha_files <- coha_files[str_detect(coha_files, "/18\\d\\d_|/190\\d_")]

coha_txt <- readtext::readtext(coha_files) %>% data.frame(stringsAsFactors = F)

coha_txt <- coha_txt %>% 
  mutate(text = str_replace_all(text, "[[:punct:]]", " ")) %>%
  mutate(text = str_squish(text))

coha_corpus <- corpus(coha_txt)

coha_tkns <- quanteda::tokens(coha_corpus, what = "fastestword")
coha_tkns <- tokens_compound(coha_tkns, pattern = phrase(websters_mwe))

coha_dfm <- dfm(coha_tkns)
coha_freq <- textstat_frequency(coha_dfm)

sum(coha_freq$frequency)

head_words <- str_replace_all(head_words, " ", "_")

df <- data.frame(head_word = head_words,
                 in_text = head_words  %in% coha_freq$feature,
                 stringsAsFactors = F)


write_csv(df, "/Users/davidwestbrown/Downloads/websters_in_coha.csv")

table(df_2$in_coha_early)