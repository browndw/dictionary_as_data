
library(tidyverse)

johnson_files <- list.files("/Users/davidwestbrown/SpiderOak Hive/Dictionary Chapter/dictionary_data/johnson",
                         pattern = "*.html", full.names = T)

cited_johnson <- lapply(johnson_files, extract_names)

cited_johnson <- bind_rows(cited_johnson)

bible_books <- read_csv("/Users/davidwestbrown/SpiderOak Hive/Dictionary Chapter/dictionary_data/bible_list.csv")

cited_johnson <- cited_johnson %>% mutate(author = ifelse(!is.na(bible_books$source[match(cited_johnson$author, bible_books$book)]),
                                                bible_books$source[match(cited_johnson$author, bible_books$book)], author))   

cited_johnson <- cited_johnson %>%
  count(author) %>%
  arrange(-n) %>%
  rownames_to_column("rank_johnson") %>%
  rename(n_johnson = n)

cited_all <- full_join(cited_johnson, cited_websters, by = "author") %>%
  select(author, n_johnson, n_websters, rank_johnson, rank_websters) %>%
  mutate(rank_johnson = as.numeric(rank_johnson)) %>%
  mutate(rank_websters = as.numeric(rank_websters))

write_csv(cited_all, "/Users/davidwestbrown/Downloads/cited_authors.csv")

396/.09


extract_names <- function(x){
  html <- xml2::read_html(x)
  names <- rvest::html_text(rvest::html_nodes(html, 'span')) %>%
    data.frame(author = .) %>%
    filter(str_detect(author, "[a-z]") ==T) %>%
    mutate(author = str_squish(author)) %>%
    filter(str_detect(author, "^[A-Z][a-z][a-z]+|[A-Z]'[A-Z][a-z][a-z]+") ==T) %>%
    mutate(author = ifelse(str_detect(author, "^Shak"), "Shakespeare", author)) %>%
    mutate(author = ifelse(str_detect(author, "peare$"), "Shakespeare", author)) %>%
    mutate(author = ifelse(str_detect(author, "^Lear$"), "Shakespeare", author)) %>%
    mutate(author = ifelse(str_detect(author, "^Othello$"), "Shakespeare", author)) %>%
    mutate(author = ifelse(str_detect(author, "^Tempest$"), "Shakespeare", author)) %>%
    mutate(author = ifelse(str_detect(author, "^Cymbeline$"), "Shakespeare", author)) %>%
    mutate(author = ifelse(str_detect(author, "^Cymb$"), "Shakespeare", author)) %>%
    mutate(author = ifelse(str_detect(author, "^Macbeth$"), "Shakespeare", author)) %>%
    mutate(author = ifelse(str_detect(author, "^Gen$"), "Genesis", author)) %>%
    mutate(author = ifelse(str_detect(author, "^Gen\\.$"), "Genesis", author)) %>%
    mutate(author = ifelse(str_detect(author, "^Deut$"), "Deuteronomy", author)) %>%
    mutate(author = ifelse(str_detect(author, "^Ezek$"), "Ezekiel", author)) %>%filter(author != "Sir") %>%
    mutate(author = ifelse(str_detect(author, "^Exod$"), "Exodus", author)) %>%filter(author != "Sir") %>%
    mutate(author = ifelse(str_detect(author, "^Isa$"), "Isaiah", author)) %>%filter(author != "Sir") %>%
    mutate(author = ifelse(str_detect(author, "^Psalm$"), "Psalms", author)) %>%filter(author != "Sir") %>%
    mutate(author = ifelse(str_detect(author, "^Jer$"), "Jeremiah", author)) %>%filter(author != "Sir") %>%
    mutate(author = ifelse(str_detect(author, "^Eccl$"), "Ecclesiastes", author)) %>%filter(author != "Sir") %>%
    mutate(author = ifelse(str_detect(author, "^Lev$"), "Leviticus", author)) %>%filter(author != "Sir") %>%
    mutate(author = ifelse(str_detect(author, "^Revel$"), "Revelations", author)) %>%filter(author != "Sir") %>%
    mutate(author = ifelse(str_detect(author, "^Chron$"), "Chronicles", author)) %>%filter(author != "Sir") %>%
    mutate(author = ifelse(str_detect(author, "^Cor$"), "Corinthians", author)) %>%filter(author != "Sir") %>%
    mutate(author = ifelse(str_detect(author, "^Millon$"), "Milton", author)) %>%
    mutate(author = ifelse(str_detect(author, "^Milion$"), "Milton", author)) %>%
    mutate(author = ifelse(str_detect(author, "^Swifi$"), "Swift", author)) %>%
    filter(author != "Err") %>%
    filter(author != "Hist") %>%
    filter(author != "Ess") %>%
    filter(author != "Serm") %>%
    filter(author != "Sat.") %>%
    filter(author != "Gov.") %>%
    filter(author != "Rev.") %>%
    filter(author != "Dec.") %>%
    filter(author != "The") 
  return(names)
}


