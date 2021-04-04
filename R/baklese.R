
library(tidyverse)

z_subs <- read_csv("/Users/davidwestbrown/SpiderOak Hive/Dictionary Chapter/dictionary_data/zombadings_subs.csv")

bak_dict <- read_csv("/Users/davidwestbrown/SpiderOak Hive/Dictionary Chapter/dictionary_data/baklese_dictionary.csv")

bak_tks <- bak_dict$Baklese %>%
  str_split(",") %>%
  unlist() %>%
  data.frame(Baklese = .) %>%
  mutate(Baklese = str_remove(Baklese, "\\(.*?\\)")) %>%
  mutate(Baklese = str_remove(Baklese, "/.*?$")) %>%
  mutate(Baklese = str_replace_all(Baklese, "[[:punct:]]", " ")) %>%
  mutate(Baklese = str_squish(Baklese)) %>%
  mutate(Baklese = tolower(Baklese)) %>%
  mutate(n_tks = str_count(Baklese, " ") + 1) 

zomb_tks <- z_subs %>%
  tidytext::unnest_tokens(Baklese, Baklese) %>%
  count(Baklese, sort = TRUE) %>%
  filter(!is.na(Baklese))

df <- bak_tks %>%
  filter(n_tks == 1) %>%
  inner_join(zomb_tks)
