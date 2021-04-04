

library(tidyverse)
library(rvest)

h_j <- read_html("/Users/davidwestbrown/SpiderOak Hive/Dictionary Chapter/dictionary_data/hobson_jobson.htm")


hj_entries <- h_j %>%
  html_node("entries")

hj_citations <- hj_entries %>%
  html_nodes(".citation")

xml_remove(hj_citations)

all_entries <- hj_entries %>%
  html_nodes('p') %>%
  html_text() %>%
  data.frame(entries = .) %>%
  mutate(entries = stringi::stri_trans_general(entries, "Latin-ASCII")) %>%
  mutate(entries = str_squish(entries)) %>%
  mutate(entries = str_remove(entries, "^\\[")) %>%
  filter(str_detect(entries, "^[A-Z]\\S+[A-Z]+|^[A-Z][A-Z]+|^[A-Z]\\.[A-Z]"))


servant_entries <- all_entries %>%
  filter(str_detect(entries, "servant"))

wilson_glossary <- read_html("/Users/davidwestbrown/Downloads/Glossary_of_Judicial_and_Revenue_Terms.html")

wilson_glossary <- wilson_glossary %>%
  html_nodes('div') %>%
  html_text() %>%
  data.frame(entries = .) 

