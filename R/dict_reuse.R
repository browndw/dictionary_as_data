
library(tidyverse)
library(rvest)
library(textreuse)


blount <- read_html("/Users/davidwestbrown/Downloads/blount_glossographia.xml") %>%
  html_node("text")

blount <- blount %>%
  html_nodes('p') %>%
  html_text() %>%
  data.frame(entries = .) %>%
  mutate(entries = stringi::stri_trans_general(entries, "Latin-ASCII")) %>%
  mutate(entries = str_squish(entries))


for (i in seq_along(blount)) {
  filename <- str_c("/Users/davidwestbrown/Downloads/blount_phillips/blount", "-", str_pad(i, 5, pad = "0"), ".txt")
  writeLines(blount[[i]], filename)
}

phillips <- read_html("/Users/davidwestbrown/Downloads/phillips_new_world.xml") %>%
  html_node("text")

phillips <- phillips %>%
  html_nodes('item') %>%
  html_text() %>%
  data.frame(entries = .) %>%
  mutate(entries = stringi::stri_trans_general(entries, "Latin-ASCII")) %>%
  mutate(entries = str_squish(entries))

phillips <- phillips[-c(1:17, 12251:12719), ]

for (i in seq_along(phillips)) {
  filename <- str_c("/Users/davidwestbrown/Downloads/blount_phillips/phillips", "-", str_pad(i, 5, pad = "0"), ".txt")
  writeLines(phillips[[i]], filename)
}

h <- 120
b <- 60
minhash <- minhash_generator(n = h, seed = 623)

entries <- TextReuseCorpus(dir = "/Users/davidwestbrown/Downloads/blount_phillips",
                            tokenizer = tokenize_ngrams, n = 5,
                            keep_tokens = FALSE,
                            minhash_func = minhash)

buckets <- lsh(entries, bands = b)

scores <- buckets %>%
  lsh_candidates() %>%
  lsh_compare(entries, jaccard_similarity)

char_idx <- toupper(letters[1:26])

entries_txt <- readtext::readtext("/Users/davidwestbrown/Downloads/blount_phillips")
entries_txt <- entries_txt %>%
  mutate(doc_id = str_remove(doc_id, ".txt"))

phillips_cribbed <- scores %>%
  mutate(author_a = str_extract(a, "[a-z]+")) %>%
  mutate(author_b = str_extract(b, "[a-z]+")) %>%
  filter(author_a != author_b) %>%
  mutate(id = str_remove(b, "[a-z]+-")) %>%
  mutate(id = as.integer(id)) %>%
  group_by(id) %>%
  summarize(score = max(score), id = unique(id), a = unique(a), b = unique(b)) %>%
  left_join(entries_txt, by = c("a" = "doc_id")) %>%
  left_join(entries_txt, by = c("b" = "doc_id"))

seq_phillips <- data.frame(b = seq(1:length(phillips)), 
                           first_char = substring(phillips, 1, 1)) %>%
  mutate(first_char = toupper(first_char)) %>%
  mutate(in_seq = ifelse(lag(first_char) == first_char | lead(first_char) == first_char, 
         T, F)) %>%
  mutate(first_char = ifelse(in_seq == F & lag(in_seq) == T & lead(in_seq) == T, 
                         lead(first_char), first_char))



df <- seq_phillips %>%
  mutate(idx = match(first_char, char_idx)) %>%
  mutate(test = ifelse(abs(lead(idx) - idx) == 1, 
                             T, F))

sum(df$test == T)

seq_phillips <- seq_phillips %>%
  mutate(first_char = ifelse(lag(first_char) != first_char & lead(first_char) != first_char, 
                             lead(first_char), first_char))

for_plotting <- scores %>%
  mutate(author_a = str_extract(a, "[a-z]+")) %>%
  mutate(author_b = str_extract(b, "[a-z]+")) %>%
  filter(author_a != author_b) %>%
  mutate(b = str_remove(b, "[a-z]+-")) %>%
  mutate(b = as.integer(b)) %>%
  group_by(b) %>%
  summarize(score = max(score), b = unique(b)) %>%
  right_join(for_plotting) %>%
  arrange(b) %>%
  dplyr::mutate(score = replace_na(score, 0)) %>%
  mutate(from_blount = ifelse(score > .1, T, F)) %>%
  mutate(X = rep(seq(1:100), length.out=nrow(df))) %>%
  group_by(X) %>% mutate(Y = sequence(n())) %>% ungroup


p1 <- df %>% 
  # select(section = borrower_section, n) %>% 
  ggplot(aes(x = X, y = -Y, fill = from_blount)) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("lightgrey", "tomato")) +
  theme_void() +
  theme(legend.position = "none")
ggsave("/Users/davidwestbrown/Downloads/blount_in_phillips.png", plot=p1, width=6.5, height=3.5, dpi=300)


