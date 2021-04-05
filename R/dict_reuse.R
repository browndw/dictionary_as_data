
library(tidyverse)
library(rvest)
library(textreuse)

# This code is an implementation of the textreuse package.
# The theory behind the identification of similarity and
# the specific use of hashing for efficient processing are
# described in detail here: 
# https://lincolnmullen.com/blog/an-introduction-to-the-textreuse-package-with-suggested-applications/
#
# The code below also relies heavily on the scripts written by
# Lincoln Mullen and Kellen Funk for their Spine of American Law project:
# https://github.com/lmullen/civil-procedure-codes

# set the parameters for hashing
h <- 120
b <- 60
minhash <- minhash_generator(n = h, seed = 623)

# read the entries into a corpus and hash
entries <- TextReuseCorpus(dir = "data/dictionary_data/blount_phillips/blount_phillips_entries",
                            tokenizer = tokenize_ngrams, n = 5,
                            keep_tokens = FALSE,
                            minhash_func = minhash)

# use locally sensitive hashing to efficiently identify potential matches
buckets <- lsh(entries, bands = b)

# calculate scores according to jaccard similarity
scores <- buckets %>%
  lsh_candidates() %>%
  lsh_compare(entries, jaccard_similarity)


# read in all the entries from both blount and phillips
entries_txt <- readtext::readtext("data/dictionary_data/blount_phillips/blount_phillips_entries")
entries_txt <- entries_txt %>%
  mutate(doc_id = str_remove(doc_id, ".txt"))

# make a dataframe showing potential matches side by side for visual inspection
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

# read in the phillips dictionary
phillips <- read_html("data/dictionary_data/blount_phillips/phillips_new_world.xml") %>%
  html_node("text")

# make a dataframe of all the entries
# we will use this to make a 'map' of the dictionary
phillips <- phillips %>%
  html_nodes('item') %>%
  html_text() %>%
  data.frame(entries = .) %>%
  mutate(entries = textclean::replace_html(entries)) %>%
  mutate(entries = str_remove_all(entries, "▪")) %>%
  mutate(entries = stringi::stri_trans_general(entries, "Latin-ASCII")) %>%
  mutate(entries = textclean::replace_non_ascii(entries)) %>%
  mutate(entries = str_squish(entries))

# remove header and footer rows
phillips <- phillips[-c(1:17, 12251:12719), ]

# make an index of the alphabet to convert letters to numbers
char_idx <- toupper(letters[1:26])

# make an indexed sequence af all the entries
seq_phillips <- data.frame(b = seq(1:length(phillips)), 
                           first_char = substring(phillips, 1, 1)) %>%
  mutate(first_char = toupper(first_char)) %>%
  mutate(in_seq = ifelse(lag(first_char) == first_char | lead(first_char) == first_char, 
         T, F)) %>%
  mutate(first_char = ifelse(in_seq == F & lag(in_seq) == T & lead(in_seq) == T, 
                         lead(first_char), first_char)) %>%
  mutate(idx = match(first_char, char_idx)) 

# approximate breaks between letters (a to b, b to c, etc.)
letter_breaks <- seq_phillips %>%
  mutate(letter_break = ifelse(lag(idx, default = F) - idx == -1 & lag(idx, n = 2L, default = F) - idx == -1, T, F)) %>%
  filter(letter_break == T) %>%
  group_by(first_char) %>%
  slice_head(n=1) %>%
  select(b, first_char) %>%
  rename(letter_break = first_char)

# combine breqks with the larger sequence 
seq_phillips <- left_join(seq_phillips, letter_breaks) %>%
  select(b, letter_break)

# create a dataframe for plotting
# note that we're filtering for entries with a similarity > .1
# also we're going to use the letter breaks we've identified to alternate shading
for_plotting <- scores %>%
  mutate(author_a = str_extract(a, "[a-z]+")) %>%
  mutate(author_b = str_extract(b, "[a-z]+")) %>%
  filter(author_a != author_b) %>%
  mutate(b = str_remove(b, "[a-z]+-")) %>%
  mutate(b = as.integer(b)) %>%
  group_by(b) %>%
  summarize(score = max(score), b = unique(b)) %>%
  right_join(seq_phillips) %>%
  arrange(b) %>%
  fill(letter_break) %>%
  mutate(letter_break = as.numeric(as.factor(letter_break))) %>%
  mutate(letter_break = as.logical(letter_break%%2)) %>%
  mutate(score = replace_na(score, 0)) %>%
  mutate(from_blount = ifelse(score > .1, T, F)) %>%
  mutate(fill_color = ifelse(from_blount == T, 1, 2)) %>%
  mutate(fill_color = ifelse(letter_break == T & from_blount == F, 3, fill_color)) %>%
  mutate(fill_color = as.factor(fill_color)) %>%
  mutate(X = rep(seq(1:100), length.out=nrow(df))) %>%
  group_by(X) %>% mutate(Y = sequence(n())) %>% ungroup

# calculate the percent of 'cribbed' entries from blount in phillips
sum(for_plotting$from_blount)/nrow(for_plotting)*100

# create the plot
for_plotting %>% 
  ggplot(aes(x = X, y = -Y, fill = fill_color)) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("black", "#d3d3d3", "#ededed")) +
  theme_void() +
  theme(legend.position = "none")

ggsave("/Users/user/Downloads/blount_in_phillips.png", plot=p1, width=6.5, height=3.5, dpi=300)


