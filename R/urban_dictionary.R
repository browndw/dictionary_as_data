
library(tidyverse)
library(tidytext)
library(lubridate)


fem_activity <- read_csv("/Users/davidwestbrown/SpiderOak Hive/Dictionary Chapter/dictionary_data/ud_feminist.csv")

p1 <- ggplot(fem_activity, aes(x = Date, y = Activity)) +
  geom_col(fill = "gray50") +
  annotate("segment", x = as.Date("2017-9-15"), xend = as.Date("2017-9-15"), y = 0, yend = 160,
           linetype="dotted", colour = "gray40", size = .25) +
  annotate("text", x = as.Date("2017-5-01"), y = 130, size = 2, hjust = 1, vjust = 1,
           label = "The publication of a\n NYT's article documenting\nHarvey Weinstein's history of sexual assault\n(October 2017)") +
  annotate("text", x = as.Date("2017-8-01"), y = 131, size = 3, hjust = 1, vjust = 1,
           label = "→") +
  annotate("segment", x = as.Date("2016-12-15"), xend = as.Date("2016-12-15"), y = 0, yend = 100,
           linetype="dotted", colour = "gray40", size = .25) +
  annotate("text", x = as.Date("2016-8-01"), y = 68, size = 2, hjust = 1, vjust = 1,
           label = "The 2017 U.S. Presidential inaguaration\nand Women's March\n(January 2017)") +
  annotate("text", x = as.Date("2016-11-10"), y = 50, size = 3, hjust = 1, vjust = 0,
           label = "→") +
  annotate("segment", x = as.Date("2016-10-15"), xend = as.Date("2016-10-15"), y = 0, yend = 100,
           linetype="dotted", colour = "gray40", size = .25) +
  annotate("text", x = as.Date("2016-6-01"), y = 40, size = 2, hjust = 1, vjust = 1,
           label = "The 2016 U.S. Presidential election\n(November 2016)") +
  annotate("text", x = as.Date("2016-9-01"), y = 30, size = 3, hjust = 1, vjust = 0,
           label = "→") +
  xlab("") + 
  ylab("Normalized Activity") +
  theme_classic() +
  theme(axis.text.y = element_blank()) +
  theme(axis.ticks.y = element_blank())

ggsave("/Users/davidwestbrown/Downloads/feminist_ud.png", plot=p1, width=6.5, height=3.5, dpi=300)


ud_df <- read_csv("/Users/davidwestbrown/SpiderOak Hive/Dictionary Chapter/dictionary_data/urbandict-word-defs.csv")

ud_df <- ud_df %>%
  mutate(word = tolower(word)) %>%
  mutate(word = str_squish(word))

ud_df <- ud_df %>%
  filter(word == "feminist") %>%
  mutate(down_votes = ifelse(down_votes == 0, 1, down_votes))


df <- ud_df %>%
  select(word_id, definition) %>%
  unnest_tokens(word, definition)

word_counts <- df %>%
  filter(!word %in% stop_words$word) %>%
  group_by(word_id) %>%
  summarize(words = n())

def_sent <- df %>%
  inner_join(get_sentiments("bing")) %>%
  filter(!is.na(sentiment)) %>% 
  group_by(word_id) %>% 
  count(sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n) %>%
  mutate(negative = replace_na(negative, 0)) %>%
  mutate(positive = replace_na(positive, 0)) %>%
  left_join(word_counts) %>%
  mutate(neg_ratio = (negative / words)*-1) %>%
  mutate(pos_ratio = positive / words) %>%
  mutate(score = pos_ratio + neg_ratio)

def_sent <- df %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(word_id) %>%
  summarize(value = sum(value)) %>%
  right_join(ud_df) %>%
  mutate(value = replace_na(value, 0)) %>%
  mutate(volume = abs(value)) %>%
  left_join(word_counts) %>%
  mutate(sent_rate = value / words) %>%
  mutate(vol_rate = volume / words) %>%
  dplyr::select(value, volume, sent_rate, vol_rate, up_votes:down_votes, everything())
  
def_sent %>% count(value > 0) %>% mutate(per = n/sum(n))

def_sent$definition[def_sent$value == max(def_sent$value)]

def_sent <- df %>%
  inner_join(get_sentiments("nrc")) %>%
  group_by(word_id) %>%
  count(sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n) %>%
  right_join(ud_df) %>%
  replace(is.na(.), 0) %>%
  mutate(volume = up_votes/down_votes) %>%
  dplyr::select(anger:surprise, up_votes:down_votes, ratio, everything())

cor.test(def_sent$anger, def_sent$negative)

M <- cor(def_sent[,1:6])

corrplot::corrplot(M, method = "circle")


feminist <- read_csv("/Users/davidwestbrown/SpiderOak Hive/Dictionary Chapter/dictionary_data/ud_feminist_definitions.csv")


df <- feminist %>% group_by(month=floor_date(date_posted, "quarter")) %>%
  count()

ggplot(df, aes(x = month, y = n)) +
  geom_col() +
  theme_classic()
