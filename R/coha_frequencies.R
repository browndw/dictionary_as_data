
library(tidyverse)


coha_counts <- read_csv("/Users/davidwestbrown/Downloads/coha_counts.csv")
total_counts <- read_csv("/Users/davidwestbrown/Downloads/coha_totals.csv")


time_seq <- data.frame(decade = rep(seq(1810, 2000, 10), times = 6), 
                       feature = rep(c("feller", "golly", "gosh", "nope", "wow", "yep"), times = 1, each = 20))

df <- coha_counts %>%
  filter(feature == "yep" | feature == "nope" | feature == "gosh" | feature == "golly" | feature == "wow" | feature == "feller" | feature == "fellers") %>%
  mutate(feature = ifelse(feature == "fellers", "feller", feature)) %>%
  left_join(total_counts) %>%
  group_by(feature, year) %>%
  summarize(year = unique(year),
            feature = unique(feature),
            frequency = sum(frequency),
            total_tokens = sum(total_tokens)) %>%
  ungroup() %>%
  mutate(freq_norm = (frequency/total_tokens)*1000000)

p1 <- ggplot(df, aes(x=year, y=freq_norm, group = feature)) +
  geom_vline(xintercept = 1913, linetype = "dotted") +
  geom_point(shape = 21, size =.35, color = "black", alpha = .35) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), size=.25, alpha = .25, color = "gray20") +
  ylim(0, 25) +
  theme_linedraw() +
  labs(x="", y = "Frequency (per million words)") + 
  theme(legend.position = "none") +
  theme(panel.grid = element_blank()) +
  theme(axis.text = element_text(size = 6)) +
  facet_wrap( ~ feature, ncol=2) +
  theme(strip.text = element_text(size = 6.5, face = "bold", color = "black")) +
  theme(strip.background = element_rect(fill="gray80", colour="black"))


ggsave("/Users/davidwestbrown/Downloads/coha_frequencies.png", p1, width = 6.5, height = 4, dpi = 300)

df <- coha_counts %>%
  filter(feature == "yep" | feature == "nope" | feature == "gosh" | feature == "golly" | feature == "wow" | feature == "feller" | feature == "fellers") %>%
  mutate(feature = ifelse(feature == "fellers", "feller", feature)) %>%
  left_join(total_counts) %>%
  mutate(time_chunk = cut(year, seq(1809, 2009, 10), dig.lab = 5)) %>%
  mutate(time_chunk = str_remove_all(time_chunk, "\\(|\\]")) %>%
  separate(time_chunk, c("year_start", "year_end"), sep = ",") %>%
  mutate(year_start = as.numeric(year_start) + 1) %>%
  rename(decade = year_start) %>%
  group_by(feature, decade) %>%
  summarize(decade = unique(decade),
            feature = unique(feature),
            frequency = sum(frequency),
            total_tokens = sum(total_tokens)) %>%
  ungroup() %>%
  mutate(freq_norm = (frequency/total_tokens)*1000000) %>%
  mutate(group = ifelse(feature == "yep" | feature == "nope", "nope/yep", feature)) %>%
  mutate(group = ifelse(feature == "gosh" | feature == "golly", "golly/gosh", group)) %>%
  group_by(group) %>% 
  mutate(grouping_var = match(feature, unique(feature))) %>%
  mutate(grouping_var = as.factor(grouping_var))

p1 <- ggplot(df, aes(x=decade, y=freq_norm, group = feature)) +
  geom_vline(xintercept = 1905, linetype = "dotted", size = .25) +
  geom_bar(stat="identity", position = position_dodge(), width = 8, color = "black", size = .25, fill = "white") +
  theme_linedraw() +
  theme(legend.position = "none") +
  theme(panel.grid = element_blank()) +
  theme(axis.text = element_text(size = 6)) +
  facet_wrap( ~ feature, ncol=2) +
  theme(strip.text = element_text(size = 6.5, face = "bold", color = "black")) +
  theme(strip.background = element_rect(fill="gray80", colour="black"))

library(quanteda)

coha_txt <- readtext::readtext("/Users/davidwestbrown/Desktop/COHAClean")

year <- coha_txt %>%
  select(doc_id) %>%
  mutate(year = str_extract(doc_id, "^\\d{4}"))

coha_corpus <- corpus(coha_txt)
docvars(coha_corpus) <- year

coha_tks <- tokens(coha_corpus, what = "fastestword")
coha_dfm <- dfm(coha_tks, groups = "year")

coha_counts <- textstat_frequency(coha_dfm, groups = "year")

rm(coha_txt, coha_corpus, coha_tks, coha_dfm)

total_counts <- coha_counts %>%
  group_by(group) %>%
  summarize(total_tokens = sum(frequency)) %>%
  rename(year = group)

coha_counts <- coha_counts %>%
  group_by(group) %>%
  mutate(year_total = sum(frequency)) %>%
  ungroup() %>%
  mutate(freq_norm = (frequency/year_total)*1000000) %>%
  rename(year = group) %>%
  select(year, feature, frequency, freq_norm, rank, docfreq) %>%
  mutate(freq_norm = round(freq_norm, 2))

coha_counts %>% group_by(year) %>% summarize(total = sum(freq_norm))

write_csv(coha_counts, "/Users/davidwestbrown/Downloads/coha_counts.csv")
write_csv(total_counts, "/Users/davidwestbrown/Downloads/coha_totals.csv")

  