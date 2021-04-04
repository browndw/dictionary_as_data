library(tidyverse)

source("R/utils.R")

# Calculate the frequencies for lemmatized 'feller" in American English
# This requires reading in large data tables, so it will take a few minutes
feller <- google_ngram(word_forms = c("feller", "fellers"), variety = "us", by = "year")

# Format the dataframe
feller <- feller %>%
  mutate(year = as.numeric(year)) %>%
  filter(year > 1799 & year < 1910) %>%
  mutate(token = "feller")

# Calculate the frequencies for 'worrit" in American English
# This requires reading in large data tables, so it will take a few minutes
worrit <- google_ngram(word_forms = c("worrit"), variety = "us", by = "year")

# Format the dataframe
worrit <- worrit %>%
  mutate(year = as.numeric(year)) %>%
  filter(year > 1799 & year < 1910) %>%
  mutate(token = "worrit")

# Combine dataframes
websters_words <- list(feller, worrit) %>%
  bind_rows()

# Plot the data
ggplot(websters_words, aes(x=year, y=counts_permil, group = token)) +
  geom_point(aes(fill = token, shape = token), size =.35, color = "black", alpha = .35) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), size=.25, alpha = .25, color = "gray20") +
  scale_shape_manual(values=c(21, 25)) + 
  annotate("text", x = 1890, y = .5, size = 4, hjust = 0, vjust = 1,
           label = "↓ worrit", fontface = "bold") +
  annotate("text", x = 1885, y = 3.5, size = 4, hjust = 1, vjust = 1,
           label = "feller →", fontface = "bold") +
  scale_fill_manual(values=c("gray20", "white")) +
  labs(x="", y = "Frequency (per million words)") + 
  theme_classic() +
  theme(legend.position = "none")


