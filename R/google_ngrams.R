library(tidyverse)

irregardless <- google_ngram(word_forms = c("irregardless"), variety = "eng", by = "year")
irregardless <- irregardless %>%
  mutate(year = as.numeric(year)) %>%
  filter(year > 1799 & year < 2001) %>%
  mutate(token = "irregardless")

write_csv(irregardless, "/Users/davidwestbrown/Downloads/irregardless_frequencies.csv")


p1 <- ggplot(irregardless, aes(x=year, y=counts_permil)) +
  geom_vline(xintercept = 1961, linetype = "dotted") +
  geom_point(shape = 21, size =.35, color = "black", alpha = .35) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), size=.25, alpha = .25, color = "gray20") +
  annotate("text", x = 1955, y = .0615, size = 2, hjust = 1, vjust = 1,
           label = "The publication of\nWebster's Third\n(1961)") +
  annotate("text", x = 1960, y = .06, size = 3, hjust = 1, vjust = 0,
           label = "→") +labs(x="", y = "Frequency (per million words)") + 
  theme_classic() +
  theme(legend.position = "none")

ggsave("/Users/davidwestbrown/Downloads/irregardless.png", p1, width = 6.5, height = 3.5, dpi = 300)

gosh <- google_ngram(word_forms = c("gosh"), variety = "us", by = "year")
gosh <- gosh %>%
  mutate(year = as.numeric(year)) %>%
  filter(year > 1799 & year < 2001) %>%
  mutate(token = "gosh")

golly <- google_ngram(word_forms = c("golly"), variety = "us", by = "year")
golly <- golly %>%
  mutate(year = as.numeric(year)) %>%
  filter(year > 1799 & year < 2001) %>%
  mutate(token = "golly")

yep <- google_ngram(word_forms = c("yep"), variety = "us", by = "year")
yep <- yep %>%
  mutate(year = as.numeric(year)) %>%
  filter(year > 1799 & year < 2001) %>%
  mutate(token = "yep")

nope <- google_ngram(word_forms = c("nope"), variety = "us", by = "year")
nope <- nope %>%
  mutate(year = as.numeric(year)) %>%
  filter(year > 1799 & year < 2001) %>%
  mutate(token = "nope")

wow <- google_ngram(word_forms = c("wow"), variety = "us", by = "year")
wow <- wow %>%
  mutate(year = as.numeric(year)) %>%
  filter(year > 1799 & year < 2001) %>%
  mutate(token = "wow")

feller <- google_ngram(word_forms = c("feller", "fellers"), variety = "us", by = "year")
feller <- feller %>%
  mutate(year = as.numeric(year)) %>%
  filter(year > 1799 & year < 1910) %>%
  mutate(token = "feller")

worrit <- google_ngram(word_forms = c("worrit"), variety = "us", by = "year")
worrit <- worrit %>%
  mutate(year = as.numeric(year)) %>%
  filter(year > 1799 & year < 1910) %>%
  mutate(token = "worrit")

df_list <- list(feller, worrit)

df <- bind_rows(df_list)

write_csv(df, "/Users/davidwestbrown/Downloads/ngram_frequencies.csv")

ggplot(df, aes(x=year, y=freq_norm, group = token)) +
  geom_vline(xintercept = 1913, linetype = "dotted") +
  geom_point(shape = 21, size =.35, color = "black", alpha = .35) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), size=.25, alpha = .25, color = "gray20") +
  theme_linedraw() +
  theme(legend.position = "none") +
  theme(panel.grid = element_blank()) +
  theme(axis.text = element_text(size = 6)) +
  facet_wrap( ~ feature, ncol=2) +
  theme(strip.text = element_text(size = 6.5, face = "bold", color = "black")) +
  theme(strip.background = element_rect(fill="gray80", colour="black"))

p1 <- ggplot(df, aes(x=year, y=counts_permil, group = token)) +
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

ggsave("/Users/davidwestbrown/Downloads/feller_worrit.png", p1, width = 6.5, height = 4, dpi = 300)



yep_nope <- bind_rows(yep, nope) %>%
  mutate(token = as.factor(token))

p2 <- ggplot(yep_nope, aes(x=year, y=counts_permil, group = token)) +
  geom_vline(xintercept = 1913, linetype = "dotted") +
  geom_point(aes(fill = token, shape = token), size =.35, color = "black", alpha = .35) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), size=.25, alpha = .25, color = "gray20") +
  scale_shape_manual(values=c(21, 25)) + 
  annotate("text", x = 1910, y = 1.75, size = 3.5, hjust = 1, vjust = 1,
           label = "1913 →") +
  annotate("text", x = 1985, y = .5, size = 2.5, hjust = 0, vjust = 1,
           label = "← yep", fontface = "bold") +
  annotate("text", x = 1990, y = 1.5, size = 2.5, hjust = 1, vjust = 1,
           label = "nope →", fontface = "bold") +
  scale_fill_manual(values=c("gray20", "white")) +
  labs(x="", y = "Frequency (per million words)") + 
  theme_classic() +
  theme(legend.position = "none")

ggsave("/Users/davidwestbrown/Downloads/yep_nope.png", p2, width = 6.5, height = 4, dpi = 300)



ggplot(feller, aes(x=year, y=counts_permil)) +
  geom_vline(xintercept = 1913, linetype = "dotted") +
  geom_point(shape = 21, size =.35, color = "black", alpha = .35) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), size=.25, alpha = .25, color = "gray20") +
  annotate("text", x = 1910, y = 3.75, size = 3.5, hjust = 1, vjust = 1,
           label = "1913 →") +
  annotate("text", x = 1985, y = 2.55, size = 2.5, hjust = 1, vjust = 1,
           label = "wow →", fontface = "bold") +
  scale_fill_manual(values=c("gray20", "white")) +
  labs(x="", y = "Frequency (per million words)") + 
  theme_classic() +
  theme(legend.position = "none")



ggplot(all_tokens, aes(x=year, y=counts_permil, group = token)) +
  geom_vline(xintercept = 1913, linetype = "dotted") +
  geom_point(shape = 21, size =.35, color = "black", alpha = .35) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), size=.25, alpha = .25, color = "gray20") +
  theme_linedraw() +
  theme(legend.position = "none") +
  theme(panel.grid = element_blank()) +
  theme(axis.text = element_text(size = 6)) +
  facet_wrap( ~ source, ncol=2) +
  theme(strip.text = element_text(size = 6.5, face = "bold", color = "black")) +
  theme(strip.background = element_rect(fill="gray80", colour="black"))


google_ngram <- function(word_forms, variety=c("eng", "gb", "us", "fiction"), by=c("year", "decade")){
  n <- lapply(word_forms, function(x) stringr::str_count(x, "\\w+"))
  n <- unique(n)
  if (length(n) > 1)  stop ("Check spelling. Word forms should be lemmas of the same word (e.g. 'teenager' and 'teenagers' or 'walk' , 'walks' and 'walked'")
  gram <- lapply(word_forms, function(x) substring(x, 1, n))
  gram <- tolower(unique(gram))
  if (length(gram) > 1)  stop ("Check spelling. Word forms should be lemmas of the same word (e.g. 'teenager' and 'teenagers' or 'walk' , 'walks' and 'walked'")
  
  if(variety == "eng") repo <- paste0("http://storage.googleapis.com/books/ngrams/books/googlebooks-eng-all-", n, "gram-20120701-", gram, ".gz")
  if(variety != "eng") repo <- paste0("http://storage.googleapis.com/books/ngrams/books/googlebooks-eng-", variety, "-all-", n, "gram-20120701-", gram, ".gz")
  
  all_grams <- suppressWarnings(readr::read_tsv(repo, col_names = FALSE, quote = ""))
  colnames(all_grams) <- c("token", "year", "token_count", "pages")

  if(variety == "eng") repo_total <-("http://storage.googleapis.com/books/ngrams/books/googlebooks-eng-all-totalcounts-20120701.txt")
  if(variety != "eng") repo_total <-paste0("http://storage.googleapis.com/books/ngrams/books/googlebooks-eng-", variety, "-all-totalcounts-20120701.txt")
  total_counts <- suppressWarnings(read.csv(repo_total, header = FALSE, sep = "\t", quote = ""))

  total_counts <- as.data.frame(t(total_counts))
  total_counts <- data.frame(V1 = (total_counts[!is.na(total_counts),]))
  total_counts <- data.frame(do.call("rbind", strsplit(as.character(total_counts$V1), "," ,fixed = TRUE)))
  colnames(total_counts) <- c("year", "total_count", "page_count", "volume_count")
  total_counts$total_count <- as.numeric(as.character(total_counts$total_count))
  total_counts$decade <- gsub("\\d$", "0", total_counts$year)
  if (by == "year") total_counts <- aggregate(total_count ~ year, total_counts, sum)
  if (by == "decade") total_counts <- aggregate(total_count ~ decade, total_counts, sum)
  
  grep_words <- paste0("^", word_forms, "$", collapse = "|")
  all_tokens <- subset(all_grams, grepl(grep_words, all_grams$token, ignore.case=TRUE))
  all_tokens$token <- tolower(all_tokens$token)
  sum_tokens <- aggregate(token_count ~ year, all_tokens, sum)
  sum_tokens$decade <- gsub("\\d$", "0", sum_tokens$year)
  if (by == "decade") sum_tokens <- aggregate(token_count ~ decade, sum_tokens, sum)
  if (by == "year") sum_tokens <- merge(sum_tokens, y = total_counts[,c(1:2)], by = "year")
  if (by == "decade") sum_tokens <- merge(sum_tokens, y = total_counts[,c(1:2)], by = "decade")
  counts_norm <- mapply(function(x,y) (x/y)*1000000, sum_tokens$token_count, sum_tokens$total_count)
  sum_tokens$counts_permil <- counts_norm
  return(sum_tokens)
}

ngram_df$year <- as.numeric(ngram_df$year)
ngram_df <- ngram_df[ which(ngram_df$year >= start & ngram_df$year <= end), ]

plot_year <- function(ngram_df, start=1800, end=2000) {
  ngram_df$year <- as.numeric(ngram_df$year)
  ngram_df <- ngram_df[ which(ngram_df$year >= start & ngram_df$year <= end), ]
  ggplot(ngram_df, aes(x=year, y=counts_permil)) +
    geom_point(size = .5) +
    geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), size=.25) +
    labs(x="year", y = "frequency (per million words)")+ 
    theme(panel.grid.minor.x=element_blank(),
          panel.grid.major.x=element_blank()) +
    theme(panel.grid.minor.y =   element_blank(),
          panel.grid.major.y =   element_line(colour = "gray",size=0.25)) +
    theme(rect = element_blank()) +
    theme(legend.title=element_blank()) +
    theme(axis.title = element_text(family = "Arial", color="#666666", face="bold", size=10))
}

