
# This function extracts frequency data from Google Books' Ngram data:
# http://storage.googleapis.com/books/ngrams/books/datasetsv2.html
# The function is set up to facilitate the counting of lemmas
# and ingnore differences in capitalization.
# The user has control over what to combine into counts with
# the "word_forms argument.
#
# NOTE!!! Google's data tables are HUGE. Sometime running into
# multiple gigabytes for simple text files. Thus, depending
# on the table being accessed, the return time can be slow.
# For example, asscessing the 1-gram Q file should take only a few seconds,
# but the 1-gram T file might take 10 minutes to process.
# The 2-gram, 3-gram, etc. files are even larger and slower to process.

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
  counts_norm <- round(counts_norm, 2)
  sum_tokens$counts_permil <- counts_norm
  return(sum_tokens)
}

# This is a simple wrapper for plotting by decade.

plot_decade <- function(ngram_df, start=1800, end=2000) {
  ngram_df$decade <- as.numeric(ngram_df$decade)
  ngram_df <- ngram_df[ which(ngram_df$decade >= start & ngram_df$decade <= end), ]
  ggplot(ngram_df, aes(x=decade, y=counts_permil)) +
    geom_bar(stat = "identity") +
    labs(x="decade", y = "frequency (per million words)")+ 
    theme(panel.grid.minor.x=element_blank(),
          panel.grid.major.x=element_blank()) +
    theme(panel.grid.minor.y =   element_blank(),
          panel.grid.major.y =   element_line(colour = "gray",size=0.25)) +
    theme(rect = element_blank()) +
    theme(legend.title=element_blank()) +
    theme(axis.title = element_text(family = "Arial", color="#666666", face="bold", size=10))
}

# This is a simple wrapper function for plotting by year, with a confidence interval.

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

# This function extracts and cleans citation data from Johnson's dictionary,
# which are stored as html files.

johnson_cited <- function(x){
  
  names <- rvest::read_html(x) %>%
    rvest::html_nodes('body') %>%
    rvest::html_nodes('span') %>%
    rvest::html_text() %>%
    data.frame(author = .) %>%
    mutate(author = str_squish(author)) %>%
    mutate(author = textclean::replace_curly_quote(author)) %>%
    mutate(author = stringi::stri_trans_general(author, "Latin-ASCII")) %>%
    mutate(test = lag(str_detect(author, "'s|s'"), default = F)) %>%
    filter(test == F) %>%
    select(author) %>%
    mutate(author = str_remove(author, "'$")) %>%
    filter(str_detect(author, "[a-z]") ==T) %>%
    filter(str_detect(author, "^[A-Z][a-z][a-z]+|[A-Z]'[A-Z][a-z][a-z]+") ==T) %>%
    mutate(author = str_remove(author, "'s$")) %>%
    mutate(author = ifelse(str_detect(author, "peare$"), "Shakespeare", author)) %>%
    mutate(author = ifelse(str_detect(author, "^Shakesp$"), "Shakespeare", author)) %>%
    mutate(author = ifelse(str_detect(author, "^Lear$"), "Shakespeare", author)) %>%
    mutate(author = ifelse(str_detect(author, "^Othello$"), "Shakespeare", author)) %>%
    mutate(author = ifelse(str_detect(author, "^Tempest$"), "Shakespeare", author)) %>%
    mutate(author = ifelse(str_detect(author, "^Cymbeline$"), "Shakespeare", author)) %>%
    mutate(author = ifelse(str_detect(author, "^Hamlet$"), "Shakespeare", author)) %>%
    mutate(author = ifelse(str_detect(author, "^Macbeth$"), "Shakespeare", author)) %>%
    mutate(author = ifelse(str_detect(author, "^Paradise$"), "Milton", author)) %>%
    mutate(author = ifelse(str_detect(author, "^Fairy$"), "Spenser", author)) %>%
    mutate(author = ifelse(str_detect(author, "^Fairie$"), "Spenser", author)) %>%
    mutate(author = ifelse(str_detect(author, "^Aeneid$"), "Dryden", author)) %>%
    mutate(author = ifelse(str_detect(author, "^Naturall$"), "Bacon", author)) %>%
    mutate(author = ifelse(str_detect(author, "^Spectator$"), "Addison", author)) %>%
    mutate(author = ifelse(str_detect(author, "^Odyssey$"), "Pope", author)) %>%
    mutate(author = ifelse(str_detect(author, "^Sermons$"), "South", author)) %>%
    mutate(author = ifelse(str_detect(author, "^Vulgar$"), "Brown", author)) %>%
    mutate(author = ifelse(str_detect(author, "^Farrier"), "Wallace", author)) %>%
    mutate(author = ifelse(str_detect(author, "^Gen$"), "Genesis", author)) %>%
    mutate(author = ifelse(str_detect(author, "^Gen\\.$"), "Genesis", author)) %>%
    mutate(author = ifelse(str_detect(author, "^Deut$"), "Deuteronomy", author)) %>%
    mutate(author = ifelse(str_detect(author, "^Ezek$"), "Ezekiel", author)) %>%
    mutate(author = ifelse(str_detect(author, "^Exod$"), "Exodus", author)) %>%
    mutate(author = ifelse(str_detect(author, "^Isa$"), "Isaiah", author)) %>%
    mutate(author = ifelse(str_detect(author, "^Psalm$"), "Psalms", author)) %>%
    mutate(author = ifelse(str_detect(author, "^Jer$"), "Jeremiah", author)) %>%
    mutate(author = ifelse(str_detect(author, "^Eccl$"), "Ecclesiastes", author)) %>%
    mutate(author = ifelse(str_detect(author, "^Lev$"), "Leviticus", author)) %>%
    mutate(author = ifelse(str_detect(author, "^Revel$"), "Revelations", author)) %>%
    mutate(author = ifelse(str_detect(author, "^Chron$"), "Chronicles", author)) %>%
    mutate(author = ifelse(str_detect(author, "^Cor$"), "Corinthians", author)) %>%
    mutate(author = ifelse(str_detect(author, "^Gulliv"), "Swift", author)) %>%
    mutate(author = ifelse(str_detect(author, "^Brown$"), "Browne", author)) %>%
    mutate(author = ifelse(str_detect(author, "^Par$|^Parad$"), "Spenser", author)) %>%
    mutate(author = ifelse(str_detect(author, "^Educ\\S+n$"), "Locke", author)) %>%
    mutate(author = ifelse(str_detect(author, "^Law$"), "Hooker", author)) %>%
    mutate(author = ifelse(str_detect(author, "^Sermon"), "South", author)) %>%
    mutate(author = ifelse(str_detect(author, "^Colour"), "Boyle", author))
  
  return(names)
}

clean_entries_johnson <- function(x) {
  bible_books <- read_csv("data/data_tables/bible_list.csv")
  stop_words <- readLines("data/data_tables/stop_words.txt")
  
  cleaned_entries <- x %>% mutate(author = ifelse(!is.na(bible_books$source[match(cited_johnson$author, bible_books$book)]),
                                                            bible_books$source[match(cited_johnson$author, bible_books$book)], author))   
  
  cleaned_entries <- cleaned_entries$author[!cleaned_entries$author %in% stop_words] %>% 
    data.frame(author = ., stringsAsFactors = F)
  
  cleaned_entries <- cleaned_entries %>%
    count(author) %>%
    arrange(-n) %>%
    mutate(name_length = str_count(author)) %>%
    mutate(name_start = substring(author, 1, 4))
  
  replacement_df <- cleaned_entries %>%
    group_by(name_start) %>%
    top_n(1, n) %>%
    rename(replacement = author) %>%
    filter(n > 49) %>%
    select(replacement, name_start) %>%
    full_join(cleaned_entries) %>%
    mutate(replacement = ifelse(is.na(replacement), author, replacement)) %>%
    group_by(replacement) %>%
    summarize(author = unique(replacement),
              n = sum(n)) %>%
    arrange(-n)
  
  reference_vector <- replacement_df$author[replacement_df$n >= 50]
  check_names <- replacement_df$author[replacement_df$n < 50]
  
  correct_spelling <- function(word) {
    edit_dist <- suppressWarnings(stringdist::stringdist(word, reference_vector, method = "jw"))
    if(length(edit_dist[edit_dist < .2]) == 0) {return(NA)}
    replacement <- reference_vector[ edit_dist == min(edit_dist)]
    if(length(replacement) > 1) replacement <- replacement[1]
    return(replacement)
  }
  
  replacement_df$new_name <- c(reference_vector, sapply(check_names, correct_spelling) %>% unlist())
  
  replacement_df <- replacement_df %>%
    mutate(new_name = ifelse(is.na(new_name), author, new_name)) %>%
    group_by(new_name) %>%
    summarize(author = unique(new_name),
              n = sum(n)) %>%
    arrange(-n)
  
  return(replacement_df)
  
}

# Extract head words from Websters
headwords_websters <- function(x){
  
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

# Extract cited authors from Websters
websters_cited <- function(x){
  
  doc <- read_file(x)
  doc<- doc %>% str_squish() %>% str_replace_all("</p>\\s+<p>", "</p><p>")
  doc <- doc %>% str_split("</p><p>") %>% unlist()
  
  nodes <-  doc[grepl("<source>1913 Webster</source>", doc)]
  
  authors <- lapply(nodes, function(x) str_extract_all(x, "<q?au>.*?</q?au>"))
  authors <- authors %>% unlist()
  authors <- authors %>% str_split("(?<=[a-z-][a-z-]\\.) (?=[A-Z][a-z-])") %>% unlist()
  authors <- authors[!is.na(authors)]
  authors <- authors %>% str_remove_all("<.*?>") %>% tolower()
  authors <- authors %>% unlist() %>% unname() %>% table() %>% as_tibble() %>% 
    rename_with(~ gsub(".", "author", .x, fixed = TRUE)) %>% 
    mutate(author = str_remove(author, ".$")) %>%
    mutate(author = str_remove(author, "^[[:punct:]]")) %>%
    mutate(author = str_remove_all(author, "\\(.*?\\)")) %>%
    mutate(author = str_remove_all(author, "\\[.*?\\]")) %>%
    mutate(author = str_remove_all(author, "\\(.*?$")) %>%
    mutate(author = str_squish(author)) %>%
    mutate(author = ifelse(str_detect(author, "\\. \\d+"), "bible", author)) %>%
    mutate(author = ifelse(str_detect(author, "^\\d"), "bible", author)) %>%
    mutate(author = ifelse(str_detect(author, "^shak"), "shakespeare", author)) %>%
    mutate(author = textclean::replace_html(author)) %>%
    mutate(author = str_to_title(author)) %>%
    mutate(author = ifelse(author == "L'estrange", "L'Estrange", author)) %>%
    mutate(author = ifelse(author == "Sir T. Browne", "Browne", author)) %>%
    mutate(author = ifelse(author == "Sir P. Sidney", "Sidney", author)) %>%
    group_by(author) %>%
    summarize(n = sum(n)) %>%
    arrange(-n) %>%
    rename(n_websters = n)
  
  return(authors)
}

