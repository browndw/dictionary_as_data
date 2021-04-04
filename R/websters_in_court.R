
library(tidyverse)

civil_cases <- read_csv("/Users/davidwestbrown/SpiderOak Hive/Dictionary Chapter/dictionary_data/federal_civil_cases.csv")
criminal_cases <- read_csv("/Users/davidwestbrown/SpiderOak Hive/Dictionary Chapter/dictionary_data/federal_criminal_cases.csv")
webster_citations <- read_csv("/Users/davidwestbrown/SpiderOak Hive/Dictionary Chapter/dictionary_data/federal_citations.csv")

federal_caseload <- civil_cases %>%
  select(Year, US_Suits_Terminated) %>%
  full_join(select(criminal_cases, Year, Terminated)) %>%
  filter(Year > 1874 & Year < 2015) %>%
  mutate(case_load = US_Suits_Terminated + Terminated) %>%
  mutate(time_chunk = cut(Year, seq(1874, 2014, 10), dig.lab = 5)) %>%
  mutate(time_chunk = str_remove_all(time_chunk, "\\(|\\]")) %>%
  mutate(time_chunk = str_replace(time_chunk, "4,", "5-")) %>%
  group_by(time_chunk) %>%
  summarize(case_load = sum(case_load))

citation_freq <- federal_caseload %>%
  full_join(webster_citations) %>%
  mutate(narrow_freq = (narrow_search/case_load)*1000) %>%
  mutate(broad_freq = (broad_search/case_load)*1000)

p1 <- ggplot(citation_freq, aes(x = time_chunk, y = broad_freq)) +
  geom_col() +
  xlab("") + 
  ylab("Per 1000 Terminated Cases") +
  ylim(0, 10) +
  theme_classic() +
  theme(axis.text.x = element_text(angle=90, vjust = .5))
  

ggsave("/Users/davidwestbrown/Downloads/websters_court.png", plot=p1, width=6.5, height=3.5, dpi=300)
