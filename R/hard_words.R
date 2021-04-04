
library(tidyverse)
library(stringdist)

cawdrey <- read_lines("/Users/davidwestbrown/Downloads/cawdrey_wordlist.txt") %>%
  data.frame(token = .) %>%
  mutate(token = str_squish(token))

cp <- read_lines("/Users/davidwestbrown/Downloads/cp_wordlist.txt") %>%
  data.frame(token = .) %>%
  mutate(token = str_squish(token))

df <- inner_join(cawdrey, cp)


df <- cawdrey %>%
  mutate(near_match = ClosestMatch2(token)) %>%
  filter(!is.na(near_match))

ClosestMatch2 = function(string){
  cp$token[amatch(string, cp$token, maxDist=1)]
}

nrow(df)/nrow(cawdrey)
