library(forcats)
library(tidyverse)
devtools::load_all()

valid_authors <- read.csv2("data-raw/valid-authors.csv", header = FALSE) %>%
  pull(V1) %>%
  paste(collapse = "|")

df <- hymns %>%
  mutate(year = str_extract(author, "[:digit:]{4}"),
         author_clean = str_extract(author, valid_authors),
         author_clean = str_replace(author_clean, "Wipo", "Wipo af Burgund"),
         author_clean = str_replace(author_clean, "Gerhard Teerstegen", "Gerhard Tersteegen"))

df %>% filter(is.na(author_clean)) %>%
  count(hymn_id, author, sort = T) %>% view()

hymns %>% count(author, sort = T) %>% view()

df %>% count(author_clean, sort = T) %>% view()

Wipo - Wipo af Burgund

Gerhard Teerstegen
Gerhard Tersteegen



df %>%
  filter(str_detect(author_clean, "Berthier")) %>%
  distinct(hymn_id, .keep_all = T) %>%
  select(hymn_id, title, author, author_clean) %>%
  view()



