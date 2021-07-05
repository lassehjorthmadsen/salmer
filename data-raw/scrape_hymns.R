# Scrape hymns from www.dendanskesalmebogonline.dk
library(tidyverse)
library(rvest)

source("data-raw/content_to_tibble.R")
base_url <- "https://www.dendanskesalmebogonline.dk/salme/"

hymns <- 1:791 %>% # Hardcoded no of hymns as 791
  set_names() %>%
  map_chr(~ paste0(base_url, .x)) %>%
  map(read_html, encoding = "utf-8") %>%
  map_dfr(content_to_tibble, .id = "hymn_id")

# Clean
hymns <- hymns %>%
  mutate(across(c(hymn_id, verse), as.numeric))

valid_authors <- read.csv2("data-raw/valid-authors.csv", header = FALSE) %>%
  pull(V1) %>%
  paste(collapse = "|")

hymns <- hymns %>%
  mutate(year = str_extract(author, "[:digit:]{4}"),
         author = str_extract(author, valid_authors),
         author = str_replace(author, "Wipo", "Wipo af Burgund"),
         author = str_replace(author, "Gerhard Teerstegen", "Gerhard Tersteegen"))

# Checks
# Quick look at hymn id, title, author
hymns %>%
  count(hymn_id = as.numeric(hymn_id), title, author, melody1, no_verses) %>%
  slice_sample(n = 10)

# does no_verses always agree with max(verse)?
hymns %>%
  filter(!is.na(no_verses)) %>%
  group_by(hymn_id) %>%
  count(okay = max(verse) == max(no_verses)) %>%
  ungroup() %>%
  count(okay)

# Is no_verses NA only when we have no verses?
hymns %>% count(is.na(no_verses), text == "")

# Do we have data for all 791 hymns?
n_distinct(hymns$hymn_id) == 791

# use data in package
usethis::use_data(hymns, overwrite = TRUE)
