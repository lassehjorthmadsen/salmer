library(tidyverse)
devtools::load_all()

hymns <- hymns %>% rename(doc_id = hymn_id)
usethis::use_data(hymns, overwrite = TRUE)

annotated_hymns <- annotated_hymns %>%
  mutate(vowels = str_count(token, "a|e|i|o|u|y|æ|ø|å")) %>%
  mutate(across(c(doc_id, token_id, head_token_id), as.integer))

usethis::use_data(annotated_hymns, overwrite = TRUE)

df <- annotated_hymns %>%
  select(doc_id, paragraph_id, token, upos)

df <- df %>% mutate(ryhm = str_extract(token, "[aeiouyæøå][^aeiouyæøå]+$"))

df %>%
  filter(!is.na(ryhm)) %>%
  slice_sample(n = 10) %>%
  left_join(select(df, token, ryhm), by = "ryhm") %>%
  slice_sample(n = 10)
