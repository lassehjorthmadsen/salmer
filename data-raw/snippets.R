library(tidyverse)
devtools::load_all()

annotated_hymns <- annotated_hymns %>% mutate(across(c(doc_id, token_id, head_token_id), as.integer))

df <- annotated_hymns %>%
  select(doc_id, paragraph_id, token, upos)

few_hymns <- hymns %>%
  #filter(hymn_id < 51) %>%
  select(hymn_id, text) %>%
  group_by(hymn_id) %>%
  mutate(paragraph_id = row_number()) %>%
  rename(doc_id = hymn_id) %>%
  ungroup()

few_hymns2 <- collapse_annotation(df)

comp <- few_hymns %>%
  mutate(text = str_trim(text)) %>%
  left_join(few_hymns2, by = c("doc_id", "paragraph_id")) %>%
  replace_na(list(text.y = ""))

comp %>% view()
comp <- comp %>% mutate(lx = nchar(text.x), ly = nchar(text.y))
comp %>% count(ly - lx)

comp %>% count(text.x == text.y)
comp %>% filter(text.x != text.y) %>% data.frame()
comp %>% filter(str_detect(text.x, "' "))
comp %>% filter(str_detect(text.x, "1"))

