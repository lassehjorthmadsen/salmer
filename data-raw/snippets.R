library(tidyverse)
library(devtools)
load_all()

# Get pronounciation dictionary
pronounciation <- readRDS("data-raw/pronounciation.rds")

test_hymn <- 749

# De ti mest afspillede salmer på denne side:
# 754 Se, nu stiger solen af havets skød
# 448 Fyldt af glæde over livets under
# 522 Nåden er din dagligdag
# 192 Hil dig, Frelser og Forsoner
# 787 Du, som har tændt millioner af stjerner
# 582 At tro er at komme
# 749 I østen stiger solen op
# 15 Op, al den ting, som Gud har gjort
# 731 Nu står der skum fra bølgetop
# 729 Nu falmer skoven trindt om land

# cut_up
cu <- cut_up(ref_id = test_hymn,
             df = annotated_hymns,
             except = "PUNCT")

# get new rhymes

# first, df with rhyme info
# (consider if whole pronounciation dictionary should be used for fixing ryhmes)
pr <- pronounciation %>%
  distinct(token, .keep_all = T) %>%
  inner_join(distinct(cu, token), by = "token") %>%
  select(token, stress_vowel, remainder) %>%
  mutate(token = tolower(token))

# then, df with all tokens from hymns, with rhyme info
annotated_tokens <- annotated_hymns %>%
  select(token, vowels, stress_vowel, remainder) %>%
  distinct(.keep_all = TRUE)

# Filter out all last words in our cut-up
last_words <- cu %>%
  rowid_to_column("row_id") %>%
  filter(upos != "PUNCT") %>%
  group_by(line_id) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  select(row_id,
         token_new,
         vowels,
         upos,
         rhyme_scheme) %>%
  mutate(token_new = tolower(token_new))

# Get rhyming contrains, if any
last_words2 <- last_words %>%
  mutate(
    must_rhyme_with = case_when(
      rhyme_scheme == 1 ~ lag(token_new, 1),
      rhyme_scheme == 2 ~ lag(token_new, 2),
      rhyme_scheme == 3 ~ lag(token_new, 3),
      rhyme_scheme == 4 ~ lag(token_new, 4),
      TRUE ~ NA_character_)
    ) %>%
  left_join(pronounciation, by = c("must_rhyme_with" = "token")) %>%
  filter(!is.na(rhyme_scheme)) %>%
  left_join(annotated_tokens,
            by = c("vowels" = "vowels",
                   #"upos" = "upos", We don't constrain on upos for rhymes
                   "stress_vowel" = "stress_vowel",
                   "remainder" = "remainder")) %>%
  filter(must_rhyme_with != tolower(token)) %>%
  group_by(row_id) %>%
  filter(token != "") %>%
  slice_sample(n = 1) %>%
  ungroup()

# Fix rhymes
cu_final <- cu %>%
  left_join(select(last_words2, row_id, token), by = c("token_no" ="row_id")) %>%
  mutate(token_new = coalesce(token.y, token_new))

# show final cut-up version along with original and other bits
cu_final %>%
  collapse_annotation(token = token_new) %>%
  bind_cols(last_word = last_words$token_new) %>%
  bind_cols(hymns %>% filter(doc_id == test_hymn) %>% select(text)) %>%
  left_join(cu %>% filter(!is.na(rhyme_scheme)) %>% select(line_id, rhyme_scheme), by = "line_id") %>%
  view("final")


# Test some (possible) rhyms:
get_rhymes("borg", pronounciation)
get_rhymes("kartofler", pronounciation)
get_rhymes("smerte", pronounciation)
get_rhymes("svin", pronounciation)
get_rhymes("fisk", pronounciation)
