library(tidyverse)
library(devtools)
load_all()

# Get pronounciation dictionary
# pronounciation <- readRDS("data-raw/pronounciation.rds")

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

# cut up
cu <- cut_up(ref_id = test_hymn,
             df = annotated_hymns,
             except = "PUNCT")

# new rhymes
nr <- new_rhymes(cu, annotated_hymns)

# show final cut-up version along with original and other bits
nr %>%
  collapse_annotation(token = token_new) %>%
  bind_cols(hymns %>% filter(doc_id == test_hymn) %>% select(text)) %>%
  left_join(cu %>% filter(!is.na(rhyme_scheme)) %>% select(line_id, rhyme_scheme, verse), by = "line_id") %>%
  view("final")

new_rhymes <- function(hymn, df = annotated_hymns) {

  # All tokens from hymns, with rhyme info
  annotated_tokens <- annotated_hymns %>%
    filter(!is.na(sampa)) %>%
    distinct(token_new = tolower(token), vowels, stress_vowel, remainder)

  # Pronunciation info for relevant tokens
  pr <- annotated_tokens %>%
    select(-vowels) %>%
    inner_join(distinct(cu, token_new), by = "token_new")

  # Filter out all last words in our cut-up
  last_words <- cu %>%
    rowid_to_column("row_id") %>%
    filter(upos != "PUNCT") %>%
    group_by(line_id) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    select(row_id,
           token = token_new,
           vowels,
           rhyme_scheme) %>%
    mutate(token = tolower(token))

  # Get rhyming contrains, if any
  new_last_words <- last_words %>%
    mutate(
      must_rhyme_with = case_when(
        rhyme_scheme == 1 ~ lag(token, 1),
        rhyme_scheme == 2 ~ lag(token, 2),
        rhyme_scheme == 3 ~ lag(token, 3),
        rhyme_scheme == 4 ~ lag(token, 4),
        TRUE ~ NA_character_)
    ) %>%
    left_join(pr, by = c("must_rhyme_with" = "token_new")) %>%
    filter(!is.na(rhyme_scheme)) %>%
    left_join(annotated_tokens,
              by = c("vowels" = "vowels",
                     #"upos" = "upos", We don't constrain on upos for rhymes
                     "stress_vowel" = "stress_vowel",
                     "remainder" = "remainder")) %>%
    filter(must_rhyme_with != token) %>%
    group_by(row_id) %>%
    filter(token != "") %>%
    slice_sample(n = 1) %>%
    ungroup()

  # Fix rhymes
  cu_final <- cu %>%
    left_join(select(new_last_words, row_id, token_replace = token_new), by = c("token_no" ="row_id")) %>%
    mutate(token_new = coalesce(token_replace, token_new))

  return(cu_final)
}


# Test some (possible) rhyms:
get_rhymes("borg", pronounciation)
get_rhymes("kartofler", pronounciation)
get_rhymes("smerte", pronounciation)
get_rhymes("svin", pronounciation)
get_rhymes("fisk", pronounciation)
