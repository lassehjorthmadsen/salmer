library(tidyverse)
library(devtools)
load_all()

# Show sample hymns
hymns %>% filter(doc_id == 1) %>% pull(text)

# Get pronounciation dictionary
pronounciation <- readRDS("data-raw/pronounciation.rds")

# cut_up
cu <- cut_up(ref_id = 1,
             df = annotated_hymns,
             except = "PUNCT")

# show cut-up version
cu %>% collapse_annotation(token = token_new)

# show original
cu %>% collapse_annotation(token = token)

# get rhyme_scheme:
rs <- rhyme_scheme(ref_id = 1,
                   df = annotated_hymns,
                   pron = pronounciation)

# bind rhyme scheme
cu <- cu %>% bind_cols(select(rs, scheme))

# get new rhymes

# first, df with both pronounciation and POS
pr <- pronounciation %>%
  distinct(token, .keep_all = T) %>%
  inner_join(distinct(annotated_hymns, token), by = "token") %>%
  select(token, stress_vowel, remainder)


nr <- cu %>%
  rowid_to_column("row_id") %>%
  filter(upos != "PUNCT") %>%
  group_by(line_id) %>%
  slice_max(order_by = token_id) %>%
  ungroup() %>%
  select(row_id,
         token_new,
         vowels,
         upos,
         scheme) %>%
  left_join(pr, by = c("token_new" = "token")) %>%
  mutate(
    must_rhyme_stress = case_when(
      scheme == 1 ~ lag(stress_vowel, 1),
      scheme == 2 ~ lag(stress_vowel, 2),
      scheme == 3 ~ lag(stress_vowel, 3),
      scheme == 4 ~ lag(stress_vowel, 4),
      TRUE ~ NA_character_
    ),
    must_rhyme_remainder = case_when(
      scheme == 1 ~ lag(remainder, 1),
      scheme == 2 ~ lag(remainder, 2),
      scheme == 3 ~ lag(remainder, 3),
      scheme == 4 ~ lag(remainder, 4),
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(scheme)) %>%
  left_join(select(annotated_hymns, token, vowels, upos, stress_vowel, remainder),
            by = c("vowels" = "vowels",
                   "upos" = "upos",
                   "must_rhyme_stress" = "stress_vowel",
                   "must_rhyme_remainder" = "remainder")) %>%
  group_by(row_id) %>%
  filter(token_new != token) %>%
  filter(token != "") %>%
  slice_sample(n = 1) %>%
  ungroup()

cu2 <- cu %>%
  left_join(select(nr, row_id, token), by = c("token_no" ="row_id")) %>%
  mutate(token_new = coalesce(token.y, token_new))

# show final cut-up version
cu %>% collapse_annotation(token = token_new)

# Get more rhyme schemas (takes a while)
many_rs <-
  1:2 %>% map(rhyme_scheme, df = annotated_hymns, pron = pronounciation)





# Can't simply do
cu %>% filter(!is.na(scheme)) %>% mutate(test = lag(token, scheme))


# Test some (possible) rhyms:
get_rhymes("borg", pronounciation)




#' Pronunciation Lexicon for Danish
#'
#' A collection of Danish words with pronounciations available at
#' \href{https://sprogteknologi.dk/dataset/nst-lexical-database-for-danish}{sprogteknologi.dk}
#' or from
#' \href{https://www.nb.no/sprakbanken/ressurskatalog/oai-nb-no-sbr-26}{den norske sprogbank i Nationalbiblioteket}
#' where documentation is also available.
#' Each row corresponds to one Danish word with phonetic annotation using
#' \href{https://www.phon.ucl.ac.uk/home/sampa/index.html}{sampa}
#' -- a computer readable phonetic alphabet.
#'
#' @format A tibble with 226,238 rows and 5 variables:
#' \describe{
#'   \item{token}{word in Danish (mostly)}
#'   \item{sampa}{Pronounciation in sampa phonetic alphabet}
#'   \item{rhyme_part}{The part of pronounciation relevant for rhymes: Stressed syllable and everyhing after}
#'   \item{stress_vowel}{Stressed syllable starting with first vowel}
#'   \item{remainder}{Everything in the sampa string field after stressed syllable}
#' }
"pronounciations"
