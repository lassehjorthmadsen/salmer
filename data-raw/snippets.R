library(tidyverse)
library(devtools)
load_all()

# Show sample hymns
hymns %>% filter(doc_id == 1) %>% pull(text)

# Get pronounciation dictionary
pronounciation <- readRDS("data-raw/pronounciation.rds")

# cut_up
cu <- cut_up(ref_id = 1, df = annotated_hymns, except = "PUNCT")

# show cut-up version
cu %>% collapse_annotation(token = token_new)

# show original
cu %>% collapse_annotation(token = token)

# get rhyme_scheme:
rs <- rhyme_scheme(ref_id = 1, df = annotated_hymns, pron = pronounciation)

# bind rhyme scheme
cu <- cu %>% bind_cols(select(rs, scheme))

# view line endings
cu %>%
  filter(upos != "PUNCT") %>%
  group_by(paragraph_id) %>%
  slice_max(order_by = token_id) %>%
  ungroup() %>%
  select(paragraph_id, token_id, token,token_new,scheme) %>%
  mutate(must_rhyme_with = if_else(is.na(scheme), NA, lag(token_new, n = scheme))) %>%
  view()

# Get more rhyme schemas (takes a while)
many_rs <- 1:2 %>% map(rhyme_scheme, df = annotated_hymns, pron = pronounciation)





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
