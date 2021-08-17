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
