library(tidyverse)
devtools::load_all()


# Test some (possible) rhyms:
rhyms <- pronounciation %>%
  slice_sample(n = 1000) %>%
  left_join(select(pronounciation, rhyme = token, stress_vowel, remainder),
            by = c("stress_vowel", "remainder")) %>%
  filter(token != rhyme) %>%
  group_by(token) %>%
  slice_sample(n = 5) %>%
  ungroup()

rhyms %>%
  select(token, rhyme) %>%
  slice_sample(n = 5)




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
