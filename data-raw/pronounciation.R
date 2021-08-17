# Get pronounciation dictionary, tweak to search for rhymes.
# Source: https://www.nb.no/sprakbanken/ressurskatalog/oai-nb-no-sbr-26/
# See documentation at the above site and in files:
# data-raw/DA_SAMPA_transkonv.doc
# data-raw/PhonTable_danish_ipa_sampa_ibm_v.1.3.doc

library(dplyr)
library(stringr)

devtools::load_all()

if (!file.exists("data-raw/pronounciations.rda")) {

  url <- "https://www.nb.no/sbfil/leksikalske_databaser/leksikon/da_leksikon.tar.gz"
  file <- basename(url)
  download.file(url, file)

  untar(file, exdir = "data-raw/")
  files <- list.files("data-raw", full.names = TRUE, recursive = TRUE)

  df <- read.delim(files[str_detect(files, "dan030224NST.pron$")],
                   sep = ";", quote = "", header = FALSE, skip = 3)

  # Encoding(df$V1) <- "UTF-8"   # Required by CRAN it seems. Find workaround?
  # Encoding(df$V12) <- "UTF-8"

  vowels_pat <- "(i:|i|y:|y|e:|e|2:|2|9:|9|E:|E|u:|u|o:|o|O:|O|Q:|Q|6|A:|A|a|a:|@)[^\\$]*"

  pronounciations <- df %>%
    select(token = V1, sampa = V12) %>%
    filter(!str_detect(token, "_")) %>% # Skip some multi-word entries
    mutate(vowels = str_count(token, "a|e|i|o|u|y|æ|ø|å"),
           rhyme_part = str_extract(sampa, "\".+"), # Stressed syllable and everyhing after
           stress_vowel = str_extract(rhyme_part, vowels_pat), # Vowel part of stressed syllable
           remainder = str_extract(rhyme_part, "\\$.+$")) %>% # Everthing after stressed syllable
    as_tibble()

  # Save data for use in annotate.R
  saveRDS(pronounciations, "data-raw/pronounciations.rds")
}

# Test some (possible) rhyms:
rhyms <- pronounciations %>%
  slice_sample(n = 1000) %>%
  left_join(select(pronounciations, rhyme = token, vowels, stress_vowel, remainder),
            by = c("vowels", "stress_vowel", "remainder")) %>%
  filter(token != rhyme) %>%
  group_by(token) %>%
  slice_sample(n = 5) %>%
  ungroup()

rhyms %>%
  select(token, vowels, rhyme) %>%
  group_by(vowels) %>%
  slice_sample(n = 5) %>%
  arrange(vowels, token, rhyme) %>%
  View()

