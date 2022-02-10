# This script is sourced from annotate.R if no pronunciation
# dictornary is found.
#
# Download pronunciation dictionary and tweak to search for rhymes.
# Source: https://www.nb.no/sprakbanken/ressurskatalog/oai-nb-no-sbr-26/
# See documentation at the above site and in files:
# data-raw/DA_SAMPA_transkonv.doc
# data-raw/PhonTable_danish_ipa_sampa_ibm_v.1.3.doc

url <- "https://www.nb.no/sbfil/leksikalske_databaser/leksikon/da_leksikon.tar.gz"
file <- basename(url)

files <- list.files("data-raw", full.names = TRUE, recursive = TRUE)
diction_files <- str_detect(files, "dan030224NST.pron$")

if (any(diction_files)) {
  df <- read.delim(files[which(diction_files)[1]], sep = ";", quote = "", header = FALSE, skip = 3)
} else {
  download.file(url, file)
  untar(file, exdir = "data-raw/")
}

# Encoding(df$V1) <- "UTF-8"   # Required by CRAN it seems. Find workaround?
# Encoding(df$V12) <- "UTF-8"

vowels_pat <- "(i:|i|y:|y|e:|e|2:|2|9:|9|E:|E|u:|u|o:|o|O:|O|Q:|Q|6|A:|A|a|a:|@)[^\\$]*"

pronunciation <- df %>%
  select(token = V1, sampa = V12) %>%
  filter(!str_detect(token, "_")) %>% # Skip some multi-word entries
  mutate(rhyme_part = str_extract(sampa, "\".+"), # Stressed syllable and everyhing after
         stress_vowel = str_extract(rhyme_part, vowels_pat), # Vowel part of stressed syllable
         stress_vowel = str_remove(stress_vowel, "\\?"), # Get rid of '?' indicating 'push'
         remainder = str_extract(rhyme_part, "\\$.+$")) %>% # Everything after stressed syllable
  as_tibble()

# Save data for use in annotate.R
saveRDS(pronunciation, "data-raw/pronunciation.rds")
