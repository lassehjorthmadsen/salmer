library(udpipe)
library(dplyr)
library(stringr)
library(tibble)

devtools::load_all()

# Check for udpipe models for POS-tagging
model_files <- list.files("data-raw", "*.udpipe", full.names = TRUE)

# Load or download udpipe model
if (length(model_files) > 0) {
  mdl <- udpipe_load_model(model_files[1])
} else {
  location <- udpipe_download_model(language = "danish-ddt", model_dir = "data-raw")
  mdl <- udpipe_load_model(location$file_model)
}

# Load or download pronounciation dictionary
if (!file.exists("data-raw/pronounciations.rds")) {
  source("data-raw/pronounciation.R")
} else {
  pronounciation <- readRDS("data-raw/pronounciation.rds")
}

# Annotate hymns
if (!file.exists("data/annotated_hymns.rda")) {
  # POS tagging and vowel counting
  annotated_hymns <- udpipe_annotate(object = mdl,
                                     x = hymns$text,
                                     doc_id = as.character(hymns$doc_id)) %>%
    as_tibble() %>%
    mutate(vowels = str_count(token, "a|e|i|o|u|y|æ|ø|å")) %>%
    mutate(across(c(doc_id, token_id, head_token_id), as.integer)) %>%
    select(-sentence_id, -sentence, -xpos, -feats, -head_token_id, -dep_rel, -deps, -misc)

  # Join pronounciation
  pron <- pronounciation %>%
    mutate(token_low = tolower(token)) %>%
    distinct(token_low, .keep_all = TRUE) %>%
    select(token_low, sampa, stress_vowel, remainder)

  annotated_hymns <- annotated_hymns %>%
    mutate(token_low = tolower(token)) %>%
    left_join(pron, by = "token_low") %>%
    select(-token_low)

  # use data in package
  usethis::use_data(annotated_hymns, overwrite = TRUE)
  }

