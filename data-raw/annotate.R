library(udpipe)
library(dplyr)
library(stringr)
library(tibble)

devtools::load_all()

model_files <- list.files("data-raw", "*.udpipe", full.names = TRUE)

if (length(model_files) > 0) {
  mdl <- udpipe_load_model(model_files[1])
} else {
  location <- udpipe_download_model(language = "danish-ddt", model_dir = "data-raw")
  mdl <- udpipe_load_model(location$file_model)
}

if (!file.exists("data/annotated_hymns.rda")) {
  annotated_hymns <- udpipe_annotate(object = mdl,
                                     x = hymns$text,
                                     doc_id = as.character(hymns$doc_id)) %>%
    as_tibble() %>%
    mutate(vowels = str_count(token, "a|e|i|o|u|y|æ|ø|å")) %>%
    mutate(across(c(doc_id, token_id, head_token_id), as.integer)) %>%
    select(-sentence_id, -sentence, -xpos, -feats, -head_token_id, -dep_rel, -deps, -misc)

  # use data in package
  usethis::use_data(annotated_hymns, overwrite = TRUE)
  }
