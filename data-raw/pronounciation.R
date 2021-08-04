library(dplyr)
library(stringr)


if (!file.exists("data/pronounciations.rda")) {

  tmpdir <- tempdir()

  url <- "https://www.nb.no/sbfil/leksikalske_databaser/leksikon/da_leksikon.tar.gz"
  file <- basename(url)
  download.file(url, file)

  untar(file, exdir = tmpdir )
  files <- list.files(tmpdir, full.names = TRUE, recursive = TRUE)

  df <- read.delim(files[1], sep = ";", quote = "", header = FALSE, skip = 3)

  pronounciations <- df %>%
    select(token = V1, pronounce = V12) %>%
    filter(!str_detect(token, "_")) %>%
    mutate(vowels = str_count(token, "a|e|i|o|u|y|æ|ø|å"),
           dollars = str_count(pronounce, "\\$"),
           ending1 = str_extract(pronounce, "\\$[^\\$]+$"),
           ending2 = str_extract(pronounce, "\\$[^\\$]+\\$[^\\$]+$")) %>%
    as_tibble()

  # use data in package
  usethis::use_data(pronounciations, overwrite = TRUE)
}


# Test some (possible) rhyms:
rhyms <- pronounciations %>%
  filter(!is.na(ending1)) %>%
  slice_sample(n = 100) %>%
  left_join(select(pron, ryhme = token, ending1), by = "ending1") %>%
  group_by(token) %>%
  slice_sample(n = 10) %>%
  ungroup()
