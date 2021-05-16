content_to_tibble <- function(content) {
  # Extract elements from html content and build tibble

  title <- content %>%
    html_elements(".salme-navn") %>%
    html_text2() %>%
    tibble(.name_repair = function (x) "title")

  meta <- content %>%
    html_elements(".salme-melodi") %>%
    html_text2() %>%
    tibble(.name_repair = function (x) "meta") %>%
    separate(meta,
             into = c("copyright", "melody1", "melody2"),
             sep = "\n\r|\n",
             fill = "right",
             extra = "drop") %>%
    mutate(copyright = str_remove(copyright, "[©] "),
           melody1 = str_remove(melody1, "Mel.: "),
           melody2 = str_trim(melody2))

  verses <- content %>%
    html_elements(".salme-vers-tekst") %>%
    html_text2() %>%
    str_split("\n") %>%
    map(tibble, .name_repair = function (x) "text") %>%
    bind_rows(.id = "verse") %>%
    bind_rows(tibble("text" = "")) # If no verses found, empty line

  author <- content %>%
    html_elements(".salme-forfatter:nth-child(2)") %>%
    html_text2() %>%
    str_subset("") %>%
    tibble(.name_repair = function (x) "author") %>%
    bind_rows(tibble("author" = NA)) %>% # If no author found, use NA
    arrange() %>%
    slice_head(n = 1)

  no_verses <- content %>%
    html_elements(".salme-vers-nr p") %>%
    html_text2() %>%
    as.numeric() %>%
    append(0) %>% # If no verse number found, use 0 for max() to work
    max(na.rm = T) %>%
    tibble(.name_repair = function (x) "no_verses") %>%
    mutate(no_verses = na_if(no_verses, 0))

  bind_cols(title, verses, author, meta, no_verses) %>%
    filter(text != "" | is.na(no_verses)) %>%
    return()
}
