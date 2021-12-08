#' 'Cut-up' a hymn (or something else)
#'
#' @description
#' Using the cut-up technique popularized by William S. Burroughs,
#' \code{cut_up()} replaces each word in the reference document with
#' a random sample of words with same number of wovels
#' and same part-of-speach-tag.
#'
#' @param hymn A tokenized hymn that needs new rhymes, possible output
#' from \code{salmer::cut_up()} in the same format as \code{annotated_hymns}
#' @param df A tibble based on output from \code{udpipe::udpipe_annotate()}
#'
#' @return A tibble with new column \code{token_new} with a suggested
#' replacement for column \code{token}.
#'
#' @examples
#' ## Not run
#' hymn <- annotated_hymns %>% dplyr::filter(doc_id == 1)
#' new_rhymes(hymn)
#'
#' @export
new_rhymes <- function(hymn, df = annotated_hymns) {

  # All tokens from hymns, with rhyme info
  annotated_tokens <- annotated_hymns %>%
    filter(!is.na(.data$stress_vowel)) %>%
    distinct(token_new = tolower(.data$token), .data$vowels, .data$stress_vowel, .data$remainder)

  # Pronunciation info for relevant tokens
  pr <- annotated_tokens %>%
    select(-.data$vowels) %>%
    inner_join(distinct(hymn, .data$token_new), by = "token_new")

  # Filter out all last words in our cut-up
  last_words <- hymn %>%
    rowid_to_column("row_id") %>%
    filter(upos != "PUNCT") %>%
    group_by(.data$line_id) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    select(.data$row_id,
           token = .data$token_new,
           .data$vowels,
           .data$rhyme_scheme) %>%
    mutate(token = tolower(.data$token))

  # Get rhyming contrains, if any
  new_last_words <- last_words %>%
    mutate(
      must_rhyme_with = case_when(
        rhyme_scheme == 1 ~ lag(.data$token, 1),
        rhyme_scheme == 2 ~ lag(.data$token, 2),
        rhyme_scheme == 3 ~ lag(.data$token, 3),
        rhyme_scheme == 4 ~ lag(.data$token, 4),
        TRUE ~ NA_character_)
    ) %>%
    left_join(pr, by = c("must_rhyme_with" = "token_new")) %>%
    filter(!is.na(.data$rhyme_scheme)) %>%
    left_join(annotated_tokens,
              by = c("vowels" = "vowels",
                     #"upos" = "upos", We don't constrain on upos for rhymes
                     "stress_vowel" = "stress_vowel",
                     "remainder" = "remainder")) %>%
    filter(.data$must_rhyme_with != .data$token) %>%
    group_by(.data$row_id) %>%
    filter(.data$token != "") %>%
    slice_sample(n = 1) %>%
    ungroup()

  # Fix rhymes
  final <- hymn %>%
    left_join(select(new_last_words, .data$row_id, token_replace = .data$token_new), by = c("token_no" ="row_id")) %>%
    mutate(token_new = coalesce(.data$token_replace, .data$token_new))

  return(final)
}
