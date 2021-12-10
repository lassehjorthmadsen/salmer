#' Find rhymes for a hymn (or something else)
#'
#' @description
#' Similar to \code{cut_up()}, \code{new_rhymes()} takes
#' a piece of tokenized text input, and replaces words in
#' rhyme scheme with words that rhyme.
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
#' new_rhymes(hymn = cut_up(annotated_hymns[1:32, ]), df = annotated_hymns)
#'
#' @export
new_rhymes <- function(hymn, df) {

  # All tokens from hymns, with rhyme info
  annotated_tokens <- df %>%
    dplyr::filter(!is.na(.data$stress_vowel)) %>%
    dplyr::distinct(token_new = tolower(.data$token), .data$vowels, .data$stress_vowel, .data$remainder)

  # Pronunciation info for relevant tokens
  pr <- annotated_tokens %>%
    dplyr::select(-.data$vowels) %>%
    dplyr::inner_join(dplyr::distinct(hymn, .data$token_new), by = "token_new")

  # Filter out all last words in our cut-up
  last_words <- hymn %>%
    tibble::rowid_to_column("row_id") %>%
    dplyr::filter(.data$upos != "PUNCT") %>%
    dplyr::group_by(.data$line_id) %>%
    dplyr::slice_tail(n = 1) %>%
    dplyr::ungroup() %>%
    dplyr::select(.data$row_id,
           token = .data$token_new,
           .data$vowels,
           .data$rhyme_scheme) %>%
    dplyr::mutate(token = tolower(.data$token))

  # Get rhyming contrains, if any
  new_last_words <- last_words %>%
    dplyr::mutate(
      must_rhyme_with = dplyr::case_when(
        rhyme_scheme == 1 ~ lag(.data$token, 1),
        rhyme_scheme == 2 ~ lag(.data$token, 2),
        rhyme_scheme == 3 ~ lag(.data$token, 3),
        rhyme_scheme == 4 ~ lag(.data$token, 4),
        TRUE ~ NA_character_)
    ) %>%
    dplyr::left_join(pr, by = c("must_rhyme_with" = "token_new")) %>%
    dplyr::filter(!is.na(.data$rhyme_scheme)) %>%
    dplyr::left_join(annotated_tokens,
              by = c("vowels" = "vowels",
                     #"upos" = "upos", We don't constrain on upos for rhymes
                     "stress_vowel" = "stress_vowel",
                     "remainder" = "remainder")) %>%
    dplyr::filter(.data$must_rhyme_with != .data$token) %>%
    dplyr::group_by(.data$row_id) %>%
    dplyr::filter(.data$token != "") %>%
    dplyr::slice_sample(n = 1) %>%
    dplyr::ungroup()

  # Fix rhymes
  final <- hymn %>%
    dplyr::left_join(dplyr::select(new_last_words, .data$row_id, token_replace = .data$token_new), by = c("token_no" ="row_id")) %>%
    dplyr::mutate(token_new = dplyr::coalesce(.data$token_replace, .data$token_new))

  return(final)
}
