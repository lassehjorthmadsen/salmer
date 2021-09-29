#' Get rhyme scheme for a given hymn
#'
#' @param ref_id An integer number, refering to a hymn number
#' @param df tibble based on output from \code{udpipe::udpipe_annotate()}
#' similar to the \code{annotated_hymns} dataset.
#' @param pron A tibble assumed to contain a \code{token},
#' a \code{stress_vowel} and a \code{remainder} column, all
#' coded in then phonetic alphabet. Passed on to \code{get_rhymes()}.
#'
#' @return a tibble with rhyme structure in new column, \code{scheme}. The
#' number, x, in that column indicates that the word must rhyme with the x-
#' previous word. I.e "2" means that this must rhyme with the word two lines
#' above.
#'
#' @importFrom rlang .data
#'
#' @export
rhyme_scheme <- function(ref_id = 1, df, pron) {

  song <- df %>%
    tibble::rowid_to_column("row_id") %>%
    dplyr::filter(.data$doc_id == ref_id)

  last_words <- song %>%
    dplyr::filter(.data$upos != "PUNCT") %>%
    dplyr::group_by(.data$line_id) %>%
    dplyr::slice_tail(n = 1) %>%
    dplyr::mutate(rhymes = list(get_rhymes(.data$token, pron))) %>%
    dplyr::ungroup()

  rhymes <- last_words %>%
    dplyr::group_by(.data$verse) %>%
    dplyr::mutate(lag1 = dplyr::lag(.data$token, 1),
                  lag2 = dplyr::lag(.data$token, 2),
                  lag3 = dplyr::lag(.data$token, 3),
                  lag4 = dplyr::lag(.data$token, 4)) %>%
    dplyr::ungroup() %>%
    dplyr::rowwise() %>%
    dplyr::mutate(scheme = dplyr::case_when(lag1 %in% unlist(rhymes) ~ 1,
                                            lag2 %in% unlist(rhymes) ~ 2,
                                            lag3 %in% unlist(rhymes) ~ 3,
                                            lag3 %in% unlist(rhymes) ~ 4,
                                            TRUE ~ NA_real_)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(tok_row = dplyr::row_number() - .data$scheme,
                  must_rhyme = ifelse(is.na(.data$tok_row), NA_character_, .data$token[.data$tok_row])) %>%
    dplyr::select(.data$row_id, .data$scheme, .data$tok_row, .data$must_rhyme)

  song %>%
    dplyr::left_join(rhymes, by = "row_id") %>%
    dplyr::select(-.data$row_id)
}
