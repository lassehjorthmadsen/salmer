#' Get rhyme scheme for a given hymn
#'
#' @param ref_id
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
#' @examples
#' ## Not run
#' cut_up(annotated_hymns)
#'
#' @export
rhyme_scheme <- function(ref_id = 1, df, pron) {

  song <- df %>%
    tibble::rowid_to_column("row_id") %>%
    dplyr::filter(doc_id == ref_id)

  last_words <- song %>%
    dplyr::filter(upos != "PUNCT") %>%
    dplyr::group_by(paragraph_id) %>%
    dplyr::slice_tail(n = 1) %>%
    dplyr::mutate(rhymes = list(get_rhymes(token, pron))) %>%
    dplyr::ungroup()

  rhymes <- last_words %>%
    dplyr::mutate(lag1 = dplyr::lag(token, 1),
                  lag2 = dplyr::lag(token, 2),
                  lag3 = dplyr::lag(token, 3),
                  lag4 = dplyr::lag(token, 4)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(scheme = dplyr::case_when(lag1 %in% unlist(rhymes) ~ 1,
                                            lag2 %in% unlist(rhymes) ~ 2,
                                            lag3 %in% unlist(rhymes) ~ 3,
                                            lag3 %in% unlist(rhymes) ~ 4,
                                            TRUE ~ NA_real_)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(tok_row = row_number() - scheme,
                  must_rhyme = ifelse(is.na(tok_row), NA_character_, token[tok_row])) %>%
    dplyr::select(row_id, scheme, tok_row, must_rhyme)

  song %>%
    dplyr::left_join(rhymes, by = "row_id") %>%
    dplyr::select(-row_id)
}
