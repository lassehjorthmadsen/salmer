#' 'Cut-up' a hymn (or something else)
#'
#' @description
#' Using the cut-up technique popularized by William S. Burroughs,
#' \code{cut_up()} replaces each word in the reference document with
#' a random sample of words with same number of wovels
#' and same part-of-speach-tag.
#'
#' @param df A tibble based on output from \code{udpipe::udpipe_annotate()}
#' which is assumed to also contain a \code{wovel} column.
#' @param ref_id An integer number; this is the text that
#' will get new words.
#' @param except character. upos tags that will not be cut-up.
#' @param token Name of column containing tokens
#' @param token_no Name of column containing token ids
#' @param doc_id Name of column containing document ids
#' @param vowels Name of column containing number of wovels in token
#' @param upos Name of column containing POS-tag
#' @param token_new Name of column to contain a new, suggested token
#'
#' @return A tibble with new column \code{token_new} with a suggested
#' replacement for column \code{token}.
#' @examples
#' ## Not run
#' cut_up(annotated_hymns)
#'
#' @export
cut_up <- function(df,
                   ref_id = 1,
                   except = NULL,
                   token = token,
                   token_no = token_no,
                   doc_id = doc_id,
                   vowels = vowels,
                   upos = upos,
                   token_new = token_new) {

  # For each token, sample 1 out of all possible matches
  out <- dplyr::filter(df, doc_id == ref_id) %>%
    tibble::rowid_to_column("token_no") %>%
    dplyr::left_join(dplyr::select(df, token, upos, vowels),
              by = c("upos", "vowels"),
              suffix = c("", "_new")) %>%
    dplyr::group_by(token_no) %>%
    dplyr::slice_sample(n = 1) %>%
    dplyr::ungroup()

  # Exceptions: Keep the original for these upos values
  out <- out %>%
    dplyr::mutate(token_new = ifelse(upos %in% except, token, token_new))

  return(out)
}
