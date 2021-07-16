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
#' @return A tibble with new column \code{token_new} with a suggested
#' replacement for column \code{token}.
#' @examples
#' ## Not run
#' cut_up(hymns)
#'
cut_up <- function(df, ref_id = 1, except = NULL) {

  # For each token, sample 1 out of all possible matches
  out <- filter(df, doc_id == ref_id) %>%
    rowid_to_column("token_no") %>%
    left_join(select(df, token, upos, vowels),
              by = c("upos", "vowels"),
              suffix = c("", "_new")) %>%
    group_by(token_no) %>%
    slice_sample(n = 1) %>%
    ungroup()

  # Exceptions: Keep the original for these upos values
  out <- out %>%
    mutate(token_new = ifelse(upos %in% except, token, token_new))

  return(out)
}
