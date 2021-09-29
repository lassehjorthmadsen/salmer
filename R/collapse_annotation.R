#' 'Collapses' a annotation back into text
#'
#' @description
#' Takes an annotation-style dataframe and concatenates tokens
#' back into lines of text
#'
#' @param df A tibble based on output from \code{udpipe::udpipe_annotate()}
#' @param token Name of column containing tokens to be collapsed
#' @param doc_id Name of column containing document ids to group by
#' @param line_id Name of column containing line ids to group by
#' @param text Name of column to contain collapsed text
#'
#' @return A tibble with lines in column \code{text}.
#' @examples
#' ## Not run
#' collapse_annotation(annotated_hymns)
#'
#' @export
collapse_annotation <- function(df,
                                token = token,
                                doc_id = doc_id,
                                line_id = line_id,
                                text = text) {
  token <- dplyr::enquo(token)

  # Damn « and »
  fix_leading_spaces <- c(" ," = ",",
                          " !" = "!",
                          " :" = ":",
                          " ;" = ";",
                          " \\u00AB" = "\\u00AB", # «
                          "\\u00BB " = "\\u00BB", # »
                          " \\?" = "\\?",
                          " \\." = "\\.",
                          "\\\" " = "\\\"",
                          " \\\"" = "\\\"")
  out <- df %>%
    dplyr::group_by(doc_id, line_id) %>%
    dplyr::summarise(text = paste(!!token, collapse = " ")) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(text = stringr::str_replace_all(text, fix_leading_spaces))

  return(out)
}
