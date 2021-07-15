#' 'Collapses' a annotation back into text
#'
#' @description
#' Takes an annotation-style dataframe and concatenates tokens
#' back into lines of text
#'
#' @param df A tibble based on output from \code{udpipe::udpipe_annotate()}
#' which is assumed to contain \code{token}, \code{doc_id},
#' and \code{paragraph_id} columns.
#' @return A tibble with lines in column \code{text}.
#' @examples
#' ## Not run
#' collapse_annotation(df)
#'
collapse_annotation <- function(df) {

  fix_leading_spaces <- c(" ," = ",",
                          " !" = "!",
                          " :" = ":",
                          " ;" = ";",
                          " «" = "«",
                          "» " = "»",
                          " \\?" = "\\?",
                          " \\." = "\\.",
                          "\\\" " = "\\\"",
                          " \\\"" = "\\\"")
  out <- df %>%
    group_by(doc_id, paragraph_id) %>%
    summarise(text = paste(token, collapse = " ")) %>%
    ungroup() %>%
    mutate(text = str_replace_all(text, fix_leading_spaces))

  return(out)
}
