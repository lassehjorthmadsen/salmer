#' Hymns in Danish
#'
#' A dataset containing full text and metadata for 791 hymns officially
#' used in Danish churches. Data is scrapped from
#' \href{http://www.dendanskesalmebogonline.dk}{Den Danske Salmebog Online}.
#' Each row corresponds to one line from one hymn, with information
#' on hymn number, title, author and more (see below).
#'
#'
#' @format A tibble with 24,715 rows and 9 variables:
#' \describe{
#'   \item{doc_id}{Official hymn number}
#'   \item{title}{Title of hymn}
#'   \item{verse}{Verse number}
#'   \item{text}{one line of text}
#'   \item{copyright}{first line of metadata: copyright holder}
#'   \item{melody1}{second line of metadata: usually melody}
#'   \item{melody2}{third line of metadata: sometimes name of composer}
#'   \item{no_verses}{Total number of verses}
#'   \item{year}{Year the hymn was written, if known}
#'   \item{line_id}{line number}
#' }
"hymns"

#' Hymns, tokenized and POS-tagged
#'
#' The \code{hymn} dataset, tokenized and tagged with part-of-speach (POS).
#' Each line represent a word from a given hymn.
#'
#' @format A tibble with 153,717 rows and 7 variables:
#' \describe{
#'   \item{doc_id}{Official hymn number}
#'   \item{paragraph_id}{Line number in hymn}
#'   \item{token_id}{Token number in line}
#'   \item{token}{The original token, i.e. word}
#'   \item{lemma}{The lemmatized, i.e. dictionary form, of token}
#'   \item{upos}{POS-tag, i.e. part-of-speach, like VERB or PUNCT for punctuation}
#'   \item{vowels}{Number of vowels in token -- good for finding alternative words
#'       using the \code{cut_up()} function}
#' }
"annotated_hymns"
