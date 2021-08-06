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
#'   \item{verse}{Number of verse}
#'   \item{text}{one line of text}
#'   \item{copyright}{first line of metadata: copyright holder}
#'   \item{melody1}{second line of metadata: usually melody}
#'   \item{melody2}{third line of metadata: sometimes name of composer}
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


#' Pronunciation Lexicon for Danish
#'
#' A collection of Danish words with pronounciations available at
#' \href{https://sprogteknologi.dk/dataset/nst-lexical-database-for-danish}{sprogteknologi.dk}
#' or from
#' \href{https://www.nb.no/sprakbanken/ressurskatalog/oai-nb-no-sbr-26}{den norske sprogbank i Nationalbiblioteket}
#' where documentation is also available.
#' Each row corresponds to one Danish word with phonetic annotation using
#' \href{https://www.phon.ucl.ac.uk/home/sampa/index.html}{sampa}
#' -- a computer readable phonetic alphabet.
#'
#' @format A tibble with 226,238 rows and 5 variables:
#' \describe{
#'   \item{token}{word in Danish (mostly)}
#'   \item{sampa}{Pronounciation in sampa phonetic alphabet}
#'   \item{rhyme_part}{The part of pronounciation relevant for rhymes: Stressed syllable and everyhing after}
#'   \item{stress_vowel}{Stressed syllable starting with first vowel}
#'   \item{remainder}{Everything in the sampa string field after stressed syllable}
#' }
"pronounciations"
