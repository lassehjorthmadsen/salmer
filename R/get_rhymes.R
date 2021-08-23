#' Find possible rhymes for a given word
#'
#' @description
#' Given a pronounciation dictionary with columns for
#' tokens (words) and pronounciations, this function
#' returns a vector of possible rhymes.
#'
#' @param word character. Word to find rhymes for
#' @param pron A tibble assumed to contain a \code{token},
#' a \code{stress_vowel} and a \code{remainder} column, all
#' coded in then phonetic alphabet
#' \href{https://www.phon.ucl.ac.uk/home/sampa/index.html}{sampa}.
#' @param token character. Name of column containing token (word).
#' @param stress_vowel character. Name of column containing
#' pronounciation for the stressed vowel. Used for finding rhymes.
#' @param remainder character. Name of column containing
#' pronounciation for the remainer of the token, i.e. everything
#' *after* the stressed vowel. Used for finding rhymes.
#'
#' @return A charactor vector with possible rhyming words.
#'
#' @examples
#' pr <- data.frame(token = c("appellere", "akkumulere", "bygning"),
#' sampa = c("A$pE$\"le:?$6", "a$ku$mu$\"le:?$6", "\"by$gneN"),
#' rhyme_part = c("\"le:?$6", "\"le:?$6", "\"by$gneN"),
#' stress_vowel = c("e:?", "e:?", "y"),
#' remainder = c("$6", "$6", "$gneN"))
#'
#' get_rhymes(word = "appellere", pron = pr)
#'
#' @importFrom rlang .data
#'
#' @export
get_rhymes <- function(word,
                       pron,
                       token = token,
                       stress_vowel = stress_vowel,
                       remainder = remainder) {

  #token <- dplyr::enquo(token)
  word <- tolower(word)
  pron <- pron %>% dplyr::mutate(token = tolower({{token}}))

  pron %>%
    dplyr::filter({{token}} == word) %>%
    dplyr::distinct({{token}}, .keep_all = TRUE) %>%
    dplyr::left_join(dplyr::select(pron, {{token}}, {{stress_vowel}}, {{remainder}}),
            by = c("stress_vowel", "remainder")) %>%
    dplyr::filter(.data[["token.x"]] != .data[["token.y"]]) %>%
    dplyr::distinct(.data[["token.y"]]) %>%
    dplyr::arrange() %>%
    dplyr::pull()
}
