#' temp
#'
#' @description temp
#'
#' @param x tibble
#' @param y tibble
#' @param bj character
#'
#' @return A charactor vector with possible rhyming words.
#'
#' @export
temp <- function(x, y, bj) {
    x %>% dplyr::left_join(y, by = bj) %>% dplyr::filter(.[[1]] > 7)
}
