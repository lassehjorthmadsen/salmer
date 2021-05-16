#' Hymns in Danish, scrapped from www.dendanskesalmebogonline.dk
#' Each row corresponds to one line from one hymn, with information
#' on hymn number, title, author and more (see below).
#'
#'
#' @format A tibble with 24,715 rows and 9 variables:
#' \describe{
#'   \item{hymn_id}{Official hymn number}
#'   \item{title}{Title of hymn}
#'   \item{verse}{Number of verse}
#'   \item{text}{one line of text}
#'   \item{copyright}{first line of metadata: copyright holder}
#'   \item{melody1}{second line of metadata: usually melody}
#'   \item{melody2}{third line of metadata: sometimes name of composer}
#'   ...
#' }
"hymns"
