#' Print an atomic vector as your input.
#'
#' @param x an atomic vector.
#'
#' @returns An atomic vector as your input.
#'
#' @export
#'
#' @examples
#' echo_vec(letters)
echo_vec <- function(x) {
    if (!is.atomic(x)) {
        stop("Only accept an atomic vector")
    }

    y <- paste0('c("', paste0(x, collapse = '", "'), '")')
    message("Copy this: ", y)

    y
}
