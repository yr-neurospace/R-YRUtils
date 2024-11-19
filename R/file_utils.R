#' Transfer files in various modes.
#'
#' @param from character vector, containing file names.
#' @param to same as `from`.
#' @param ops character vector, containing operations. If the
#'   previous one cannot be run successfully over all files,
#'   then the latter will be run until some operation can be run
#'   successfully, or it will raise an error.
#'
#' @returns A list containing `from` and `to`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' from <- c("file1.txt", "file2.txt", "file3.txt")
#' to <- c("temp1.txt", "temp2.txt", "temp3.txt")
#' file_transfer(from, to)
#' }
file_transfer <- function(from, to, ops = c("soft", "hard", "copy", "move")) {
    from <- normalizePath(from, mustWork = TRUE)
    to <- file.path(normalizePath(dirname(to), mustWork = TRUE), basename(to))

    for (op in ops) {
        if (op == "soft") {
            status <- file.symlink(from, to)
        } else if (op == "hard") {
            status <- file.link(from, to)
        } else if (op == "copy" || op == "move") {
            status <- file.copy(from, to, recursive = FALSE, copy.mode = TRUE, copy.date = FALSE)
        } else {
            stop("invalid ops (valid ops include 'soft', 'hard', 'copy', 'move')")
        }

        if (all(status)) {
            message("running in ", op, " mode successfully")
            break
        } else {
            file.remove(to[status])
        }
    }

    if (op == "move" && all(status)) {
        if (!all(file.remove(from))) {
            warning("when running in ", op, " mode, all source files have been copied successfully. But some source files could not be removed, you can remove them yourself if needed.")
        }
    }

    if (!all(status)) {
        stop("cannot be run successfully in any provided mode")
    }

    list(from = from, to = to)
}
