#' Preview an R plot and resize it before saving it interactively
#' 
#' The plot object is previewed as a PNG format, and then 
#' if the plot size is nice, it will be saved in the given format, 
#' inferred from the file extension.
#' 
#' @param plot a plot object.
#' @param file file path to save plot.
#' @param width the width of the plot in pixels (1200 by default).
#' @param height the height of the plot in pixels (1200 by default).
#' 
#' @returns NULL.
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' library(ggplot2)
#' 
#' df <- data.frame(x = rnorm(100),
#'                  y = rnorm(100))
#' 
#' p <- ggplot(df, aes(x, y)) +
#'     geom_point()
#' 
#' ppreview(p, "test.pdf")
#' }
ppreview <- function(
    plot, file,
    width = 1200, height = 1200) {
    if (width <= 0 || height <= 0) {
        stop("Both width and height must be positive numbers")
    }

    while (TRUE) {
        plot_source <- unigd::ugd_render_inline(
            {
                print(plot)
            },
            width = width,
            height = height,
            as = "png"
        )
        plot_image <- png::readPNG(plot_source, native = FALSE, info = TRUE)
        grid::grid.newpage()
        grid::grid.raster(plot_image)

        flag <- readline("Is the image size OK? [Y]:")
        if (flag == "Y") {
            break
        }

        while (TRUE) {
            new_size <- readline("Please enter new width and height separated by '/':")
            new_size <- as.numeric(strsplit(new_size, split = "/", fixed = TRUE)[[1]])
            if (any(is.na(new_size)) || length(new_size) != 2 || any(new_size <= 0)) {
                warning("The input length is not equal to 2 or the input contains non-numeric characters or negative numbers.")
            } else {
                width <- new_size[1]
                height <- new_size[2]
                break
            }
        }
    }

    unigd::ugd_save_inline(
        {
            print(plot)
        },
        file = file,
        width = width,
        height = height
    )
    message("Plot has been saved in ", file)
}
