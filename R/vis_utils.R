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
#' df <- data.frame(
#'     x = rnorm(100),
#'     y = rnorm(100)
#' )
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
                message("The input length is not equal to 2 or the input contains non-numeric characters or negative numbers.")
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

#' Provide an easy-to-use interface to add fonts
#'
#' You can add fonts from either `sysfonts::font_files()` or local font files
#' by giving `font_families`, and/or add google fonts by giving `google_fonts`
#' (see [Google fonts](https://fonts.google.com)).
#'
#' @param font_families a character vector or a data frame. If it is a character vector,
#'   then the font files will be retrieved from `sysfonts::font_files()`; otherwise the
#'   given data frame must have exactly the same forms as retrieved from `sysfonts::font_files()`,
#'   except that the `file` column must be absolute paths.
#' @param google_fonts a character vector. If a named vector is given, the names will be used as family names.
#' @param load_only logical. If `TRUE`, call `showtext_auto`.
#' @param ... parameters passed to `font_add_google()`.
#'
#' @returns NULL.
#'
#' @seealso For more info, refer to the `showtext` and `sysfonts` packages.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' set_fonts(font_families = c("Arial", "Times New Roman"))
#' set_fonts(google_fonts = c("bell" = "Schoolbell"))
#' }
set_fonts <- function(
    font_families = c("Arial", "Times New Roman"),
    google_fonts = NULL,
    load_only = FALSE,
    ...) {
    if (is.character(google_fonts)) {
        google_fonts <- trimws(google_fonts)
        for (i in seq_along(google_fonts)) {
            sysfonts::font_add_google(google_fonts[i],
                family = if (is.null(names(google_fonts[i])) || trimws(names(google_fonts[i])) == "") google_fonts[i] else trimws(names(google_fonts[i])),
                ...
            )
        }
    }

    if (!is.null(font_families)) {
        if (is.character(font_families)) {
            target_fonts_df <- dplyr::filter(sysfonts::font_files(), family %in% font_families)
        } else if ("data.frame" %in% class(font_families)) {
            target_fonts_df <- font_families
        } else {
            stop("font_families can either be a character vector or a data frame")
        }

        not_existed_font_families <- font_families[!(font_families %in% unique(target_fonts_df[["family"]]))]
        message("Font famalies NOT found in your system: ", paste0(not_existed_font_families, collapse = ", "))

        for (font_family in unique(target_fonts_df[["family"]])) {
            font_df <- dplyr::filter(target_fonts_df, family == font_family)
            if ("Regular" %in% font_df[["face"]]) {
                sysfonts::font_add(
                    family = font_family,
                    regular = font_df[["file"]][font_df[["face"]] == "Regular"],
                    bold = if ("Bold" %in% font_df[["face"]]) font_df[["file"]][font_df[["face"]] == "Bold"] else NULL,
                    italic = if ("Bold Italic" %in% font_df[["face"]]) font_df[["file"]][font_df[["face"]] == "Bold Italic"] else NULL,
                    bolditalic = if ("Italic" %in% font_df[["face"]]) font_df[["file"]][font_df[["face"]] == "Italic"] else NULL
                )
            } else {
                message("Font family ", font_family, " won't be added, because it doesn't have the regular face, which is mandatory")
            }
        }
    }

    if (!load_only) {
        showtext::showtext_auto()
        message("showtext_auto() has been called. You can use these fonts directly")
    } else {
        message("showtext_auto() has NOT been called yet. You can call showtext_auto() or showtext_begin() to make these fonts available")
    }

    message("Tips: you can call either showtext_auto()/showtext_auto(FALSE) or showtext_begin()/showtext_end() to make these newly added fonts available/unavailable")
}
