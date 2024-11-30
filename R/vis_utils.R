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

#' Group GO terms via hierarchical clustering
#'
#' This function first calculates a dissimilarity matrix via `cluster::daisy` from an asymmetric binary matrix,
#' in which rows are terms and columns are genes and each value can be either
#' 1 (the term contains this gene) or 0 (vice versa). Then this dissimilarity matrix
#' is used to perform hierarchical clustering using `cluster::agnes`. Finally,
#' we cut the dendrogram based on a given height threshold into modules. In each module,
#' terms have a significantly high number of common genes, compared to other terms, not in the same module.
#' This means that terms in the same module may be redundant and you can only pick some of them
#' based on your scientific question for further analysis.
#'
#' @param df a data frame. At present, we assume that you should get it from the enriched results of clusterProfiler.
#' @param out_file_prefix a character string. This should contain both the output directory and the output file prefix.
#' @param x_column which column in `df` is used as the x-axis of the dot plot.
#' @param fill_column which column in `df` is used as the `fill` aesthetics in dot plot.
#' @param size_column which column in `df` is used as the `size` aesthetics in dot plot.
#' @param heatmap_data which type of data should be used to plot heatmap. Can be either
#'   `sim` (using similarity matrix) or `dissim` (using dissimilarity matrix).
#' @param heatmap_width the maximum width of heatmap in inches.
#' @param heatmap_height the maximum height of heatmap in inches.
#' @param cut_hc_height the height threshold for cutting the dendrogram.
#'
#' @returns A list containing all data/plot objects used/generated.
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom grDevices dev.off pdf
group_enriched_terms <- function(df,
                                 out_file_prefix = "",
                                 x_column = "GeneRatio",
                                 fill_column = "p.adjust",
                                 size_column = "Count",
                                 heatmap_data = c("sim", "dissim"),
                                 heatmap_width = 500,
                                 heatmap_height = 500,
                                 cut_hc_height = 0.25) {
    df <- df %>%
        dplyr::mutate(GeneRatio = sapply(GeneRatio, function(x) {
            x <- as.numeric(strsplit(x, split = "/", fixed = TRUE)[[1]])
            x[1] / x[2]
        }))

    gen_des_df <- apply(dplyr::distinct(df[, c("Description", "geneID")]), 1, function(x) {
        tibble::tibble(
            geneID = strsplit(x["geneID"], split = "/", fixed = TRUE)[[1]],
            Description = x["Description"],
            value = 1
        )
    }) %>%
        do.call(rbind, .) %>%
        tidyr::pivot_wider(
            id_cols = "geneID", names_from = "Description",
            values_from = "value", values_fill = 0
        ) %>%
        as.data.frame()
    row.names(gen_des_df) <- gen_des_df[["geneID"]]
    gen_des_df <- gen_des_df[, names(gen_des_df) != "geneID"]

    mat <- t(as.matrix(gen_des_df))

    dissim_mat <- cluster::daisy(mat, type = list(asymm = 1:ncol(mat)))
    hc <- cluster::agnes(dissim_mat)

    n <- attr(dissim_mat, "Size")
    ordered_labels <- hc$order.lab

    df[["Description"]] <- factor(df[["Description"]], levels = rev(ordered_labels))
    p_dotplot <- ggplot2::ggplot(df, ggplot2::aes(.data[[x_column]], Description,
        color = .data[[fill_column]],
        size = .data[[size_column]]
    )) +
        ggplot2::geom_point() +
        ggplot2::scale_color_gradient(low = "#e06663", high = "#327eba") +
        ggplot2::scale_size(range = c(3, 8)) +
        ggplot2::theme_bw()
    while (TRUE) {
        ppreview(p_dotplot, file = paste0(out_file_prefix, "dotplot.pdf"))

        message(
            "Now you can open the saved figure file to see whether the figure size is indeed OK. ",
            "If not, you can adjust it again."
        )
        exit_flag <- readline(prompt = "Is the image size indeed OK? [Y]:")
        if (exit_flag == "Y") {
            break
        }
    }

    ordered_dissim_mat <- as.matrix(dissim_mat)[ordered_labels, ordered_labels]
    for (j in 1:(n - 1)) {
        for (i in (j + 1):n) {
            ordered_dissim_mat[i, j] <- NA
        }
    }

    ordered_sim_mat <- matrix(
        nrow = n, ncol = n,
        dimnames = list(ordered_labels, ordered_labels)
    )
    for (i in 1:(n - 1)) {
        for (j in (i + 1):n) {
            # Jaccard similarity coefficient
            # intersection / union
            ordered_sim_mat[i, j] <- sum(gen_des_df[[ordered_labels[i]]] & gen_des_df[[ordered_labels[j]]]) / sum(gen_des_df[[ordered_labels[i]]] | gen_des_df[[ordered_labels[j]]])
        }
    }
    for (i in 1:n) {
        ordered_sim_mat[i, i] <- 1
    }

    while (TRUE) {
        p_dendrogram <- factoextra::fviz_dend(hc,
            show_labels = FALSE,
            h = cut_hc_height, lwd = 0.5
        ) +
            ggplot2::geom_hline(yintercept = cut_hc_height) +
            ggplot2::labs(title = NULL)
        ppreview(p_dendrogram, file = paste0(out_file_prefix, "dendrogram.pdf"))

        exit_flag <- readline(prompt = "Is the cut height OK? [Y]:")
        if (exit_flag == "Y") {
            break
        } else {
            while (TRUE) {
                cut_hc_height <- as.numeric(readline("Please enter new cut height:"))
                if (!is.na(cut_hc_height)) {
                    break
                }
            }
        }
    }

    clusters <- dendextend::cutree(hc, h = cut_hc_height)
    clusters <- clusters[ordered_labels]
    clusters_df <- data.frame(Description = names(clusters), Cluster = as.character(clusters))

    if (heatmap_data[1] == "sim") {
        hm_mat <- ordered_sim_mat
        color_fun <- circlize::colorRamp2(c(0, 1), c("white", "red"))
    } else if (heatmap_data[1] == "dissim") {
        hm_mat <- ordered_dissim_mat
        color_fun <- circlize::colorRamp2(c(0, 1), c("red", "white"))
    } else {
        stop("unsupported heatmap_data value")
    }
    p_heatmap <- ComplexHeatmap::Heatmap(hm_mat,
        cluster_rows = FALSE,
        cluster_columns = FALSE,
        cluster_row_slices = FALSE,
        cluster_column_slices = FALSE,
        row_order = colnames(hm_mat),
        column_order = row.names(hm_mat),
        row_split = factor(as.character(clusters),
            levels = unique(as.character(clusters))
        ),
        column_split = factor(as.character(clusters),
            levels = unique(as.character(clusters))
        ),
        col = color_fun,
        na_col = "white",
        show_row_names = TRUE,
        show_column_names = TRUE,
        show_row_dend = FALSE,
        show_column_dend = FALSE,
        column_names_rot = 45,
        row_names_side = "right",
        column_names_side = "top",
        width = ncol(hm_mat) * grid::unit(12, "mm"),
        height = nrow(hm_mat) * grid::unit(12, "mm"),
        row_names_gp = grid::gpar(fontsize = 24),
        column_names_gp = grid::gpar(fontsize = 24),
        row_title_side = "left",
        column_title_side = "bottom",
        border = TRUE,
        border_gp = grid::gpar(col = "grey90"),
        row_gap = grid::unit(3, "mm"),
        column_gap = grid::unit(3, "mm")
    )
    pdf(
        file = ifelse(heatmap_data[1] == "sim",
            paste0(out_file_prefix, "sim_heatmap.pdf"),
            paste0(out_file_prefix, "dissim_heatmap.pdf")
        ),
        width = heatmap_width, height = heatmap_height
    )
    print(p_heatmap)
    dev.off()
    message(
        "Heatmap is saved in ",
        ifelse(heatmap_data[1] == "sim",
            paste0(out_file_prefix, "sim_heatmap.pdf"),
            paste0(out_file_prefix, "dissim_heatmap.pdf")
        ),
        " with predefined size. If the size is not OK, you can save the heatmap object again yourself."
    )

    list(
        df = df,
        gen_des_df = gen_des_df,
        hc = hc,
        clusters_df = clusters_df,
        ordered_sim_mat = ordered_sim_mat,
        ordered_dissim_mat = ordered_dissim_mat,
        p_dotplot = p_dotplot,
        p_dendrogram = p_dendrogram,
        p_heatmap = p_heatmap
    )
}
