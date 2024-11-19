#' Extract gene IDs from GTF/GFF file.
#'
#' @param gff_file character, path to GTF/GFF file.
#' @param target_type character, which type (gene or transcript) to be extracted.
#'
#' @returns A data frame.
#'
#' @export
#'
#' @importFrom magrittr %>%
extract_gene_id_from_gff <- function(gff_file, target_type = "gene") {
    gene_cols <- list(
        seqnames = c("seqnames"),
        gene_type = c("type"),
        gene_id = c("gene_id", "ID"),
        gene_name = c("gene_name", "Name"),
        gene_biotype = c("gene_type", "gene_biotype", "biotype"),
        gene_version = c("gene_version", "version"),
        gene_level = c("level")
    )
    transcript_cols <- list(
        seqnames = c("seqnames"),
        transcript_type = c("type"),
        transcript_id = c("transcript_id", "ID"),
        transcript_name = c("transcript_name", "Name"),
        transcript_biotype = c("transcript_type", "biotype"),
        transcript_version = c("version"),
        transcript_support_level = c("transcript_support_level"),
        gene_id = c("gene_id", "Parent")
    )

    if (!(target_type %in% c("gene", "transcript"))) {
        stop("invalid target_type")
    }

    df <- rtracklayer::import(gff_file) %>%
        as.data.frame() %>%
        tibble::as_tibble()

    gene_anno <- df %>%
        dplyr::filter(stringr::str_detect(df[["type"]], ".*gene$"))

    actual_gene_cols <- c()
    for (name in names(gene_cols)) {
        for (item in gene_cols[[name]]) {
            if (item %in% names(gene_anno)) {
                actual_gene_cols <- c(actual_gene_cols, stats::setNames(item, name))
                break
            }
        }
    }

    gene_anno <- gene_anno[, actual_gene_cols]
    names(gene_anno) <- names(actual_gene_cols)
    gene_anno[["gene_id"]] <- gsub("^gene:", "", gene_anno[["gene_id"]])

    if (any(stringr::str_detect(gene_anno[["gene_id"]], "^[a-zA-Z0-9]+\\.[0-9]+$")) && !("gene_version" %in% names(gene_anno))) {
        gene_anno <- gene_anno %>%
            tidyr::separate_wider_delim(
                cols = "gene_id",
                delim = ".",
                names = c("gene_id_without_version", "gene_version"),
                cols_remove = FALSE
            )
    }

    if (target_type == "transcript") {
        transcript_anno <- df %>%
            dplyr::filter(stringr::str_detect(df[["type"]], ".*transcript$|.*RNA$"))

        actual_transcript_cols <- c()
        for (name in names(transcript_cols)) {
            for (item in transcript_cols[[name]]) {
                if (item %in% names(transcript_anno) && !all(is.na(transcript_anno[[item]]))) {
                    actual_transcript_cols <- c(actual_transcript_cols, stats::setNames(item, name))
                    break
                }
            }
        }

        transcript_anno <- transcript_anno[, actual_transcript_cols]
        names(transcript_anno) <- names(actual_transcript_cols)
        transcript_anno[["transcript_id"]] <- gsub("^transcript:", "", transcript_anno[["transcript_id"]])
        transcript_anno[["gene_id"]] <- sapply(transcript_anno[["gene_id"]], function(x) {
            paste0(unlist(x), collapse = ";")
        })
        transcript_anno[["gene_id"]] <- gsub("^gene:", "", transcript_anno[["gene_id"]])

        if (any(stringr::str_detect(transcript_anno[["transcript_id"]], "^[a-zA-Z0-9]+\\.[0-9]+$")) && !("transcript_version" %in% names(transcript_anno))) {
            transcript_anno <- transcript_anno %>%
                tidyr::separate_wider_delim(
                    cols = "transcript_id",
                    delim = ".",
                    names = c("transcript_id_without_version", "transcript_version"),
                    cols_remove = FALSE
                )
        }

        transcript_anno <- dplyr::left_join(transcript_anno, gene_anno, by = c("seqnames", "gene_id"))

        return(dplyr::distinct(transcript_anno))
    }

    dplyr::distinct(gene_anno)
}
