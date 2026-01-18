tabUI_abbreviations <- function(id) {
    ns <- NS(id)

    list_items <- function(items) {
        tags$ul(lapply(items, tags$li))
    }

    disease_items <- c(
        "HC - Healthy Control",
        "DM - Dermatomyositis",
        "CLE - Cutaneous Lupus Erythematosus",
        "DLE - Discoid Lupus Erythematosus",
        "SLE - Systemic Lupus Erythematosus",
        "Pso - Psoriasis",
        "Vit - Vitiligo"
    )

    skin_items <- c(
        "H - Healthy",
        "NL - Non-lesional",
        "L - Lesional"
    )

    tissue_items <- c(
        "E - Epidermis",
        "D - Dermis"
    )

    cell_core_items <- c(
        "KC - Keratinocyte",
        "MC - Myeloid cell",
        "FB - Fibroblast",
        "EC - Endothelial cell",
        "SMC - Smooth muscle cell",
        "DC - Dendritic cell",
        "moDC - Monocyte-derived dendritic cell",
        "pDC - Plasmacytoid dendritic cell",
        "LC - Langerhans cell",
        "NK - Natural killer cell",
        "CD4 - CD4+ T cell",
        "CD8 - CD8+ T cell",
        "Treg - Regulatory T cell",
        "Tc17 - IL-17-producing cytotoxic T cell"
    )

    cell_subtype_items <- c(
        "KC_* - Keratinocyte subtypes (basal, spinous, granular, bulge, eccrine, sheath, etc.)",
        "FB_* - Fibroblast subtypes",
        "EC_* - Endothelial subtypes",
        "MC_* - Myeloid cell subtypes",
        "proInf - Pro-inflammatory",
        "sup / deep - Superficial / deep",
        "Tol - Tolerogenic",
        "unk - Unknown"
    )

    assay_items <- c(
        "scRNA-seq - Single-cell RNA sequencing",
        "bulk RNA-seq - Bulk RNA sequencing",
        "seqFISH - Sequential fluorescence in situ hybridization",
        "inDrop - Droplet-based microfluidics-based single-cell RNA-seq method",
        "10x - 10x Genomics; scRNA-seq platform used in this study",
        "NULISA - NULISA proteomics platform",
        "NULISAseq - Targeted panel built on the NULISA platform",
        "OLINK - Olink Proteomics platform",
        "GSE - GEO Series accession (Gene Expression Omnibus)"
    )

    metric_items <- c(
        "CPM - Counts per million",
        "log2(CPM + 1) - Log2-transformed counts per million",
        "FOV - Field of view",
        "pg/mL - Picograms per milliliter",
        "um - Micrometer (scalebar unit)"
    )

    tagList(
        bslib::card(
            bslib::card_header("Abbreviations"),
            bslib::card_body(
                tags$p("Common abbreviations used in the datasets, metadata, and plot labels.")
            )
        ),
        layout_columns(
            col_widths = c(6, 6),
            bslib::card(
                bslib::card_header("Clinical metadata"),
                bslib::card_body(
                    tags$h5("Disease types"),
                    list_items(disease_items),
                    tags$h5(class = "mt-3", "Skin condition"),
                    list_items(skin_items),
                    tags$h5(class = "mt-3", "Tissue type"),
                    list_items(tissue_items)
                )
            ),
            bslib::card(
                bslib::card_header("Cell types and subtypes"),
                bslib::card_body(
                    tags$h5("Core cell types"),
                    list_items(cell_core_items),
                    tags$h5(class = "mt-3", "Subtype prefixes and suffixes"),
                    list_items(cell_subtype_items)
                )
            )
        ),
        layout_columns(
            col_widths = c(6, 6),
            bslib::card(
                bslib::card_header("Assays and platforms"),
                bslib::card_body(
                    list_items(assay_items)
                )
            ),
            bslib::card(
                bslib::card_header("Plot labels and units"),
                bslib::card_body(
                    list_items(metric_items)
                )
            )
        )
    )
}

tabServer_abbreviations <- function(id, data_path) {
    moduleServer(id, function(input, output, session) {
        invisible(NULL)
    })
}
