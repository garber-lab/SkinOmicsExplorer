tabUI_CLE_gse179633 <- function(id){
    ns <- NS(id)
    tagList(
        layout_columns(
            bslib::card(
                bslib::card_header("Seurat Embedding Plot"),
                bslib::card_body(modUI_SeuratEmbeddingPlot(
                    ns("dimplot"),
                    width_default = 6,
                    height_default = 6,
                    format_default = "png"
                ))
            ),
            bslib::card(
                bslib::card_header("Seurat Feature Plot"),
                bslib::card_body(modUI_SeuratFeaturePlot(
                    ns("featureplot"),
                    width_default = 6,
                    height_default = 6,
                    format_default = "png"
                ))
            ),
            col_width = c(6,6)
        ),
        card(
            bslib::card_header("Violin plot for normalized gene expression in all cell types"),
            bslib::card_body(
                modUI_SeuratVlnPlot(
                    ns("vlnplot_celltype"),
                    width_default = 15,
                    height_default = 4.5,
                    format_default = "png",
                    allow_subset = TRUE
                )
            )
        ),
        bslib::card(
            bslib::card_header("Heatmap for pseudo-bulked gene expression"),
            bslib::card_body(
                modUI_PseudoBulkHeatmap_bin(
                    ns("pseudoBulk_heatmap2"),
                    width_default = 6,
                    height_default = 10,
                    format_default = "pdf",
                    allow_subset = TRUE,
                    show_bins_toggle = TRUE,
                    show_bins_label = "Show transition"
                )
            )
        ),
        layout_columns(
            bslib::card(
                bslib::card_header("Violin plot for normalized gene expression by disease and tissue type"),
                bslib::card_body(
                    modUI_SeuratVlnPlot(
                        ns("vlnplot_tissueType_disease"),
                        width_default = 6,
                        height_default = 4.5,
                        format_default = "png",
                        allow_subset = FALSE
                    )
                )
            ),
            bslib::card(
                bslib::card_header("Heatmap for pseudo-bulked gene expression"),
                bslib::card_body(
                    modUI_PseudoBulkHeatmap_bin(
                        ns("pseudoBulk_heatmap"),
                        width_default = 6,
                        height_default = 10,
                        format_default = "pdf",
                        allow_subset = TRUE
                    )
                )
            ),
            col_width = c(6,6)
        )
    )
}

tabServer_CLE_gse179633 <- function(id, data_path, active_tab){
    moduleServer(id, function(input, output, session){
        dataname <- "CLE_gse179633"

        is_active <- reactive(identical(active_tab(), "CLE_gse179633"))
        srt <- reactiveVal(NULL)
        bulk_tb <- reactiveVal(NULL)

        observeEvent(is_active(), {
            if (!isTRUE(is_active())) return()
            if (is.null(srt()) || is.null(bulk_tb())) {
                progress <- Progress$new(session, min = 0, max = 1)
                on.exit(progress$close())

                progress$set(message = "Loading data", detail = "Estimated ~10 seconds")
                if (is.null(srt())) {
                    srt(readRDS(paste0(data_path(), "CLE_gse179633/CLE_gse179633_seuratObject_shiny.rds")))
                }

                progress$set(value = 0.7)
                if (is.null(bulk_tb())) {
                    bulk_tb(readRDS(paste0(data_path(), "CLE_gse179633/CLE_gse179633_pseudobulk_sum_CellSubtype_CellType_Disease_tissueType_bin30.rds")))
                }
                progress$set(value = 1)
            }
        }, ignoreInit = TRUE)
        bulk_meta <- reactive({
            req(bulk_tb(), srt())
            df <- as.data.frame(do.call(rbind, strsplit(colnames(bulk_tb()), ":")))
            colnames(df) <- c("CellSubtype","CellType","Disease","tissueType")
            rownames(df) <- colnames(bulk_tb())
            bin_pattern <- "_bin[0-9]+$"
            has_bin <- grepl(bin_pattern, df$CellSubtype)
            df$bin <- NA
            df$bin[has_bin] <- sub("^.*_bin", "bin", df$CellSubtype[has_bin])
            df$CellSubtype[has_bin] <- sub(bin_pattern, "", df$CellSubtype[has_bin])
            obj <- srt()
            df$CellSubtype <- factor(df$CellSubtype, levels = levels(obj$CellSubtype))
            df$CellType <- factor(df$CellType, levels = levels(obj$CellType))
            df$Disease <- factor(df$Disease, levels = levels(obj$Disease))
            df$tissueType <- factor(df$tissueType, levels = levels(obj$tissueType))
            return(df)
        })

        modServer_SeuratEmbeddingPlot(
            id = "dimplot",
            srt = srt,
            groupby_default = "CellType",
            dataname = dataname,
            raster = FALSE
        )

        modServer_SeuratFeaturePlot(
            id = "featureplot",
            srt = srt,
            dataname = dataname,
            raster = FALSE,
            feature_default = "IFNG"
        )

        modServer_SeuratVlnPlot(
            id = "vlnplot_celltype",
            srt = srt,
            dataname = dataname,
            groupby_column = "CellSubtype",
            splitby_column = "CellType",
            subsetby_columns = c("Disease", "tissueType"),
            feature_default = "IFNG"
        )

        modServer_PseudoBulkHeatmap_bin(
            id = "pseudoBulk_heatmap2",
            bulk_tb = bulk_tb,
            bulk_meta = bulk_meta,
            dataname = dataname,
            groupby_column = "CellSubtype",
            splitby_column = "CellType",
            subsetby_columns = c("Disease"),
            show_bins_toggle = TRUE
        )

        modServer_SeuratVlnPlot(
            id = "vlnplot_tissueType_disease",
            srt = srt,
            dataname = dataname,
            groupby_column = "tissueType",
            splitby_column = "Disease",
            subsetby_columns = c("CellType"),
            feature_default = "IFNG"
        )
        
        modServer_PseudoBulkHeatmap_bin(
            id = "pseudoBulk_heatmap",
            bulk_tb = bulk_tb,
            bulk_meta = bulk_meta,
            dataname = dataname,
            groupby_column = "tissueType",
            splitby_column = "Disease",
            subsetby_columns = c("CellType"),
            show_bins_default = FALSE,
            show_bins_toggle = FALSE
        )
        invisible(list(srt = srt))
    })
}
