tabUI_DM_10x <- function(id){
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
            col_widths = c(6, 6)
        ),
        layout_columns(
            bslib::card(
                bslib::card_header("Violin plot for normalized gene expression in all cell subtypes"),
                bslib::card_body(
                    modUI_SeuratVlnPlot(
                        ns("vlnplot_cellsubtype"),
                        width_default = 5,
                        height_default = 4.5,
                        format_default = "png",
                        allow_subset = TRUE
                    )
                )
            ),
            bslib::card(
                bslib::card_header("Heatmap for pseudo-bulked gene expression by sample"),
                bslib::card_body(
                    modUI_PseudoBulkHeatmap(
                        ns("pseudoBulk_heatmap"),
                        width_default = 6,
                        height_default = 10,
                        format_default = "pdf",
                        allow_subset = TRUE
                    )
                )
            ),
            col_widths = c(6, 6)
        )
    )
}

tabServer_DM_10x <- function(id, data_path, active_tab){
    moduleServer(id, function(input, output, session){
        dataname <- "DM_10x"

        colors.ic <- c(
            "LC" = "#A35976",
            "DC1" = "#947065",
            "DC2" = "#DDCDAE",
            "moDC" = "#D7AABF",
            "MC_Tol" = "#8E7CA8",
            "Migratory" = "#486482",
            "pDC" = "#84A2E0"
        )

        is_active <- reactive(identical(active_tab(), "DM_10x"))
        srt <- reactiveVal(NULL)
        bulk_tb <- reactiveVal(NULL)

        observeEvent(is_active(), {
            if (!isTRUE(is_active())) return()
            if (is.null(srt()) || is.null(bulk_tb())) {
                if (is.null(srt())) {
                    srt(readRDS(paste0(data_path(), "DM_10x/DM_10x_seuratObject_shiny.rds")))
                }

                if (is.null(bulk_tb())) {
                    bulk_tb(readRDS(paste0(data_path(), "DM_10x/DM_10x_srt_pseudobulk_sum_CellSubtype_Sample.rds")))
                }
            }
        }, ignoreInit = TRUE)

        bulk_meta <- reactive({
            req(bulk_tb(), srt())
            df <- do.call(rbind, strsplit(colnames(bulk_tb()), ":"))
            df <- as.data.frame(df, stringsAsFactors = FALSE)
            colnames(df) <- c("CellSubtype", "Sample")
            obj <- srt()
            cellsubtype_levels <- if (is.factor(obj$CellSubtype)) levels(obj$CellSubtype) else unique(obj$CellSubtype)
            sample_levels <- if (is.factor(obj$Sample)) levels(obj$Sample) else unique(obj$Sample)
            df$CellSubtype <- factor(df$CellSubtype, levels = cellsubtype_levels)
            df$Sample <- as.character(df$Sample)
            if (!all(df$Sample %in% sample_levels)) {
                sample_dash_first <- sub("_", "-", df$Sample)
                if (all(sample_dash_first %in% sample_levels)) {
                    df$Sample <- sample_dash_first
                } else {
                    sample_dash_all <- gsub("_", "-", df$Sample)
                    if (all(sample_dash_all %in% sample_levels)) {
                        df$Sample <- sample_dash_all
                    }
                }
            }
            df$Sample <- factor(df$Sample, levels = unique(c(sample_levels, df$Sample)))
            rownames(df) <- colnames(bulk_tb())
            return(df)
        })

        modServer_SeuratEmbeddingPlot(
            id = "dimplot",
            srt = srt,
            groupby_default = "CellSubtype",
            dataname = dataname,
            raster = FALSE,
            groupby_colors_list = list(CellSubtype = colors.ic)
        )

        modServer_SeuratFeaturePlot(
            id = "featureplot",
            srt = srt,
            dataname = dataname,
            raster = FALSE,
            feature_default = "IFNB1"
        )

        modServer_SeuratVlnPlot(
            id = "vlnplot_cellsubtype",
            srt = srt,
            dataname = dataname,
            groupby_column = "CellSubtype",
            subsetby_columns = c("Sample"),
            feature_default = "IFNB1",
            groupby_colors = colors.ic,
            groupby_colors_by = "CellSubtype"
        )

        modServer_PseudoBulkHeatmap(
            id = "pseudoBulk_heatmap",
            bulk_tb = bulk_tb,
            bulk_meta = bulk_meta,
            dataname = dataname,
            groupby_column = "CellSubtype",
            splitby_column = NULL,
            subsetby_columns = c("CellSubtype")
        )
        invisible(list(srt = srt))
    })
}
