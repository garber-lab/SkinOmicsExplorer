tabUI_fourDisease_indrop <- function(id){
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
            col_widths = c(6,6)
        ),
        card(
            bslib::card_header("Violin plot for normalized gene expression in all cell types"),
            bslib::card_body(
                modUI_SeuratVlnPlot(
                    ns("vlnplot_celltype"),
                    width_default = 12,
                    height_default = 4.5,
                    format_default = "png",
                    allow_subset = TRUE
                )
            )
        ),
        layout_columns(
            bslib::card(
                bslib::card_header("Violin plot for normalized gene expression in skin conditions"),
                bslib::card_body(
                    modUI_SeuratVlnPlot(
                        ns("vlnplot_skin_disease"),
                        width_default = 6,
                        height_default = 4.5,
                        format_default = "png",
                        allow_subset = TRUE
                        )
                    )
            ),
            bslib::card(
                bslib::card_header("Heatmap for pseudo-bulked gene expression in skin conditions"),
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
            col_widths = c(6,6)
        )
    )
}

tabServer_fourDisease_indrop <- function(id, data_path){
    moduleServer(id, function(input, output, session){
        dataname <- "fourDisease_indrop"

        srt <- reactive({
            progress <- Progress$new(session, min=0, max=1)
            on.exit(progress$close())
            progress$set(message = 'Reading Seurat Object', detail = 'about 20 seconds')

            obj <- readRDS(paste0(data_path(), 'fourDisease_indrop/fourDisease_indrop_seuratObject.rds'))
            return(obj)
        })

        bulk_tb <- reactive({
            tb <- readRDS(paste0(data_path(), 'fourDisease_indrop/fourDisease_indrop_pseudobulk_sum_CellType_Disease_Skin.rds'))
            return(tb)
        })

        bulk_meta <- reactive({
            df <- do.call(rbind, strsplit(colnames(bulk_tb()), ":"))
            df <- as.data.frame(df)
            colnames(df) <- c("CellType","Disease","Skin")
            obj <- srt()
            df[, "CellType"] <- factor(df[,"CellType"], levels = levels(obj$CellType))
            df[, "Disease"] <- factor(df[,"Disease"], levels = levels(obj$Disease))
            df[, "Skin"] <- factor(df[,"Skin"], levels = levels(obj$Skin))
            rownames(df) <- colnames(bulk_tb())
            return(df)
        })

        modServer_SeuratEmbeddingPlot(
            id = "dimplot",
            srt = srt,
            dataname = dataname
        )

        modServer_SeuratFeaturePlot(
            id = "featureplot",
            srt = srt,
            dataname = dataname
        )

        modServer_SeuratVlnPlot(
            id = "vlnplot_celltype",
            srt = srt,
            dataname = dataname,
            groupby_column = "CellSubtype",
            splitby_column = "CellType",
            subsetby_columns = c("Disease", "Skin")
        )

        modServer_SeuratVlnPlot(
            id = "vlnplot_skin_disease",
            srt = srt,
            dataname = dataname,
            groupby_column = "Skin",
            splitby_column = "Disease",
            subsetby_columns = c("CellSubtype")
        )

        modServer_PseudoBulkHeatmap(
            id = "pseudoBulk_heatmap",
            bulk_tb = bulk_tb,
            bulk_meta = bulk_meta,
            dataname = dataname,
            groupby_column = "Skin",
            splitby_column = "Disease",
            subsetby_columns = c("CellType")
        )
        invisible(list(srt = srt))
    })
}
