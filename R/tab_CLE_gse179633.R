tabUI_CLE_gse179633 <- function(id){
    ns <- NS(id)
    tagList(
        layout_columns(
            bslib::card(
                class = "mb-3",
                bslib::card_body(
                    tags$div(
                        tags$h5("Abbreviations"),
                        tags$p(tags$strong("Disease types:")),
                        tags$ul(
                            tags$li("HC - Healthy Control"),
                            tags$li("DLE - Discoid Lupus Erythematosus"),
                            tags$li("SLE - Systemic lupus erythematosus")
                        ),
                        tags$p(tags$strong("Tissue type:")),
                        tags$ul(
                            tags$li("E - Epidermis"),
                            tags$li("D - Dermis")
                        )
                    )
                )
            ),
            bslib::card(
                bslib::card_header("Seurat Embedding Plot"),
                bslib::card_body(modUI_SeuratEmbeddingPlot(
                    ns("dimplot"),
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
                    modUI_PseudoBulkHeatmap(
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

tabServer_CLE_gse179633 <- function(id, data_path){
    moduleServer(id, function(input, output, session){
        dataname <- "CLE_gse179633"

        srt <- reactive({
            progress <- Progress$new(session, min=0, max=1)
            on.exit(progress$close())
            progress$set(message = "Reading Seurat Object", detail = 'about 20 seconds')

            obj <- readRDS(paste0(data_path(), 'CLE_gse179633/CLE_gse179633_seuratObject_shiny.rds'))
            return(obj)
        })

        bulk_tb <- reactive({
            tb <- readRDS(paste0(data_path(), 'CLE_gse179633/shiny_hostingCLE_gse179633_pseudobulk_sum_CellType_CellSubtype_disease_tissueType.rds'))
        })

        bulk_meta <- reactive({
            df <- as.data.frame(do.call(rbind, strsplit(colnames(bulk_tb()), ":")))
            colnames(df) <- c("CellType","CellSubtype","Disease","tissueType")
            obj <- srt()
            df[, "CellType"] <- factor(df[,"CellType"], levels = levels(obj$CellType))
            df[, "CellSubtype"] <- factor(df[,"CellSubtype"], levels = levels(obj$CellSubtype))
            df[, "Disease"] <- factor(df[,"Disease"], levels = levels(obj$Disease))
            df[, "tissueType"] <- factor(df[,"tissueType"], levels = levels(obj$tissueType))
            rownames(df) <- colnames(bulk_tb())
            return(df)
        })

        modServer_SeuratEmbeddingPlot(
            id = "dimplot",
            srt = srt,
            dataname = dataname
        )

        modServer_SeuratVlnPlot(
            id = "vlnplot_celltype",
            srt = srt,
            dataname = dataname,
            groupby_column = "CellSubtype",
            splitby_column = "CellType",
            subsetby_columns = c("Disease", "tissueType")
        )

        modServer_SeuratVlnPlot(
            id = "vlnplot_tissueType_disease",
            srt = srt,
            dataname = dataname,
            groupby_column = "tissueType",
            splitby_column = "Disease",
            subsetby_columns = c("CellType")
        )
        
        modServer_PseudoBulkHeatmap(
            id = "pseudoBulk_heatmap",
            bulk_tb = bulk_tb,
            bulk_meta = bulk_meta,
            dataname = dataname,
            groupby_column = NULL,
            splitby_column = NULL,
            subsetby_columns = c("Disease","tissueType","CellType","CellSubtype")
        )
        invisible(list(srt = srt))
    })
}
