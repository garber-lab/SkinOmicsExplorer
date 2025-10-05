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
                bslib::card_header("Violin plot for normalized gene expression in skin conditions"),
                bslib::card_body(modUI_SeuratVlnPlot(
                    ns("vlnplot_skin_disease"),
                    width_default = 6, 
                    height_default = 4.5, 
                    format_default = "png"
                ))
            ),
            col_widths = c(6,6)
        ),
        card(
            bslib::card_header("Violin plot for normalized gene expression"),
            bslib::card_body(modUI_SeuratVlnPlot(ns("vlnplot")))
        ),
        card(
            bslib::card_header("Violin plot for normalized gene expression in all cell types"),
            bslib::card_body(
                layout_sidebar(
                    sidebar = sidebar(
                        modUI_SeuratSubset(ns("subset_vlnplot_celltype"))
                    ),
                    modUI_SeuratVlnPlot(
                        ns("vlnplot_celltype"),
                        width_default = 12, 
                        height_default = 4.5, 
                        format_default = "png"
                    )
                )
            )
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

        modServer_SeuratEmbeddingPlot(
            id = "dimplot",
            srt = srt,
            dataname = dataname
        )
        srt_subset_vlnplot_celltype <- modServer_SeuratSubset(
            id = "subset_vlnplot_celltype",
            srt = srt,
            subsetby_columns = c("Disease", "Skin")
        )
        srt_for_vlnplot_celltype <- reactive({
            req(srt())
            subset_obj <- srt_subset_vlnplot_celltype()
            if (is.null(subset_obj) || ncol(subset_obj) == 0) {
                return(srt())
            }
            subset_obj
        })
        modServer_SeuratVlnPlot(
            id = "vlnplot",
            srt = srt,
            dataname = dataname
        )
        modServer_SeuratVlnPlot(
            id = "vlnplot_celltype",
            srt = srt_for_vlnplot_celltype,
            dataname = dataname,
            groupby_column = "subCellType.pub5.2",
            splitby_column = "CellType.abbr"
        )
        modServer_SeuratVlnPlot(
            id = "vlnplot_skin_disease",
            srt = srt,
            dataname = dataname,
            groupby_column = "Skin",
            splitby_column = "Disease"
        )

        invisible(list(srt = srt))
    })
}
