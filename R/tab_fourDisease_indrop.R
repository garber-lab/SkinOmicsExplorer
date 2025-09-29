tabUI_fourDisease_indrop <- function(id){
    ns <- NS(id)
    tagList(
        layout_columns(
            bslib::card(
                bslib::card_header("Seurat Embedding Plot"),
                bslib::card_body(
                    modUI_SeuratEmbeddingPlot(ns("dimplot"))
                )
            ),
            bslib::card(
                bslib::card_header("Seurat Embedding Plot"),
                bslib::card_body(
                    modUI_SeuratEmbeddingPlot(ns("dimplot2"))
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

        modServer_SeuratEmbeddingPlot(
            id = "dimplot",
            srt = srt,
            dataname = dataname
        )
        modServer_SeuratEmbeddingPlot(
            id = "dimplot2",
            srt = srt,
            dataname = dataname
        )

        invisible(list(srt = srt))
    })
}
