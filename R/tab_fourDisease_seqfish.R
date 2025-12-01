tabUI_fourDisease_seqfish <- function(id){
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
                            tags$li("DM - Dermatomyositis"),
                            tags$li("CLE - Cutaneous Lupus Erythematosus"),
                            tags$li("Pso - Psoriasis"),
                            tags$li("Vit - Vitiligo")
                        ),
                        tags$h5("Abbreviations"),
                        tags$ul(
                            tags$li("DM - UV109, UV253"),
                            tags$li("CLE - UV243, UV260"),
                            tags$li("Pso - UV238, UV239"),
                            tags$li("Vit - VB268")
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
        )
    )
}

tabServer_fourDisease_seqfish <- function(id, data_path){
    moduleServer(id, function(input, output, session){
        dataname <- "fourDisease_seqfish"

        srt <- reactive({
            progress <- Progress$new(session, min=0, max=1)
            on.exit(progress$close())
            progress$set(message = "Reading Seurat Object", detail = 'about 20 seconds')

            obj <- readRDS(paste0(data_path(), 'fourDisease_seqfish/fourDisease_seqfish_seuratObject_shiny.rds'))
            return(obj)
        })

        modServer_SeuratEmbeddingPlot(
            id = "dimplot",
            srt,
            dataname = dataname
        )

        # modServer_SeuratImageDimPlot(
        #     id = "image_dimplot",
        #     srt = srt,
        #     dataname = dataname,
        #     groupby_column = "CellSubtype",
        #     highlight = NULL,

        # )
    })
}
