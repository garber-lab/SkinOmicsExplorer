modUI_SeuratFeaturePlot <- function(id, width_default = 6, height_default = 6, format_default = "png", allow_download = TRUE) {
    ns <- NS(id)
    download_panel <- if (isTRUE(allow_download)) {
        wellPanel(
            inlineInput("Plot width:", numericInput(ns("width"), NULL, value = width_default, min = 0.1, step = 0.1, width = 70), label_width = "100px"),
            inlineInput("Plot height:", numericInput(ns("height"), NULL, value = height_default, min = 0.1, step = 0.1, width = 70), label_width = "100px"),
            inlineInput("File format:", selectInput(ns("format"), NULL, choices = c("png", "pdf", "jpeg", "tiff"), selected = format_default, width = 70), label_width = "100px"),
            downloadButton(ns("plot_feature_download"), "Download")
        )
    } else {
        NULL
    }
    bslib::layout_sidebar(
        sidebar = bslib::sidebar(
            wellPanel(
                selectInput(
                    ns("reduction"),
                    "Plot using embedding:",
                    choices = NULL
                ),
                selectizeInput(
                    ns("gene"),
                    "Gene:",
                    choices = NULL
                ),
                actionButton(ns("plot"), "Plot")
            ),
            download_panel,
            open = "always"
        ),
        plotOutput(ns("plot_feature"))
    )
}


modServer_SeuratFeaturePlot <- function(id, srt, reduction_choices = NULL, dataname, raster = NULL, feature_default = NULL) {
    moduleServer(id, function(input, output, session) {
        observeEvent(srt(),
            {
                req(srt())
                reductions <- if (!is.null(reduction_choices)) reduction_choices else Seurat::Reductions(srt())
                updateSelectInput(session, "reduction", choices = reductions)
                gene_choices <- rownames(srt())
                gene_selected <- if (!is.null(feature_default) && feature_default %in% gene_choices) {
                    feature_default
                } else {
                    NULL
                }
                updateSelectizeInput(session, "gene", choices = gene_choices, selected = gene_selected, server = TRUE)
            },
            ignoreInit = FALSE
        )

        plot_feature <- reactive({
            req(srt(), input$reduction, input$gene)
            Seurat::FeaturePlot(
                srt(),
                features = input$gene,
                reduction = input$reduction,
                raster = raster
            ) + coord_fixed(ratio = 1)
        })

        output$plot_feature <- renderPlot(plot_feature(), res = 96) |> bindEvent(input$plot)

        output$plot_feature_download <- downloadHandler(
            filename = function() {
                paste0("FeaturePlot_", dataname, "_gene", input$gene, "_", input$reduction, ".", input$format)
            },
            content = function(file) {
                ggplot2::ggsave(filename = file, plot = plot_feature(), width = input$width, height = input$height, dpi = 300)
            }
        )
    })
}
