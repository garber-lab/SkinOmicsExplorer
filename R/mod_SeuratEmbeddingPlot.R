modUI_SeuratEmbeddingPlot <- function(id, width_default = 6, height_default = 6, format_default = "png", allow_download = TRUE) {
    ns <- NS(id)
    download_panel <- if (isTRUE(allow_download)) {
        wellPanel(
            inlineInput("Plot width:", numericInput(ns("width"), NULL, value = width_default, min = 0.1, step = 0.1, width = 70), label_width = "100px"),
            inlineInput("Plot height:", numericInput(ns("height"), NULL, value = height_default, min = 0.1, step = 0.1, width = 70), label_width = "100px"),
            inlineInput("File format:", selectInput(ns("format"), NULL, choices = c("png", "pdf", "jpeg", "tiff"), selected = format_default, width = 70), label_width = "100px"),
            downloadButton(ns("plot_embedding_download"), "Download")
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
                selectInput(
                    ns("groupby"),
                    "Color the cells by:",
                    choices = NULL
                ),
                bslib::input_switch(ns("label"), "Label in plot", value = T),
                bslib::input_switch(ns("legend"), "Show legend", value = F),
                actionButton(ns("plot"), "Plot")
            ),
            download_panel,
            open = "always"
        ),
        plotOutput(ns("plot_embedding"))
    )
}


modServer_SeuratEmbeddingPlot <- function(id, srt, groupby_column = NULL, reduction_choices = NULL, groupby_default = NULL, dataname, groupby_colors = NULL, groupby_colors_by = NULL, raster = NULL) {
    moduleServer(id, function(input, output, session) {
        observeEvent(srt(),
            {
                req(srt())
                reductions <- if (!is.null(reduction_choices)) reduction_choices else Seurat::Reductions(srt())
                updateSelectInput(session, "reduction", choices = reductions)
                groupbys <- if (!is.null(groupby_column)) {
                    groupby_column
                } else {
                    md <- srt()@meta.data
                    cols <- colnames(md)[sapply(md, function(x) is.character(x) || is.factor(x))]
                    unique(cols)
                }
                groupby_selected <- if (!is.null(groupby_default) && groupby_default %in% groupbys) {
                    groupby_default
                } else {
                    NULL
                }
                updateSelectInput(session, "groupby", choices = groupbys, selected = groupby_selected)
            },
            ignoreInit = F
        )

        plot_embedding <- reactive({
            req(srt(), input$reduction, input$groupby)
            g <- Seurat::DimPlot(
                srt(),
                reduction = input$reduction,
                group.by = input$groupby,
                shuffle = T, alpha = 0.8,
                label = isTRUE(input$label),
                raster = raster
            ) + coord_fixed(ratio = 1)
            use_colors <- NULL
            if (!is.null(groupby_colors)) {
                if (is.null(groupby_colors_by) || identical(input$groupby, groupby_colors_by)) {
                    meta_vals <- srt()@meta.data[[input$groupby]]
                    use_colors <- manual_scale_values(groupby_colors, meta_vals)
                }
            }
            if (!is.null(use_colors)) {
                g <- g + scale_color_manual(values = use_colors)
            }
            if (!isTRUE(input$legend)) {
                g <- g + NoLegend()
            }
            return(g)
        })

        output$plot_embedding <- renderPlot(plot_embedding(), res = 96) |> bindEvent(input$plot)

        output$plot_embedding_download <- downloadHandler(
            filename = function() {
                paste0("DimPlot_", dataname, "_groupby", input$groupby, "_", input$reduction, "_label", input$label, "_legend", input$legend, ".", input$format)
            },
            content = function(file) {
                ggplot2::ggsave(filename = file, plot = plot_embedding(), width = input$width, height = input$height, dpi = 300)
            }
        )
    })
}
