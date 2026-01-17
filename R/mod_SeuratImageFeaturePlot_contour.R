modUI_SeuratImageFeaturePlot_contour <- function(id, size_perInch_default = 500, format_default = "png", allow_download = TRUE, sigma_default = 400) {
    ns <- NS(id)
    sigma_label <- tags$span(
        class = "hover-hint",
        tabindex = "0",
        "Sigma:",
        `data-hint` = "Kernel bandwidth for density estimation; larger values yield smoother contours."
    )
    threshold_label <- tags$span(
        class = "hover-hint",
        tabindex = "0",
        "Threshold:",
        `data-hint` = "Quantile cutoff for contour values; only the top fraction is plotted."
    )
    download_panel <- if (isTRUE(allow_download)) {
        wellPanel(
            inlineInput("µm per inch:", numericInput(ns("size_perInch"), NULL, value = size_perInch_default, min = 1, step = 1, width = 70), label_width = "100px"),
            inlineInput("Plot width:", numericInput(ns("width"), NULL, value = 6, min = 0.1, step = 0.1, width = 70), label_width = "100px"),
            inlineInput("Plot height:", numericInput(ns("height"), NULL, value = 6, min = 0.1, step = 0.1, width = 70), label_width = "100px"),
            inlineInput("File format:", selectInput(ns("format"), NULL, choices = c("png", "pdf", "jpeg", "tiff"), selected = format_default, width = 70), label_width = "100px"),
            downloadButton(ns("plot_image_download"), "Download")
        )
    } else {
        NULL
    }

    bslib::layout_sidebar(
        sidebar = bslib::sidebar(
            wellPanel(
                selectInput(ns("fov"),
                    "Plot sample:",
                    choices = NULL
                ),
                selectInput(ns("feature"),
                    "Gene:",
                    choices = NULL
                ),
                inlineInput(
                    sigma_label,
                    numericInput(ns("sigma"), NULL, value = sigma_default, min = 1, step = 10, width = 80),
                    label_width = "90px"
                ),
                inlineInput(
                    threshold_label,
                    numericInput(ns("threshold"), NULL, value = 0.9, min = 0, max = 1, step = 0.01, width = 80),
                    label_width = "90px"
                ),
                bslib::input_switch(ns("scalebar"), "Show scalebar", value = T),
                selectInput(ns("scalebar_position"),
                    "Scalebar position:",
                    choices = c("bottomright", "bottomleft", "topright", "topleft"),
                    selected = NULL
                ),
                bslib::input_switch(ns("groupby"), "CellSubtype color", value = TRUE)
            ),
            download_panel,
            open = "always"
        ),
        plotOutput(ns("plot_image"))
    )
}


modServer_SeuratImageFeaturePlot_contour <- function(id, srt, dataname, colors.celltype, fov_choices = NULL,
                                                     feature_default = NULL,
                                                     scalebar_length = NULL, scalebar_numConv = 1, scalebar_unit = NULL,
                                                     scalebar_position_default = NULL, fov.size = NULL) {
    moduleServer(id, function(input, output, session) {
        observeEvent(srt(),
            {
                req(srt())
                fovs <- if (!is.null(fov_choices)) fov_choices else names(srt()@images)
                updateSelectInput(session, "fov", choices = fovs)
                feature_choices <- rownames(srt())
                feature_selected <- if (!is.null(feature_default) && feature_default %in% feature_choices) {
                    feature_default
                } else {
                    NULL
                }
                updateSelectInput(session, "feature", choices = feature_choices, selected = feature_selected)
            },
            ignoreInit = FALSE
        )

        observeEvent(c(input$fov, input$size_perInch),
            {
                req(input$fov, input$size_perInch)
                if (!is.null(fov.size) && input$fov %in% names(fov.size)) {
                    fov_dims <- fov.size[[input$fov]]
                    new_width <- round(fov_dims[["width"]] / input$size_perInch, 2) + 1
                    new_height <- round(fov_dims[["height"]] / input$size_perInch, 2)
                    updateNumericInput(session, "width", value = new_width)
                    updateNumericInput(session, "height", value = new_height)
                }
            },
            ignoreInit = FALSE
        )

        observeEvent(input$fov,
            {
                req(input$fov)
                if (!is.null(scalebar_position_default) && input$fov %in% names(scalebar_position_default)) {
                    updateSelectInput(session, "scalebar_position", selected = scalebar_position_default[[input$fov]])
                }
            },
            ignoreInit = FALSE
        )

        groupby_param <- reactive({
            req(srt())
            if (isTRUE(input$groupby) && "CellSubtype" %in% colnames(srt()@meta.data)) {
                return("CellSubtype")
            }
            NULL
        })

        plot_image <- reactive({
            req(srt(), input$fov, input$feature)
            ImageFeaturePlot.contour(
                object = srt(),
                feature = input$feature,
                fov = input$fov,
                sigma = input$sigma,
                threshold = input$threshold,
                group.by = groupby_param(),
                cols = colors.celltype,
                scalebar.length = if (isTRUE(input$scalebar)) scalebar_length else NULL,
                scalebar.numConv = scalebar_numConv,
                scalebar.unit = scalebar_unit,
                scalebar.position = input$scalebar_position
            )
        })

        output$plot_image <- renderPlot(plot_image(), res = 96)

        output$plot_image_download <- downloadHandler(
            filename = function() {
                paste0(
                    "ImageFeaturePlot_contour_", dataname, "_fov", input$fov,
                    "_gene", input$feature,
                    ".", input$format
                )
            },
            content = function(file) {
                ggplot2::ggsave(
                    filename = file,
                    plot = plot_image(),
                    width = input$width,
                    height = input$height,
                    dpi = 300
                )
            }
        )
    })
}
