modUI_SeuratImageDimPlot <- function(id, size_perInch_default = 500, format_default = "png", allow_download = TRUE) {
    ns <- NS(id)
    download_panel <- if (isTRUE(allow_download)) {
        wellPanel(
            numericInput(ns("size_perInch"), "µm per inch", value = size_perInch_default, min = 1),
            inlineInput("Plot width:", numericInput(ns("width"), NULL, value = 6, min = 0.1, step = 0.1, width = 70), label_width = "90px"),
            inlineInput("Plot height:", numericInput(ns("height"), NULL, value = 6, min = 0.1, step = 0.1, width = 70), label_width = "90px"),
            inlineInput("File format:", selectInput(ns("format"), NULL, choices = c("png", "pdf", "jpeg", "tiff"), selected = format_default, width = 70), label_width = "90px"),
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
                uiOutput(ns("groupby_ui")),
                bslib::input_switch(ns("scalebar"), "Show scalebar", value = T),
                selectInput(ns("scalebar_position"),
                    "Scalebar position:",
                    choices = c("bottomright", "bottomleft", "topright", "topleft"),
                    selected = NULL
                ),
                bslib::input_switch(ns("legend"), "Show legend", value = F),
                bslib::input_switch(ns("legend_concise"), "Concise legend", value = TRUE)
            ),
            wellPanel(
                tags$strong("Cell point options"),
                inlineInput("Transparency:", numericInput(ns("alpha"), NULL, value = 1, min = 0, max = 1, step = 0.1, width = 60), label_width = "120px"),
                inlineInput("Size:", numericInput(ns("size"), NULL, value = 0.1, min = 0.1, step = 0.1, width = 60), label_width = "120px")
            ),
            wellPanel(
                tags$strong("Highlight cell options"),
                selectInput(
                    ns("highlight_groups"),
                    "Highlight subtypes:",
                    choices = NULL,
                    multiple = TRUE
                ),
                inlineInput("Size:", numericInput(ns("highlight_size"), NULL, value = 1, min = 0.1, step = 0.1, width = 70), label_width = "90px")
            ),
            download_panel,
            open = "always"
        ),
        plotOutput(ns("plot_image"))
    )
}


modServer_SeuratImageDimPlot <- function(id, srt, dataname, fov_choices = NULL, groupby_column = NULL, groupby_colors = NULL,
                                         scalebar_length = NULL, scalebar_numConv = 1, scalebar_unit = NULL,
                                         scalebar_position_default = NULL, fov.size = NULL) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
        groupby_options <- reactiveVal(character())

        # Update FOV choices when srt changes
        observeEvent(srt(),
            {
                req(srt())
                fovs <- if (!is.null(fov_choices)) fov_choices else names(srt()@images)
                updateSelectInput(session, "fov", choices = fovs)

                groupbys <- if (!is.null(groupby_column)) {
                    groupby_column
                } else {
                    md <- srt()@meta.data
                    cols <- colnames(md)[sapply(md, function(x) is.character(x) || is.factor(x))]
                    unique(cols)
                }
                groupby_options(groupbys)
            },
            ignoreInit = FALSE
        )

        output$groupby_ui <- renderUI({
            choices <- groupby_options()
            req(length(choices) > 1) # returns NULL (hides UI) if condition fails
            selectInput(ns("groupby"), "Color the cells by:", choices = choices)
        })

        groupby_param <- reactive({
            choices <- groupby_options()
            if (length(choices) == 0) {
                return(NULL)
            }
            if (length(choices) == 1) {
                return(choices[1])
            }
            req(input$groupby)
            input$groupby
        })

        # Update width/height based on selected FOV size, legend, and size_perInch
        observeEvent(c(input$fov, input$legend, input$size_perInch),
            {
                req(input$fov, input$size_perInch)
                if (!is.null(fov.size) && input$fov %in% names(fov.size)) {
                    fov_dims <- fov.size[[input$fov]]
                    new_width <- round(fov_dims[["width"]] / input$size_perInch, 2)
                    new_height <- round(fov_dims[["height"]] / input$size_perInch, 2)
                    # Add 1 inch for legend if shown
                    if (isTRUE(input$legend) && isTRUE(input$concise.legend)) {
                        new_width <- new_width + 1
                    } else if (isTRUE(input$legend) && isFALSE(input$concise.legend)) {
                        new_width <- new_width + 3
                    }
                    updateNumericInput(session, "width", value = new_width)
                    updateNumericInput(session, "height", value = new_height)
                }
            },
            ignoreInit = FALSE
        )

        # Update scalebar position based on FOV default
        observeEvent(input$fov,
            {
                req(input$fov)
                if (!is.null(scalebar_position_default) && input$fov %in% names(scalebar_position_default)) {
                    updateSelectInput(session, "scalebar_position", selected = scalebar_position_default[[input$fov]])
                }
            },
            ignoreInit = FALSE
        )

        # Update highlight_groups choices when groupby changes
        observeEvent(groupby_param(),
            {
                req(srt(), groupby_param())
                md <- srt()@meta.data
                groupby_val <- groupby_param()
                if (groupby_val %in% colnames(md)) {
                    groups <- unique(as.character(md[[groupby_val]]))
                    groups <- groups[!is.na(groups)]
                    updateSelectInput(session, "highlight_groups", choices = groups, selected = NULL)
                }
            },
            ignoreInit = FALSE
        )

        # Generate the plot
        plot_image <- reactive({
            req(srt(), input$fov, groupby_param())

            # Get highlight groups (may be NULL or empty)
            highlight_groups <- if (length(input$highlight_groups) > 0) input$highlight_groups else NULL

            # Build the plot
            g <- ImageDimPlot.ssc(
                object = srt(),
                fov = input$fov,
                group.by = groupby_param(),
                cols = groupby_colors,
                alpha = input$alpha,
                size = input$size,
                highlight.groups = highlight_groups,
                highlight.size = input$highlight_size,
                legend.concise = input$legend_concise,
                scalebar.length = if (isTRUE(input$scalebar)) scalebar_length else NULL,
                scalebar.numConv = scalebar_numConv,
                scalebar.unit = scalebar_unit,
                scalebar.position = input$scalebar_position
            )

            # Handle legend visibility
            if (!isTRUE(input$legend)) {
                g <- g + ggplot2::theme(legend.position = "none")
            }

            return(g)
        })

        output$plot_image <- renderPlot(plot_image(), res = 96)

        output$plot_image_download <- downloadHandler(
            filename = function() {
                paste0(
                    "ImageDimPlot_", dataname, "_fov", input$fov,
                    "_groupby", groupby_param(),
                    "_highlight", paste(input$highlight_groups, collapse = "-"),
                    "_scalebar", input$scalebar,
                    "_legend", input$legend,
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
