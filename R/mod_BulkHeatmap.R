modUI_BulkHeatmap <- function(id, allow_subset = FALSE, width_default = 10, height_default = 3, format_default = "pdf", allow_download = TRUE) {
    ns <- NS(id)

    download_panel <- if (isTRUE(allow_download)) {
        wellPanel(
            inlineInput("Plot width:", numericInput(ns("width"), NULL, value = width_default, min = 0.1, step = 0.1, width = 70), label_width = "100px"),
            inlineInput("Plot height:", numericInput(ns("height"), NULL, value = height_default, min = 0.1, step = 0.1, width = 70), label_width = "100px"),
            inlineInput("File format:", selectInput(ns("format"), NULL, choices = c("png", "pdf", "jpeg", "tiff"), selected = format_default, width = 70), label_width = "100px"),
            downloadButton(ns("plot_heatmap_download"), "Download")
        )
    } else {
        NULL
    }

    plot_panel <- wellPanel(
        selectizeInput(ns("gene"), "Gene name:", choices = NULL, multiple = TRUE),
        uiOutput(ns("groupby_ui")),
        uiOutput(ns("splitby_ui")),
        uiOutput(ns("condition_display_sets_ui")),
        bslib::input_switch(ns("cluster_genes"), "Cluster genes", value = FALSE),
        uiOutput(ns("plot_button_ui"))
    )

    sidebar_content <- if (!allow_subset) {
        tagList(plot_panel, download_panel)
    } else {
        accordion(
            accordion_panel(
                title = "Subset data (optional)",
                value = "subset",
                modUI_BulkSubset(ns("subset"))
            ),
            accordion_panel(
                title = "Plot options",
                value = "plot",
                tagList(plot_panel, download_panel)
            ),
            multiple = FALSE,
            open = "plot"
        )
    }

    layout_sidebar(
        sidebar = sidebar(
            sidebar_content,
            open = "always"
        ),
        plotOutput(ns("plot_heatmap"))
    )
}

modServer_BulkHeatmap <- function(id, bulk_tb, bulk_meta, condition_display_sets = NULL, dataname, groupby_column = NULL, splitby_column = NULL, subsetby_columns = NULL, show_plot_button = TRUE) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        resolve_input <- function(x) {
            if (is.function(x)) x() else x
        }

        output$plot_button_ui <- renderUI({
            if (isTRUE(show_plot_button)) actionButton(ns("plot"), "Plot")
        })

        has_subset <- !is.null(subsetby_columns) && length(subsetby_columns) > 0
        if (has_subset) {
            subset_ind <- modServer_BulkSubset(
                id = "subset",
                bulk_tb = bulk_tb,
                bulk_meta = bulk_meta,
                subsetby_columns = subsetby_columns
            )
        }

        tb_for_plot <- reactive({
            tb <- resolve_input(bulk_tb)
            req(tb)
            if (!has_subset) return(tb)
            subset_tb <- tb[, subset_ind()]
            if (is.null(subset_tb)) return(tb)
            if (ncol(subset_tb) == 0) return(tb)
            subset_tb
        })

        md_for_plot <- reactive({
            md <- resolve_input(bulk_meta)
            req(md)
            if (!has_subset) return(md)
            subset_idx <- colnames(tb_for_plot())
            subset_md <- droplevels(md[rownames(md) %in% subset_idx, ])
            subset_md
        })

        groupby_options <- reactiveVal(character())
        splitby_options <- reactiveVal(character())
        observe({
            tb <- resolve_input(bulk_tb)
            md <- resolve_input(bulk_meta)
            req(tb, md)
            groupbys <- if (is.null(groupby_column)) {
                cols <- colnames(md)[sapply(md, function(x) is.character(x) || is.factor(x))]
                unique(cols)
            } else {
                groupby_column
            }

            splitbys <- if (is.null(splitby_column)) {
                cols <- colnames(md)[sapply(md, function(x) is.character(x) || is.factor(x))]
                unique(cols)
            } else {
                splitby_column
            }
            updateSelectizeInput(session, "gene", choices = rownames(tb), server = TRUE)
            groupby_options(groupbys)
            splitby_options(splitbys)
        })

        output$groupby_ui <- renderUI({
            choices <- groupby_options()
            req(length(choices) > 1)
            selectInput(ns("groupby"), "Group by:", choices = choices)
        })

        output$splitby_ui <- renderUI({
            choices <- splitby_options()
            req(length(choices) > 1)
            selectInput(ns("splitby"), "Split by:", choices = c("Unselect" = "null", choices))
        })

        groupby_param <- reactive({
            choices <- groupby_options()
            if (length(choices) == 0) return(NULL)
            if (length(choices) == 1) return(choices[1])
            req(input$groupby)
            input$groupby
        })

        splitby_param <- reactive({
            choices <- splitby_options()
            if (length(choices) == 0) return(NULL)
            if (length(choices) == 1) return(choices[1])
            req(input$splitby)
            if (input$splitby == "null") return(NULL)
            input$splitby
        })

        if (!is.null(condition_display_sets)) {
            output$condition_display_sets_ui <- renderUI({
                selectInput(
                    ns("condition_display_set"),
                    "Display conditions:",
                    choices = names(condition_display_sets),
                    selected = names(condition_display_sets)[1]
                )
            })
        } else {
            output$condition_display_sets_ui <- renderUI({ NULL })
        }

        conditions_for_display <- reactive({
            if (is.null(condition_display_sets)) return(NULL)
            req(input$condition_display_set)
            condition_display_sets[[input$condition_display_set]]
        })

        plot_heatmap <- reactive({
            plot_tb <- tb_for_plot()
            plot_md <- md_for_plot()
            req(plot_tb, plot_md, groupby_param(), input$gene)

            common_cols <- intersect(colnames(plot_tb), rownames(plot_md))
            if (length(common_cols) == 0) return(NULL)
            plot_tb <- plot_tb[, common_cols, drop = FALSE]
            plot_md <- plot_md[common_cols, , drop = FALSE]

            if (!is.null(conditions_for_display())) {
                display_conditions <- conditions_for_display()
                condition_col <- display_conditions[[1]]
                condition_values <- display_conditions[[2]]
                keep_idx <- plot_md[[condition_col]] %in% condition_values
                plot_tb <- plot_tb[, keep_idx, drop = FALSE]
                plot_md <- plot_md[keep_idx, , drop = FALSE]
            }

            HeatmapBulk(
                plot_tb,
                plot_md,
                genes = input$gene,
                groupby = groupby_param(),
                splitby = splitby_param(),
                cluster_genes = isTRUE(input$cluster_genes)
            )
        })

        plot_heatmap_event <- plot_heatmap
        if (isTRUE(show_plot_button)) {
            plot_heatmap_event <- bindEvent(plot_heatmap_event, input$plot, tb_for_plot(), md_for_plot())
        }

        output$plot_heatmap <- renderPlot({
            hmap <- plot_heatmap_event()
            req(hmap)
            ComplexHeatmap::draw(hmap)
            invisible(NULL)
        }, res = 96)

        output$plot_heatmap_download <- downloadHandler(
            filename = function() {
                groupby_str <- groupby_param()
                splitby_str <- splitby_param()

                paste0(
                    "Heatmap_",
                    dataname,
                    "_groupby",
                    groupby_str,
                    ifelse(is.null(splitby_str), "", paste0("_splitby", splitby_str)),
                    ".",
                    input$format
                )
            },
            content = function(file) {
                hmap <- plot_heatmap()
                req(hmap)
                width <- input$width
                height <- input$height
                format <- tolower(input$format)
                dpi <- 300

                if (format == "pdf") {
                    grDevices::pdf(file, width = width, height = height)
                    on.exit(grDevices::dev.off(), add = TRUE)
                    ComplexHeatmap::draw(hmap)
                } else if (format == "png") {
                    grDevices::png(file, width = width, height = height, units = "in", res = dpi)
                    on.exit(grDevices::dev.off(), add = TRUE)
                    ComplexHeatmap::draw(hmap)
                } else if (format == "jpeg") {
                    grDevices::jpeg(file, width = width, height = height, units = "in", res = dpi)
                    on.exit(grDevices::dev.off(), add = TRUE)
                    ComplexHeatmap::draw(hmap)
                } else if (format == "tiff") {
                    grDevices::tiff(file, width = width, height = height, units = "in", res = dpi)
                    on.exit(grDevices::dev.off(), add = TRUE)
                    ComplexHeatmap::draw(hmap)
                } else {
                    grDevices::pdf(file, width = width, height = height)
                    on.exit(grDevices::dev.off(), add = TRUE)
                    ComplexHeatmap::draw(hmap)
                }
            }
        )
    })
}
