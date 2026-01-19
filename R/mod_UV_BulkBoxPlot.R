modUI_UV_BulkBoxPlot <- function(id,
                                 width_default = 4,
                                 height_default = 3.5,
                                 format_default = "pdf",
                                 allow_download = TRUE,
                                 plot_height = "350px") {
    ns <- NS(id)

    plot_panel <- wellPanel(
        selectizeInput(ns("gene"), "Gene:", choices = NULL, multiple = FALSE),
        uiOutput(ns("groupby_ui")),
        uiOutput(ns("splitby_ui")),
        checkboxInput(ns("log2"), "log2(CPM + 1)", value = FALSE),
        actionButton(ns("plot"), "Plot")
    )

    download_panel <- if (isTRUE(allow_download)) {
        wellPanel(
            inlineInput("Plot width:", numericInput(ns("width"), NULL, value = width_default, min = 0.1, step = 0.1, width = 70), label_width = "100px"),
            inlineInput("Plot height:", numericInput(ns("height"), NULL, value = height_default, min = 0.1, step = 0.1, width = 70), label_width = "100px"),
            inlineInput("File format:", selectInput(ns("format"), NULL, choices = c("png", "pdf", "jpeg", "tiff"), selected = format_default, width = 70), label_width = "100px"),
            downloadButton(ns("plot_boxplot_download"), "Download")
        )
    } else {
        NULL
    }

    layout_sidebar(
        sidebar = sidebar(
            tagList(plot_panel, download_panel),
            open = "always"
        ),
        plotOutput(ns("plot_boxplot"), height = plot_height)
    )
}

modServer_UV_BulkBoxPlot <- function(id,
                                     bulk_cpm,
                                     bulk_meta,
                                     dataname = "UV_bulk",
                                     feature_default = NULL,
                                     groupby_column = NULL,
                                     splitby_column = NULL,
                                     groupby_colors = NULL,
                                     shape_by = NULL,
                                     ylab = "CPM") {
    splitby_column_missing <- missing(splitby_column)
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        resolve_input <- function(x) {
            if (is.function(x)) x() else x
        }

        data_info <- reactive({
            cpm <- resolve_input(bulk_cpm)
            meta <- resolve_input(bulk_meta)
            if (is.null(cpm) || is.null(meta)) return(NULL)

            cpm <- as.data.frame(cpm, check.names = FALSE, stringsAsFactors = FALSE)
            meta <- as.data.frame(meta, stringsAsFactors = FALSE)

            if (is.null(rownames(cpm)) || any(rownames(cpm) == "")) {
                candidates <- c("gene", "Gene", "GENE")
                candidates <- candidates[candidates %in% colnames(cpm)]
                if (length(candidates) > 0) {
                    rownames(cpm) <- cpm[[candidates[1]]]
                    cpm[[candidates[1]]] <- NULL
                }
            }

            if (is.null(rownames(cpm)) || any(rownames(cpm) == "")) return(NULL)

            if (is.null(rownames(meta)) || any(rownames(meta) == "")) return(NULL)

            list(
                cpm = cpm,
                meta = meta
            )
        })

        groupby_options <- reactiveVal(character())
        splitby_options <- reactiveVal(character())
        observe({
            info <- data_info()
            req(info)
            selected_gene <- NULL
            if (!is.null(feature_default) && feature_default %in% rownames(info$cpm)) {
                selected_gene <- feature_default
            }
            groupbys <- if (is.null(groupby_column)) {
                md <- info$meta
                cols <- colnames(md)[sapply(md, function(x) is.character(x) || is.factor(x))]
                unique(cols)
            } else {
                groupby_column
            }
            splitbys <- if (splitby_column_missing) {
                md <- info$meta
                cols <- colnames(md)[sapply(md, function(x) is.character(x) || is.factor(x))]
                unique(cols)
            } else if (is.null(splitby_column) || length(splitby_column) == 0) {
                character(0)
            } else {
                splitby_column
            }
            if (is.null(selected_gene)) {
                updateSelectizeInput(session, "gene", choices = rownames(info$cpm), server = TRUE)
            } else {
                updateSelectizeInput(session, "gene", choices = rownames(info$cpm), selected = selected_gene, server = TRUE)
            }
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
            selectInput(ns("splitby"), "Bin groups by:", choices = c("Unselect" = "null", choices))
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

        build_plot <- function() {
            info <- data_info()
            if (is.null(info)) return(NULL)
            if (is.null(input$gene) || length(input$gene) == 0) return(NULL)
            gene <- input$gene[1]
            if (!gene %in% rownames(info$cpm)) return(NULL)

            meta <- info$meta
            meta <- meta[rownames(meta) %in% colnames(info$cpm), , drop = FALSE]
            if (nrow(meta) == 0) return(NULL)

            groupby <- groupby_param()
            if (is.null(groupby) || !(groupby %in% colnames(meta))) return(NULL)

            splitby <- splitby_param()
            if (!is.null(splitby) && !(splitby %in% colnames(meta))) {
                splitby <- NULL
            }

            shape_by_use <- shape_by
            if (!is.null(shape_by_use) && !(shape_by_use %in% colnames(meta))) {
                shape_by_use <- NULL
            }

            colors <- groupby_colors
            if (is.function(groupby_colors)) {
                colors <- groupby_colors(meta[[groupby]])
            } else if (is.list(groupby_colors) && groupby %in% names(groupby_colors)) {
                colors <- groupby_colors[[groupby]]
            }

            bulk_boxplot_plot(
                cpm = info$cpm,
                meta = meta,
                gene = gene,
                group.by = groupby,
                split.by = splitby,
                shape.by = shape_by_use,
                group.color = colors,
                log2_scale = isTRUE(input$log2),
                ylab = ylab
            )
        }

        current_plot <- reactiveVal(ggplot2::ggplot() + ggplot2::theme_void())

        observeEvent(input$plot, {
            p <- build_plot()
            if (!is.null(p)) current_plot(p)
        }, ignoreNULL = FALSE)

        output$plot_boxplot <- renderPlot(current_plot(), res = 96)

        output$plot_boxplot_download <- downloadHandler(
            filename = function() {
                groupby_str <- groupby_param()
                gene_str <- if (is.null(input$gene) || length(input$gene) == 0) "gene" else input$gene[1]
                splitby_str <- splitby_param()
                paste0(
                    "Bulk_BoxPlot_",
                    dataname,
                    "_",
                    gene_str,
                    "_groupby_",
                    groupby_str,
                    ifelse(is.null(splitby_str), "", paste0("_splitby_", splitby_str)),
                    ".",
                    input$format
                )
            },
            content = function(file) {
                ggplot2::ggsave(filename = file, plot = current_plot(), width = input$width, height = input$height, dpi = 300)
            }
        )
    })
}
