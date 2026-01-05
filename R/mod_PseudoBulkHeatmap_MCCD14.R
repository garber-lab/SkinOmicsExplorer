modUI_PseudoBulkHeatmap_bin <- function(id, allow_subset = FALSE, width_default = 8, height_default = 8, format_default = "pdf", allow_download = TRUE) {
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
        bslib::input_switch(ns("cluster_genes"), "Cluster genes", value = FALSE),
        actionButton(ns("plot"), "Plot")
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


modServer_PseudoBulkHeatmap_bin <- function(id, bulk_tb, bulk_meta, dataname, groupby_column = NULL, splitby_column = NULL, subsetby_columns = NULL) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

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
            tb <- bulk_tb()
            req(tb)
            if (!has_subset) return(tb)
            subset_tb <- tb[, subset_ind()]
            if (is.null(subset_tb)) return(tb)
            if (ncol(subset_tb) == 0) return(tb)
            subset_tb
        })
        md_for_plot <- reactive({
            md <- bulk_meta()
            req(md)
            if (!has_subset) return(md)
            subset_idx <- colnames(tb_for_plot())
            subset_md <- droplevels(md[rownames(md) %in% subset_idx, ])
            subset_md
        })

        collapse_other_meta <- function(md, groupby, splitby) {
            used_cols <- c(groupby, splitby)
            used_cols <- used_cols[!is.null(used_cols)]
            used_cols <- used_cols[used_cols %in% colnames(md)]
            other_cols <- setdiff(colnames(md), used_cols)
            if (length(other_cols) <= 1) return(md)

            other_df <- md[, other_cols, drop = FALSE]
            other_df <- lapply(other_df, function(x) {
                if (is.factor(x)) as.character(x) else x
            })

            other_name <- "other_meta"
            if (other_name %in% colnames(md)) {
                other_name <- make.unique(c(colnames(md), other_name))
                other_name <- other_name[length(other_name)]
            }

            md[[other_name]] <- do.call(paste, c(other_df, sep = ":"))
            md <- md[, c(used_cols, other_name), drop = FALSE]
            md
        }

        groupby_options <- reactiveVal(character())
        splitby_options <- reactiveVal(character())
        observe({
            tb <- bulk_tb()
            md <- bulk_meta()
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

        plot_heatmap <- reactive({
            plot_tb <- tb_for_plot()
            plot_md <- md_for_plot()
            req(plot_tb, plot_md, groupby_param(), input$gene)
            plot_md <- collapse_other_meta(plot_md, groupby_param(), splitby_param())
            hmap <- HeatmapPseudoBulk_bin(
                plot_tb,
                plot_md,
                genes = input$gene,
                groupby = groupby_param(),
                splitby = splitby_param(),
                cluster_genes = isTRUE(input$cluster_genes)
            )
            draw(hmap)
        })

        output$plot_heatmap <- renderPlot(plot_heatmap(), res = 96) |> bindEvent(input$plot, tb_for_plot(), md_for_plot())

        output$plot_heatmap_download <- downloadHandler(
            filename = function() {
                groupby_str <- groupby_param()
                splitby_str <- splitby_param()

                paste0(
                    "Heatmap_", dataname,
                    "_groupby", groupby_str,
                    ifelse(is.null(splitby_str), "", paste0("_splitby", splitby_str)),
                    ".", input$format
                )
            },
            content = function(file) {
                ggsave(filename = file, plot = plot_heatmap(), width = input$width, height = input$height, dpi = 300)
            }
        )
    })
}


modServer_PseudoBulkHeatmap_MCCD14 <- function(id, bulk_tb, bulk_meta, dataname, groupby_column = NULL, splitby_column = NULL, subsetby_columns = NULL) {
    modServer_PseudoBulkHeatmap_bin(
        id = id,
        bulk_tb = bulk_tb,
        bulk_meta = bulk_meta,
        dataname = dataname,
        groupby_column = groupby_column,
        splitby_column = splitby_column,
        subsetby_columns = subsetby_columns
    )
}
