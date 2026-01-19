modUI_NulisaBoxPlot <- function(id, width_default = 6, height_default = 3.5, format_default = "pdf", allow_download = TRUE, plot_width = NULL) {
    ns <- NS(id)

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

    plot_panel <- wellPanel(
        selectizeInput(ns("gene"), "Protein name:", choices = NULL),
        uiOutput(ns("groupby_ui")),
        uiOutput(ns("splitby_ui")),
        actionButton(ns("plot"), "Plot")
    )

    layout_sidebar(
        sidebar = sidebar(
            tagList(plot_panel, download_panel),
            open = "always"
        ),
        plotOutput(ns("plot_boxplot"), width = if (is.null(plot_width)) "100%" else plot_width)
    )
}

modServer_NulisaBoxPlot <- function(id, nls, dataname, groupby_column = NULL, splitby_column = NULL, groupby_colors = NULL, splitby_colors = NULL, feature_default = NULL) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
        groupby_options <- reactiveVal(character())
        splitby_options <- reactiveVal(character())

        observe({
            df <- nls()
            req(df)

            gene_choices <- unique(as.character(df$Target))
            selected_gene <- NULL
            if (!is.null(feature_default) && feature_default %in% gene_choices) {
                selected_gene <- feature_default
            }
            if (is.null(selected_gene)) {
                updateSelectizeInput(session, "gene", choices = gene_choices, server = TRUE)
            } else {
                updateSelectizeInput(session, "gene", choices = gene_choices, selected = selected_gene, server = TRUE)
            }

            groupbys <- if (is.null(groupby_column)) {
                cols <- colnames(df)[sapply(df, function(x) is.character(x) || is.factor(x))]
                setdiff(unique(cols), "Target")
            } else {
                groupby_column[groupby_column %in% colnames(df)]
            }

            splitbys <- if (is.null(splitby_column)) {
                cols <- colnames(df)[sapply(df, function(x) is.character(x) || is.factor(x))]
                setdiff(unique(cols), "Target")
            } else {
                splitby_column[splitby_column %in% colnames(df)]
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

        plot_boxplot <- reactive({
            df <- nls()
            req(df, input$gene, groupby_param())
            group_colors <- resolve_color(groupby_colors, groupby_param())
            if (!is.null(group_colors) && !("black" %in% names(group_colors))) {
                group_colors <- c(group_colors, black = "black")
            }
            plot_obj <- BoxPlot.nulisa(
                nls = df,
                gene = input$gene,
                group.by = groupby_param(),
                split.by = splitby_param(),
                group.color = group_colors,
                ttest = FALSE,
                mean.linewidth = 0.4,
                alpha = 0.6,
                boxplot.linewidth = 0.2,
                jitter.stroke = 0.1,
                warn.mark = FALSE,
                warn.shape = NULL
            )
            if (is.list(plot_obj) && !is.null(plot_obj$g)) {
                plot_obj <- plot_obj$g
            }
            split_values <- if (is.null(splitby_param())) NULL else df[[splitby_param()]]
            apply_split_colors(plot_obj, resolve_color(splitby_colors, splitby_param()), split_values)
        })

        output$plot_boxplot <- renderPlot({
            plot_obj <- plot_boxplot()
            if (inherits(plot_obj, "gtable") || inherits(plot_obj, "grob")) {
                grid::grid.draw(plot_obj)
            } else {
                plot_obj
            }
        }, res = 96) |> bindEvent(input$plot, nls())

        output$plot_boxplot_download <- downloadHandler(
            filename = function() {
                groupby_str <- groupby_param()
                splitby_str <- splitby_param()

                paste0(
                    "Nulisa_BoxPlot_", dataname,
                    "_gene_", input$gene,
                    "_groupby", groupby_str,
                    ifelse(is.null(splitby_str), "", paste0("_splitby", splitby_str)),
                    ".", input$format
                )
            },
            content = function(file) {
                ggsave(filename = file, plot = plot_boxplot(), width = input$width, height = input$height, dpi = 300)
            }
        )
    })
}
