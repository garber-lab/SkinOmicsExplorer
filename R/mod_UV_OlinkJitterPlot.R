modUI_UV_OlinkJitterPlot <- function(id, width_default = 8, height_default = 3.5, format_default = "pdf") {
    ns <- NS(id)

    plot_panel <- wellPanel(
        selectInput(ns("gene"), "Protein:", choices = NULL),
        bslib::input_switch(
            ns("include_lps"),
            label = "Include LPS",
            value = FALSE
        )
    )

    download_panel <- wellPanel(
        inlineInput("Plot width:", numericInput(ns("width"), NULL, value = width_default, min = 0.1, step = 0.1, width = 70), label_width = "100px"),
        inlineInput("Plot height:", numericInput(ns("height"), NULL, value = height_default, min = 0.1, step = 0.1, width = 70), label_width = "100px"),
        inlineInput("File format:", selectInput(ns("format"), NULL, choices = c("png", "pdf", "jpeg", "tiff"), selected = format_default, width = 70), label_width = "100px"),
        downloadButton(ns("plot_olink_download"), "Download")
    )

    bslib::layout_sidebar(
        sidebar = bslib::sidebar(
            plot_panel,
            download_panel,
            open = "always"
        ),
        bslib::layout_columns(
            bslib::card(
                bslib::card_header("KC and DC"),
                bslib::card_body(plotOutput(ns("plot_kc_dc"), height = "350px"))
            ),
            bslib::card(
                bslib::card_header("DC minus KC"),
                bslib::card_body(plotOutput(ns("plot_diff"), height = "350px"))
            ),
            col_widths = c(8, 4)
        )
    )
}

modServer_UV_OlinkJitterPlot <- function(id, meta, raw, dataname = "UV_olink", feature_default = NULL) {
    moduleServer(id, function(input, output, session) {
        treatment_levels <- c("M", "50", "100", "IFN+50", "IFN", "LPS")
        treatment_labels <- c("Mock", "UV50", "UV100", "UV50+IFNb", "IFNb", "LPS")
        group_colors <- c(
            "Mock" = "#e6bcb4",
            "UV50" = "#893b7f",
            "UV100" = "#a33453",
            "UV50+IFNb" = "#debc40",
            "IFNb" = "#2f3b5c",
            "LPS" = "#777777"
        )

        prepared <- reactive({
            meta_df <- meta()
            raw_df <- raw()
            req(meta_df, raw_df)

            sample_col <- if ("Sample_ID" %in% colnames(raw_df)) "Sample_ID" else colnames(raw_df)[1]
            raw_mat <- raw_df[, setdiff(colnames(raw_df), sample_col), drop = FALSE]

            if ("Sample_ID" %in% colnames(meta_df) && sample_col %in% colnames(raw_df)) {
                match_idx <- match(meta_df$Sample_ID, raw_df[[sample_col]])
                if (!any(is.na(match_idx))) {
                    raw_mat <- raw_mat[match_idx, , drop = FALSE]
                }
            }

            list(meta_df = meta_df, raw_mat = raw_mat)
        })

        gene_choices <- reactive({
            data <- prepared()
            req(data$raw_mat)
            numeric_mat <- data.matrix(data$raw_mat)
            keep <- colSums(numeric_mat, na.rm = TRUE) > 0
            colnames(data$raw_mat)[keep]
        })

        observe({
            choices <- gene_choices()
            req(length(choices) > 0)

            selected <- input$gene
            if (is.null(selected) || !(selected %in% choices)) {
                selected <- if (!is.null(feature_default) && feature_default %in% choices) feature_default else choices[1]
            }

            updateSelectInput(session, "gene", choices = choices, selected = selected)
        })

        build_celltype_df <- function(meta_df, raw_mat, gene, celltype, include_lps = FALSE) {
            idx <- meta_df$celltype == celltype
            meta_sub <- meta_df[idx, , drop = FALSE]
            values <- raw_mat[idx, gene]

            df <- data.frame(
                value = as.numeric(values),
                treatment = factor(meta_sub$treatment, levels = treatment_levels, labels = treatment_labels),
                pair_ID = if ("pair_ID" %in% colnames(meta_sub)) meta_sub$pair_ID else NA,
                stringsAsFactors = FALSE
            )
            df <- df[!is.na(df$value) & !is.na(df$treatment), , drop = FALSE]
            if (!isTRUE(include_lps)) {
                df <- df[df$treatment != "LPS", , drop = FALSE]
            }
            df
        }

        build_diff_df <- function(kc_df, dc_df) {
            if (!("pair_ID" %in% colnames(kc_df)) || !("pair_ID" %in% colnames(dc_df))) {
                return(NULL)
            }
            kc_df <- kc_df[!is.na(kc_df$pair_ID), , drop = FALSE]
            dc_df <- dc_df[!is.na(dc_df$pair_ID), , drop = FALSE]
            if (nrow(kc_df) == 0 || nrow(dc_df) == 0) {
                return(NULL)
            }

            merged <- merge(
                dc_df[, c("pair_ID", "treatment", "value"), drop = FALSE],
                kc_df[, c("pair_ID", "treatment", "value"), drop = FALSE],
                by = c("pair_ID", "treatment"),
                suffixes = c("_dc", "_kc")
            )
            if (nrow(merged) == 0) {
                return(NULL)
            }
            diff_df <- data.frame(
                value = merged$value_dc - merged$value_kc,
                treatment = factor(merged$treatment, levels = treatment_labels),
                stringsAsFactors = FALSE
            )
            diff_df[diff_df$treatment != "LPS", , drop = FALSE]
        }

        build_plot <- function(df, title, ylab_text, show_legend = TRUE) {
            if (is.null(df) || nrow(df) == 0) {
                return(ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::labs(title = title, subtitle = "No data"))
            }

            plot_colors <- manual_scale_values(group_colors, df$treatment)

            base_plot <- ggplot2::ggplot(df, ggplot2::aes(x = treatment, y = value, color = treatment)) +
                ggplot2::geom_point(
                    position = ggbeeswarm::position_beeswarm(cex = 3),
                    size = 1
                ) +
                ggplot2::stat_summary(fun = mean, geom = "crossbar", width = 0.5, linewidth = 0.2, color = "black") +
                ggplot2::scale_color_manual(values = plot_colors) +
                ggplot2::theme_classic() +
                ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
                ggplot2::labs(title = title, y = ylab_text, x = NULL)

            if (!isTRUE(show_legend)) {
                base_plot <- base_plot + ggplot2::theme(legend.position = "none")
            }

            base_plot
        }

        plots <- reactive({
            data <- prepared()
            req(input$gene, input$gene %in% colnames(data$raw_mat))

            gene <- input$gene
            include_lps <- isTRUE(input$include_lps)
            kc_df <- build_celltype_df(data$meta_df, data$raw_mat, gene, "KC", include_lps = FALSE)
            dc_df <- build_celltype_df(data$meta_df, data$raw_mat, gene, "DC", include_lps = include_lps)
            diff_df <- build_diff_df(kc_df, dc_df)
            kc_levels <- length(unique(kc_df$treatment))
            dc_levels <- length(unique(dc_df$treatment))

            list(
                kc = build_plot(kc_df, "KC", "Concentration (pg/mL)", show_legend = FALSE),
                dc = build_plot(dc_df, "DC", "Concentration (pg/mL)", show_legend = FALSE),
                diff = build_plot(diff_df, "DC minus KC", "DC - KC (pg/mL)", show_legend = FALSE),
                kc_levels = kc_levels,
                dc_levels = dc_levels
            )
        })

        plot_kc_dc <- reactive({
            p <- plots()
            rel_widths <- c(max(p$kc_levels, 1), max(p$dc_levels, 1))
            cowplot::plot_grid(p$kc, p$dc, nrow = 1, align = "h", rel_widths = rel_widths)
        })

        plot_all <- reactive({
            p <- plots()
            cowplot::plot_grid(p$kc, p$dc, p$diff, nrow = 1, align = "h")
        })

        output$plot_kc_dc <- renderPlot(plot_kc_dc(), res = 96)

        output$plot_diff <- renderPlot({
            plots()$diff
        }, res = 96)

        output$plot_olink_download <- downloadHandler(
            filename = function() {
                paste0("Olink_", dataname, "_", input$gene, ".", input$format)
            },
            content = function(file) {
                ggplot2::ggsave(filename = file, plot = plot_all(), width = input$width, height = input$height, dpi = 300)
            }
        )
    })
}
