tabUI_UV_bulk_FB <- function(id) {
    ns <- NS(id)
    tagList(
        bslib::card(
            bslib::card_body(
                modUI_UV_BulkBoxPlot(ns("bulk_boxplot"))
            )
        ),
        bslib::card(
            bslib::card_header("Heatmap for bulk gene expression"),
            bslib::card_body(
                modUI_BulkHeatmap(ns("bulk_heatmap"))
            )
        )
    )
}

tabServer_UV_bulk_FB <- function(id, data_path) {
    moduleServer(id, function(input, output, session) {
        bulk_cpm_raw <- read.csv(
            "/Users/yuqing/UMass Medical School Dropbox/Yuqing Wang/Ongoing/data_hosting/shinyApp_content/UV_bulk/FB/Fib_sup_KC_normalized_counts_combatseq.tsv",
            row.names = 1,
            check.names = FALSE,
            sep = "\t"
        )
        bulk_meta_raw <- read.csv(
            "/Users/yuqing/UMass Medical School Dropbox/Yuqing Wang/Ongoing/data_hosting/shinyApp_content/UV_bulk/FB/bulk_FB_metadata.csv",
            row.names = 1,
            stringsAsFactors = FALSE
        )
        bulk_meta_raw$Treatment <- factor(
            bulk_meta_raw$Treatment,
            levels = c("Fib_media","Mock","UV100","UV50+IFNb","IFNb","Direct IFNb","Direct IFNg","Direct TNF","Direct IL-1")
        )
        bulk_cpm <- reactive(bulk_cpm_raw)
        bulk_meta <- reactive(bulk_meta_raw)

        FB_treatment_colors <- c(
            "Mock" = "#e6bcb4",
            "Fib_media" = "#ff7f0e",
            "UV100" = "#a33453",
            "UV50+IFNb" = "#debc40",
            "IFNb" = "#1f497d",
            "Direct IFNb" = "#444444",
            "Direct IFNg" = "#7FBDDA",
            "Direct TNF" = "#9575AF",
            "Direct IL-1" = "#009E73"
        )

        modServer_UV_BulkBoxPlot(
            id = "bulk_boxplot",
            bulk_cpm = bulk_cpm,
            bulk_meta = bulk_meta,
            dataname = "UV_bulk_FB",
            gene_default = "IL6",
            groupby_column = "Treatment",
            group_colors = FB_treatment_colors,
            splitby_column = character(0),
            shape_by = NULL,
            ylab = "CPM"
        )

        modServer_BulkHeatmap(
            id = "bulk_heatmap",
            bulk_tb = bulk_cpm,
            bulk_meta = bulk_meta,
            dataname = "UV_bulk_FB",
            groupby_column = "Biopsy",
            splitby_column = "Treatment"
        )
    })
}
