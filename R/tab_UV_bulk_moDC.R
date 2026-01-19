tabUI_UV_bulk_moDC <- function(id) {
    ns <- NS(id)
    tagList(
        bslib::layout_columns(
            bslib::card(
                bslib::card_body(
                    modUI_UV_BulkBoxPlot(ns("bulk_boxplot"))
                )
            ),
            bslib::card(
                bslib::card_header("Treatment conditions"),
                bslib::card_body(
                    tags$div(
                        tags$strong("Conditions"),
                        tags$ul(
                            tags$li(tags$strong("Mock:"), " supernatant from keratinocyte"),
                            tags$li(tags$strong("KC_media:"), " keratinocyte media"),
                            tags$li(tags$strong("UV50:"), " supernatant from keratinocyte treated with 50 mJ/cm",tags$sup("2")," UVB"),
                            tags$li(tags$strong("UV100:"), " supernatant from keratinocyte treated with 100 mJ/cm",tags$sup("2")," UVB"),
                            tags$li(tags$strong("UV50+IFNβ:"), " supernatant from keratinocyte pretreated with IFNβ, then 50 mJ/cm",tags$sup("2")," UVB"),
                            tags$li(tags$strong("UV100+IFNβ:"), " supernatant from keratinocyte pretreated with IFNβ, then 100 mJ/cm",tags$sup("2")," UVB"),
                            tags$li(tags$strong("IFNβ:"), " supernatant from keratinocyte pretreated with IFNβ"),
                            tags$li(tags$strong("Direct IFNβ:"), " IFNβ 50ng"),
                            tags$li(tags$strong("LPS:"), " Lipopolysaccharide")
                        ),
                        tags$strong("Cell type source"),
                        tags$ul(
                            tags$li(tags$strong("moDC:"), "monocyte-derived dendritic cells from PBMCs")
                        )
                    )
                )
            ),
            col_width = c(8, 4)
        ),
        bslib::card(
            bslib::card_header("Heatmap for bulk gene expression"),
            bslib::card_body(
                modUI_BulkHeatmap(ns("bulk_heatmap"))
            )
        )
    )
}

tabServer_UV_bulk_moDC <- function(id, data_path, active_tab) {
    moduleServer(id, function(input, output, session) {
        is_active <- reactive(identical(active_tab(), "UV_bulk_moDC"))
        bulk_cpm <- reactiveVal(NULL)
        bulk_meta <- reactiveVal(NULL)

        observeEvent(is_active(), {
            if (!isTRUE(is_active())) return()
            if (is.null(bulk_cpm()) || is.null(bulk_meta())) {
                bulk_cpm_raw <- read.csv(
                    "/Users/yuqing/UMass Medical School Dropbox/Yuqing Wang/Ongoing/data_hosting/shinyApp_content/UV_bulk/DC/bulk_moDC_cpm.csv",
                    row.names = 1,
                    check.names = FALSE
                )
                bulk_meta_raw <- read.csv(
                    "/Users/yuqing/UMass Medical School Dropbox/Yuqing Wang/Ongoing/data_hosting/shinyApp_content/UV_bulk/DC/bulk_moDC_metadata.csv",
                    row.names = 1,
                    stringsAsFactors = FALSE
                )
                bulk_meta_raw$Treatment <- factor(
                    bulk_meta_raw$Treatment,
                    levels = c("Mock", "KC_media", "UV50", "UV100", "UV50+IFNb", "UV100+IFNb", "IFNb", "Direct IFNb", "LPS")
                )
                bulk_cpm(bulk_cpm_raw)
                bulk_meta(bulk_meta_raw)
            }
        }, ignoreInit = TRUE)
        
        moDC_treatment_colors <- c(
            "Mock" = "#e6bcb4",
            "KC_media" = "#ff7f0e",
            "UV50" = "#893b7f",
            "UV100" = "#a33453",
            "UV50+IFNb" = "#debc40",
            "UV100+IFNb" = "#613F68",
            "IFNb" = "#1f497d",
            "Direct IFNb" = "#444444",
            "LPS" = "#999999"
        )

        modServer_UV_BulkBoxPlot(
            id = "bulk_boxplot",
            bulk_cpm = bulk_cpm,
            bulk_meta = bulk_meta,
            dataname = "UV_bulk_moDC",
            gene_default = "IL6",
            groupby_column = "Treatment",
            splitby_column = character(0),
            group_colors = moDC_treatment_colors,
            shape_by = NULL,
            ylab = "CPM"
        )

        modServer_BulkHeatmap(
            id = "bulk_heatmap",
            bulk_tb = bulk_cpm,
            bulk_meta = bulk_meta,
            dataname = "UV_bulk_moDC",
            groupby_column = "Biopsy",
            splitby_column = "Treatment"
        )
    })
}
