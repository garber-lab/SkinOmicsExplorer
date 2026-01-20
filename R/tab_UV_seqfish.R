tabUI_UV_seqfish <- function(id) {
    ns <- NS(id)
    tagList(
        layout_columns(
            bslib::card(
                bslib::card_header("Seurat Embedding Plot"),
                bslib::card_body(modUI_SeuratEmbeddingPlot(
                    ns("dimplot"),
                    width_default = 6,
                    height_default = 6,
                    format_default = "png"
                ))
            ),
            bslib::card(
                bslib::card_header("Seurat Feature Plot"),
                bslib::card_body(modUI_SeuratFeaturePlot(
                    ns("featureplot"),
                    width_default = 6,
                    height_default = 6,
                    format_default = "png"
                ))
            ),
            col_width = c(6, 6)
        ),
        bslib::card(
            bslib::card_header("Violin plot for normalized gene expression in all cell types"),
            bslib::card_body(
                modUI_SeuratVlnPlot(
                    ns("vlnplot_celltype"),
                    width_default = 19,
                    height_default = 4.5,
                    format_default = "png",
                    allow_subset = FALSE
                )
            )
        ),
        bslib::card(
            bslib::card_header("Heatmap for pseudo-bulked gene expression in cell types"),
            bslib::card_body(
                modUI_PseudoBulkHeatmap_bin(
                    ns("pseudoBulk_heatmap"),
                    width_default = 16,
                    height_default = 6,
                    format_default = "pdf",
                    allow_subset = TRUE,
                    show_bins_toggle = TRUE
                )
            )
        ),
        bslib::card(
            bslib::card_header("Spatial embedding Plot"),
            bslib::card_body(modUI_SeuratImageDimPlot(
                ns("image_dimplot"),
                size_perInch_default = 500, # um per inch
                format_default = "png"
            ))
        ),
        layout_columns(
            bslib::card(
                bslib::card_header("Spatial feature Plot"),
                bslib::card_body(modUI_SeuratImageFeaturePlot(
                    ns("image_featureplot"),
                    size_perInch_default = 500, # um per inch
                    format_default = "png"
                ))
            ),
            bslib::card(
                bslib::card_header("Spatial feature Plot (contour)"),
                bslib::card_body(modUI_SeuratImageFeaturePlot_contour(
                    ns("image_featureplot_contour"),
                    size_perInch_default = 500, # um per inch
                    format_default = "png",
                    sigma_default = 100
                ))
            ),
            col_width = c(6, 6)
        )
    )
}

tabServer_UV_seqfish <- function(id, data_path, active_tab) {
    moduleServer(id, function(input, output, session) {
        dataname <- "UV_seqfish"

        is_active <- reactive(identical(active_tab(), "UV_seqfish"))
        srt <- reactiveVal(NULL)
        bulk_tb <- reactiveVal(NULL)

        observeEvent(is_active(), {
            if (!isTRUE(is_active())) return()
            if (is.null(srt()) || is.null(bulk_tb())) {
                if (is.null(srt())) {
                    srt(readRDS(paste0(data_path(), "UV_seqfish/UV_seqfish_seuratObject_shiny.rds")))
                }

                if (is.null(bulk_tb())) {
                    bulk_tb(readRDS(paste0(data_path(), "UV_seqfish/UV_seqfish_pseudobulk_sum_subCellType.pub3.1.MCCD14_CellType.pub3_Condition_bin30.rds")))
                }
            }
        }, ignoreInit = TRUE)

        bulk_meta <- reactive({
            req(bulk_tb())
            df <- do.call(rbind, strsplit(colnames(bulk_tb()), ":"))
            df <- as.data.frame(df)
            colnames(df) <- c("CellSubtype", "CellType", "Condition")
            bin_pattern <- "_bin[0-9]+$"
            has_bin <- grepl(bin_pattern, df$CellSubtype)
            df$bin <- NA
            df$bin[has_bin] <- sub("^.*_bin", "bin", df$CellSubtype[has_bin])
            df$CellSubtype[has_bin] <- sub(bin_pattern, "", df$CellSubtype[has_bin])
            rownames(df) <- colnames(bulk_tb())
            return(df)
        })

        fov.sizes.um <- list(
            "UV258fov1" = c(width = 3509.529, height = 3957.394),
            "UV258fov2" = c(width = 3202.002, height = 3899.219),
            "UV258fov3" = c(width = 3269.261, height = 4943.197)
        )

        fov.scalebar.position <- c(
            "UV258fov1" = "bottomright",
            "UV258fov2" = "bottomright",
            "UV258fov3" = "bottomleft"
        )

        colors.CellType <- c(
            "KC" = "#634F8B",
            "Mel" = "#0000dd",
            "MC" = "#15e18d",
            "Lymph" = "#e7a500",
            "EC" = "#d70000",
            "Schwann" = "#5c00ab",
            "FB" = "#005759"
        )
        colors.CellSubtype <- c(
            "KC_basal" = "#634F8B", #' #645473'
            "KC_cycle" = "#FFD252",
            "KC_spinous" = "#00acc7",
            "KC_granular_deep" = "#a2766a",
            "KC_granular_sup" = "#c004b9",
            "Melanocyte" = "#0000dd",
            "KC_spinous_1" = "#E7ACC7",
            "KC_spinous_2" = "#00acc6",
            "KC_granular_deep_1" = "#a2766b",
            "KC_granular_deep_2" = "#41766A",
            "KC_bulge" = "#ff8596",
            "KC_outer_sheath_2" = "#6c004f",
            "KC_outer_sheath_1" = "#5AB1B2",
            "KC_inner_sheath" = "#ff7852",
            "KC_eccrine_ductal" = "#00d3b9",
            "KC_eccrine_glandular" = "#790000",
            "Myeloid" = "#15e18d",
            "Lymphocyte" = "#e7a500",
            "SMC" = "#ff7fd1",
            "EC" = "#d70000",
            "Lymphatic_EC" = "#739b7d",
            "Mast" = "#8f7b01",
            "Schwann" = "#5c00ab",
            "FB_mesenchymal" = "#005759",
            "FB_proInf_sup" = "#8c3cfe",
            "FB_proInf_deep" = "#ff9999",
            "FB_ITGA6" = "#FFED6F",
            "FB_G0S2" = "#66cc66",
            "FB_RAMP1" = "#bcb7ff",
            "EC_immune_interactive" = "#4DAF4A", # Green
            "Pericyte" = "#984EA3", # Purple
            "Pericyte_activated" = "#FF7F00", # Orange
            "EC_proliferating" = "#FFFF33", # Blue
            "Neutrophil" = "#3F837A",
            "LC" = "#BDD6D0", #
            "MC_unk_1" = "#DDCD42", # unk_1
            "MC_unk_2" = "#999999", # unk_2
            "MC_unk" = "#999999",
            "Migratory" = "#486482", #
            "DC1" = "#947065", #
            "DC2" = "#DDCDAE",
            "MC_CD14" = "#D7AABF",
            "pDC" = "#84A2E0", #
            "B_cell" = "#583E7F",
            "MC_Tol" = "#8E7CA8", #
            "MC_CXCL8" = "#A35976",
            "CD4" = "#FF7F50",
            "NK" = "#417505",
            "Tc17" = "#4A90E2",
            "CD4_CXCL13" = "#D0021B",
            "Treg" = "#8B572A",
            "CD8" = "#F8E71C",
            "CD4_CCL17" = "#50E3C2"
        )

        modServer_SeuratEmbeddingPlot(
            id = "dimplot",
            srt = srt,
            groupby_default = "CellType",
            groupby_colors_list = list(
                "CellType" = colors.CellType,
                "CellSubtype" = colors.CellSubtype
            ),
            dataname = dataname,
            raster = FALSE
        )

        modServer_SeuratFeaturePlot(
            id = "featureplot",
            srt = srt,
            dataname = dataname,
            raster = FALSE,
            feature_default = "IFNG"
        )

        modServer_SeuratVlnPlot(
            id = "vlnplot_celltype",
            srt = srt,
            dataname = dataname,
            groupby_column = "CellSubtype",
            splitby_column = "CellType",
            feature_default = "IFNG"
        )

        modServer_SeuratImageDimPlot(
            id = "image_dimplot",
            srt = srt,
            dataname = dataname,
            fov_choices = NULL,
            groupby_column = "CellSubtype",
            groupby_colors = colors.CellSubtype,
            scalebar_length = 485.437,
            scalebar_numConv = 1.03,
            scalebar_unit = "μm",
            scalebar_position_default = fov.scalebar.position,
            fov.size = fov.sizes.um
        )

        modServer_SeuratImageFeaturePlot(
            id = "image_featureplot",
            srt = srt,
            dataname = dataname,
            fov_choices = NULL,
            feature_default = "IFNG",
            scalebar_length = 485.437,
            scalebar_numConv = 1.03,
            scalebar_unit = "μm",
            scalebar_position_default = fov.scalebar.position,
            fov.size = fov.sizes.um
        )

        modServer_SeuratImageFeaturePlot_contour(
            id = "image_featureplot_contour",
            srt = srt,
            dataname = dataname,
            colors.cell = colors.CellSubtype,
            fov_choices = NULL,
            feature_default = "IFNG",
            groupby_column = "CellSubtype",
            scalebar_length = 485.437,
            scalebar_numConv = 1.03,
            scalebar_unit = "μm",
            scalebar_position_default = fov.scalebar.position,
            fov.size = fov.sizes.um
        )

        modServer_PseudoBulkHeatmap_bin(
            id = "pseudoBulk_heatmap",
            bulk_tb = bulk_tb,
            bulk_meta = bulk_meta,
            dataname = dataname,
            groupby_column = "CellSubtype",
            splitby_column = "CellType",
            subsetby_columns = c("Condition"),
            show_bins_toggle = TRUE
        )
    })
}
