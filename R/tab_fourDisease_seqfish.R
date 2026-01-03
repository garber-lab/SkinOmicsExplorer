tabUI_fourDisease_seqfish <- function(id) {
    ns <- NS(id)
    tagList(
        layout_columns(
            bslib::card(
                class = "mb-3",
                bslib::card_body(
                    tags$div(
                        tags$h5("Abbreviations"),
                        tags$p(tags$strong("Disease types:")),
                        tags$ul(
                            tags$li("DM - Dermatomyositis"),
                            tags$li("CLE - Cutaneous Lupus Erythematosus"),
                            tags$li("Pso - Psoriasis"),
                            tags$li("Vit - Vitiligo")
                        ),
                        tags$h5("Abbreviations"),
                        tags$ul(
                            tags$li("DM - UV109, UV253"),
                            tags$li("CLE - UV243, UV260"),
                            tags$li("Pso - UV238, UV239"),
                            tags$li("Vit - VB268")
                        )
                    )
                )
            ),
            bslib::card(
                bslib::card_header("Seurat Embedding Plot"),
                bslib::card_body(modUI_SeuratEmbeddingPlot(
                    ns("dimplot"),
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
                    format_default = "png"
                ))
            ),
            col_width = c(6, 6)
        )
    )
}

tabServer_fourDisease_seqfish <- function(id, data_path) {
    moduleServer(id, function(input, output, session) {
        dataname <- "fourDisease_seqfish"

        srt <- reactive({
            progress <- Progress$new(session, min = 0, max = 1)
            on.exit(progress$close())
            progress$set(message = "Reading Seurat Object", detail = "about 20 seconds")

            obj <- readRDS(paste0(data_path(), "fourDisease_seqfish/fourDisease_seqfish_seuratObject_shiny.rds"))
            return(obj)
        })

        fov.sizes.um <- list(
            "UV109fov1" = c(width = 3589.447, height = 4090.542),
            "UV109fov2" = c(width = 3866.105, height = 4012.468),
            "UV253fov1" = c(width = 3297.545, height = 2918.093),
            "UV253fov2" = c(width = 3509.725, height = 2850.422),
            "UV238fov1" = c(width = 3591.198, height = 3973.637),
            "UV238fov2" = c(width = 3503.545, height = 3595.112),
            "UV239fov1" = c(width = 2165.369, height = 2892.961),
            "UV239fov2" = c(width = 2180.407, height = 2822.097),
            "VB268fov1" = c(width = 1604.225, height = 2102.436),
            "VB268fov2" = c(width = 1988.106, height = 2207.908),
            "UV243fov1" = c(width = 2105.732, height = 2687.991),
            "UV260fov1" = c(width = 2939.62, height = 4756.334),
            "UV260fov2" = c(width = 2632.783, height = 4670.432)
        )

        fov.scalebar.position <- c(
            "UV109fov1" = "bottomright",
            "UV109fov2" = "bottomright",
            "UV253fov1" = "bottomright",
            "UV253fov2" = "bottomright",
            "UV243fov1" = "bottomleft",
            "UV260fov1" = "bottomleft",
            "UV260fov2" = "bottomleft",
            "UV238fov1" = "bottomright",
            "UV238fov2" = "bottomright",
            "UV239fov1" = "bottomright",
            "UV239fov2" = "bottomright",
            "VB268fov1" = "bottomright",
            "VB268fov2" = "bottomright"
        )

        colors.celltype <- c(
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
            srt,
            dataname = dataname
        )

        modServer_SeuratVlnPlot(
            id = "vlnplot_celltype",
            srt = srt,
            dataname = dataname,
            groupby_column = "CellSubtype",
            splitby_column = "CellType"
        )

        modServer_SeuratImageDimPlot(
            id = "image_dimplot",
            srt = srt,
            dataname = dataname,
            fov_choices = NULL,
            groupby_column = "CellSubtype",
            groupby_colors = colors.celltype,
            scalebar_length = 4854.369,
            scalebar_numConv = 0.103,
            scalebar_unit = "μm",
            scalebar_position_default = fov.scalebar.position,
            fov.size = fov.sizes.um
        )

        modServer_SeuratImageFeaturePlot(
            id = "image_featureplot",
            srt = srt,
            dataname = dataname,
            fov_choices = NULL,
            scalebar_length = 4854.369,
            scalebar_numConv = 0.103,
            scalebar_unit = "μm",
            scalebar_position_default = fov.scalebar.position,
            fov.size = fov.sizes.um
        )

        modServer_SeuratImageFeaturePlot_contour(
            id = "image_featureplot_contour",
            srt = srt,
            dataname = dataname,
            colors.celltype = colors.celltype,
            fov_choices = NULL,
            scalebar_length = 4854.369,
            scalebar_numConv = 0.103,
            scalebar_unit = "μm",
            scalebar_position_default = fov.scalebar.position,
            fov.size = fov.sizes.um
        )
    })
}
