tabUI_fourDisease_indrop <- function(id){
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
            col_widths = c(6,6)
        ),
        card(
            bslib::card_header("Violin plot for normalized gene expression in all cell types"),
            bslib::card_body(
                modUI_SeuratVlnPlot(
                    ns("vlnplot_celltype"),
                    width_default = 12,
                    height_default = 4.5,
                    format_default = "png",
                    allow_subset = TRUE
                )
            )
        ),
        layout_columns(
            bslib::card(
                bslib::card_header("Violin plot for normalized gene expression in skin conditions"),
                bslib::card_body(
                    modUI_SeuratVlnPlot(
                        ns("vlnplot_skin_disease"),
                        width_default = 6,
                        height_default = 4.5,
                        format_default = "png",
                        allow_subset = TRUE
                        )
                    )
            ),
            bslib::card(
                bslib::card_header("Heatmap for pseudo-bulked gene expression in skin conditions"),
                bslib::card_body(
                    modUI_PseudoBulkHeatmap(
                        ns("pseudoBulk_heatmap"),
                        width_default = 6,
                        height_default = 10,
                        format_default = "pdf",
                        allow_subset = TRUE
                    )
                )
            ),
            col_widths = c(6,6)
        )
    )
}

tabServer_fourDisease_indrop <- function(id, data_path, active_tab){
    moduleServer(id, function(input, output, session){
        dataname <- "fourDisease_indrop"

        is_active <- reactive(identical(active_tab(), "fourDisease_indrop"))
        srt <- reactiveVal(NULL)
        bulk_tb <- reactiveVal(NULL)

        observeEvent(is_active(), {
            if (!isTRUE(is_active())) return()
            if (is.null(srt()) || is.null(bulk_tb())) {
                if (is.null(srt())) {
                    srt(readRDS(paste0(data_path(), "fourDisease_indrop/fourDisease_indrop_seuratObject.rds")))
                }

                if (is.null(bulk_tb())) {
                    bulk_tb(readRDS(paste0(data_path(), "fourDisease_indrop/fourDisease_indrop_pseudobulk_sum_CellType_Disease_Skin.rds")))
                }
            }
        }, ignoreInit = TRUE)

        bulk_meta <- reactive({
            req(bulk_tb(), srt())
            df <- do.call(rbind, strsplit(colnames(bulk_tb()), ":"))
            df <- as.data.frame(df)
            colnames(df) <- c("CellType","Disease","Skin")
            obj <- srt()
            df[, "CellType"] <- factor(df[,"CellType"], levels = levels(obj$CellType))
            df[, "Disease"] <- factor(df[,"Disease"], levels = levels(obj$Disease))
            df[, "Skin"] <- factor(df[,"Skin"], levels = levels(obj$Skin))
            rownames(df) <- colnames(bulk_tb())
            return(df)
        })

        colors.CellType <- c(
            "Lymph" = "#e7a500",
            "KC" = "#634F8B",
            "MC" = "#15e18d",
            "Mel" = "#0000dd"
        )

        colors.CellSubtype <- c(
            "TC_unk" = "#9013FE",
            "KC_suprabasal" = "#00acc7",
            "KC_granular" = "#c004b9",
            "KC_spinous" = "#a2766a",
            "LC" = "#A35976",
            "Melanocyte" = "#0000dd",
            "KC_activated" = "#E8AECC",
            "KC_basal" = "#634F8B",
            "DC2" = "#DDCDAE",
            "GD" = "#F5A623",
            "NK_immature" = "#417505",
            "Th2" = "#50E3C2",
            "Migratory" = "#486482",
            "CD8_GZMK" = "#F8E71C",
            "Treg" = "#8B572A",
            "CD4" = "#FF7F50",
            "MC_CD14" = "#D7AABF",
            "DC1" = "#947065",
            "Tc17" = "#4A90E2",
            "KC_eccrine" = "#00d3b9",
            "NK_mature" = "#7ED321",
            "CD8_GZMB" = "#BD10E0",
            "MC_Tol" = "#8E7CA8",
            "pDC" = "#84A2E0",
            "Th1" = "#D0021B",
            "Mast" = "#4A4A4A"
        )

        colors.Skin <- c(
            "H" = "#3E6D95",
            "NL" = "#59B1B2",
            "L" = "#A53C8D"
        )

        colors.Disease <- c(
            "HC" = "#3E6D95",
            "CLE" = "#88A49C",
            "DM" = "#F2BDB8",
            "Vit" = "#BDB5B5",
            "Pso" = "#DA9A7E"
        )

               
        modServer_SeuratEmbeddingPlot(
            id = "dimplot",
            srt = srt,
            groupby_default = "CellType",
            groupby_colors_list = list(
                "CellType" = colors.CellType,
                "CellSubtype" = colors.CellSubtype,
                "Skin" = colors.Skin,
                "Disease" = colors.Disease
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
            subsetby_columns = c("Disease", "Skin"),
            feature_default = "IFNG",
            groupby_colors = colors.CellSubtype,
            groupby_colors_by = "CellSubtype"
        )

        modServer_SeuratVlnPlot(
            id = "vlnplot_skin_disease",
            srt = srt,
            dataname = dataname,
            groupby_column = "Skin",
            splitby_column = "Disease",
            subsetby_columns = c("CellSubtype"),
            feature_default = "IFNG",
            groupby_colors = colors.Skin,
            groupby_colors_by = "Skin"
        )

        modServer_PseudoBulkHeatmap(
            id = "pseudoBulk_heatmap",
            bulk_tb = bulk_tb,
            bulk_meta = bulk_meta,
            dataname = dataname,
            groupby_column = "Skin",
            splitby_column = "Disease",
            subsetby_columns = c("CellType")
        )
        invisible(list(srt = srt))
    })
}
