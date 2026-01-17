tabUI_fourDisease_nulisa <- function(id) {
    ns <- NS(id)
    tagList(
        bslib::card(
            bslib::card_header("NULISA BoxPlot"),
            bslib::card_body(
                modUI_NulisaBoxPlot(
                    ns("nulisa_boxplot"),
                    plot_width = "800px"
                )
            )
        )
    )
}

tabServer_fourDisease_nulisa <- function(id, data_path) {
    moduleServer(id, function(input, output, session) {
        dataname <- "fourDisease_nulisa"

        df.nulisa <- reactive({
            df <- read.csv(paste0(data_path(), "fourDisease_nulisa/fourDisease_nulisa_shiny.csv"), row.names = 1)
            if ("Skin" %in% colnames(df)) {
                df$Skin <- factor(df$Skin, levels = c("HC", "NL", "L"))
            }
            if ("Disease" %in% colnames(df)) {
                df$Disease <- factor(df$Disease, levels = c("HC", "DM", "CLE", "Pso", "Vit"))
            }
            df
        })

        colors.disease <- c(
            "HC" = "#3E6D95",
            "DM" = "#F2BDB8",
            "CLE" = "#88A49C",
            "Pso" = "#DA9A7E",
            "Vit" = "#BDB5B5"
        )

        colors.skin <- c(
            "Control" = "#3E6D95",
            "NonLesional" = "#59B1B2",
            "Lesional" = "#A53C8D",
            "HC" = "#3E6D95",
            "NL" = "#59B1B2",
            "L" = "#A53C8D"
        )

        colors.by <- list(
            Skin = colors.skin,
            Disease = colors.disease
        )

        modServer_NulisaBoxPlot(
            id = "nulisa_boxplot",
            nls = df.nulisa,
            dataname = dataname,
            groupby_column = "Skin",
            splitby_column = "Disease",
            group.color = colors.by,
            split.color = colors.by,
            gene_default = "IFNG"
        )
    })
}
