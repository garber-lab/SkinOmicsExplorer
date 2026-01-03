tabUI_abbreviations <- function(id) {
    tagList(
        bslib::card(
            bslib::card_header("Abbreviations"),
            bslib::card_body(
                tags$h5("Disease types"),
                tags$ul(
                    tags$li("HC - Healthy Control"),
                    tags$li("DM - Dermatomyositis"),
                    tags$li("CLE - Cutaneous Lupus Erythematosus"),
                    tags$li("Pso - Psoriasis"),
                    tags$li("Vit - Vitiligo"),
                    tags$li("DLE - Discoid Lupus Erythematosus"),
                    tags$li("SLE - Systemic lupus erythematosus")
                ),
                tags$h5("Skin conditions"),
                tags$ul(
                    tags$li("H - Healthy"),
                    tags$li("NL - Non-Lesional"),
                    tags$li("L - Lesional")
                ),
                tags$h5("Tissue type"),
                tags$ul(
                    tags$li("E - Epidermis"),
                    tags$li("D - Dermis")
                )
            )
        )
    )
}

tabServer_abbreviations <- function(id, data_path) {
    moduleServer(id, function(input, output, session) {
        invisible(NULL)
    })
}
