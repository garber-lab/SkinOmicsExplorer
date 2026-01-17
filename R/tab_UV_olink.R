tabUI_UV_olink <- function(id) {
    ns <- NS(id)
    bslib::card(
        bslib::card_body(
            modUI_UV_OlinkJitterPlot(ns("olink_jitter"))
        )
    )
}

tabServer_UV_olink <- function(id, data_path) {
    moduleServer(id, function(input, output, session) {
        meta <- reactive({
            path <- paste0(data_path(), "UV_olink/UV_olink_meta_data.xlsx")
            df <- openxlsx::read.xlsx(path)
            df
        })

        raw <- reactive({
            path <- paste0(data_path(), "UV_olink/UV_olink_absolute_concentration.xlsx")
            df <- openxlsx::read.xlsx(path)
            df
        })

        modServer_UV_OlinkJitterPlot(
            id = "olink_jitter",
            meta = meta,
            raw = raw,
            dataname = "UV_olink",
            gene_default = "IL6"
        )
    })
}
