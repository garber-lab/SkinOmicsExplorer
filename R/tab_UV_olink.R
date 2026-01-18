tabUI_UV_olink <- function(id) {
    ns <- NS(id)
    bslib::card(
        bslib::card_body(
            modUI_UV_OlinkJitterPlot(ns("olink_jitter"))
        )
    )
}

tabServer_UV_olink <- function(id, data_path, active_tab) {
    moduleServer(id, function(input, output, session) {
        is_active <- reactive(identical(active_tab(), "UV_olink"))
        meta <- reactiveVal(NULL)
        raw <- reactiveVal(NULL)

        observeEvent(is_active(), {
            if (!isTRUE(is_active())) return()
            if (is.null(meta()) || is.null(raw())) {
                if (is.null(meta())) {
                    path <- paste0(data_path(), "UV_olink/UV_olink_meta_data.xlsx")
                    meta(openxlsx::read.xlsx(path))
                }

                if (is.null(raw())) {
                    path <- paste0(data_path(), "UV_olink/UV_olink_absolute_concentration.xlsx")
                    raw(openxlsx::read.xlsx(path))
                }
            }
        }, ignoreInit = TRUE)

        modServer_UV_OlinkJitterPlot(
            id = "olink_jitter",
            meta = meta,
            raw = raw,
            dataname = "UV_olink",
            gene_default = "IL6"
        )
    })
}
