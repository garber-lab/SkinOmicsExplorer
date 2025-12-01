modUI_BulkSubset <- function(id){
    ns <- NS(id)
    tagList(
        wellPanel(
            uiOutput(ns("subsetby_ui")),
            actionButton(ns("run"), "Subset")
        )
    )
}

modServer_BulkSubset <- function(id, bulk_tb, bulk_meta, subsetby_columns = NULL){
    moduleServer(id, function(input, output, session){
        ns <- session$ns

        column_specs <- reactive({
            md <- bulk_meta()
            req(md)

            cols <- subsetby_columns
            if (is.null(cols)) cols <- colnames(md)
            cols <- cols[cols %in% colnames(md)]
            if (length(cols) == 0) return(list())

            specs <- vector("list", length(cols))
            keep_idx <- 1L
            for (i in seq_along(cols)) {
                column <- cols[i]
                values <- md[[column]]

                cat_values <- values # all categorical for bulk meta
                if (is.factor(cat_values)) {
                    cat_values <- as.character(cat_values)
                }
                choices <- sort(unique(cat_values))
                if (!length(choices)) next
                specs[[keep_idx]] <- list(
                    name = column,
                    type = "categorical",
                    input_ids = list(
                        values = paste0("col_", i, "_values")
                    ),
                    choices = choices
                )
                keep_idx <- keep_idx + 1L
            }

            if (keep_idx == 1L) {
                list()
            } else {
                specs[seq_len(keep_idx - 1L)]
            }
        })

        output$subsetby_ui <- renderUI({
            specs <- column_specs()
            if (length(specs) == 0) return(NULL)

            controls <- lapply(specs, function(spec){
                selectInput(
                    ns(spec$input_ids$values),
                    label = spec$name,
                    choices = spec$choices,
                    selected = character(0),
                    multiple = TRUE,
                    selectize = TRUE
                )
            })
            controls <- Filter(Negate(is.null), controls)
            if (!length(controls)) return(NULL)
            do.call(tagList, controls)
        })

        subset_ind <- reactive({
            tb <- bulk_tb()
            req(tb)
            specs <- column_specs()
            if (length(specs) == 0) return(tb)

            md <- bulk_meta()
            keep <- rep(TRUE, nrow(md))

            for (spec in specs) {
                column <- spec$name
                selected <- input[[spec$input_ids$values]]
                if (!is.null(selected) && length(selected) > 0) {
                    current <- md[[column]]
                    if (is.factor(current)) current <- as.character(current)
                    keep <- keep & (current %in% selected)
                }
            }

            keep[is.na(keep)] <- FALSE
            keep
        }) |> bindEvent(input$run, ignoreNULL = FALSE)

        subset_ind
    })
}
