modUI_SeuratSubset <- function(id){
    ns <- NS(id)
    tagList(
        wellPanel(
            uiOutput(ns("subsetby_ui")),
            actionButton(ns("run"), "Subset")
        )
    )
}

modServer_SeuratSubset <- function(id, srt, subsetby_columns = NULL){
    moduleServer(id, function(input, output, session){
        ns <- session$ns

        column_specs <- reactive({
            req(srt())
            md <- srt()@meta.data
            cols <- subsetby_columns
            if (is.null(cols)) cols <- colnames(md)
            cols <- cols[cols %in% colnames(md)]
            if (length(cols) == 0) return(list())

            specs <- vector("list", length(cols))
            keep_idx <- 1L
            for (i in seq_along(cols)) {
                column <- cols[i]
                values <- md[[column]]

                if (is.numeric(values)) {
                    numeric_values <- values[!is.na(values)]
                    numeric_values <- numeric_values[is.finite(numeric_values)]
                    if (!length(numeric_values)) next
                    rng <- range(numeric_values)
                    specs[[keep_idx]] <- list(
                        name = column,
                        type = "numeric",
                        input_ids = list(
                            greater = paste0("col_", i, "_gt"),
                            less = paste0("col_", i, "_lt")
                        ),
                        range = rng
                    )
                    keep_idx <- keep_idx + 1L
                } else if (is.character(values) || is.factor(values) || is.logical(values)) {
                    cat_values <- values
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
                if (identical(spec$type, "categorical")) {
                    selectInput(
                        ns(spec$input_ids$values),
                        label = spec$name,
                        choices = spec$choices,
                        selected = character(0),
                        multiple = TRUE,
                        selectize = TRUE
                    )
                } else if (identical(spec$type, "numeric")) {
                    rng <- spec$range
                    tags$div(
                        class = "subset-numeric-control",
                        tags$strong(spec$name),
                        numericInput(
                            ns(spec$input_ids$greater),
                            label = "Greater than",
                            value = rng[1],
                            min = rng[1],
                            max = rng[2]
                        ),
                        numericInput(
                            ns(spec$input_ids$less),
                            label = "Less than",
                            value = rng[2],
                            min = rng[1],
                            max = rng[2]
                        )
                    )
                } else {
                    NULL
                }
            })
            controls <- Filter(Negate(is.null), controls)
            if (!length(controls)) return(NULL)
            do.call(tagList, controls)
        })

        subset_result <- reactive({
            req(srt())
            specs <- column_specs()
            if (length(specs) == 0) return(srt())

            md <- srt()@meta.data
            keep <- rep(TRUE, nrow(md))

            for (spec in specs) {
                column <- spec$name
                if (identical(spec$type, "categorical")) {
                    selected <- input[[spec$input_ids$values]]
                    if (!is.null(selected) && length(selected) > 0) {
                        current <- md[[column]]
                        if (is.factor(current)) current <- as.character(current)
                        keep <- keep & (current %in% selected)
                    }
                } else if (identical(spec$type, "numeric")) {
                    gt <- input[[spec$input_ids$greater]]
                    lt <- input[[spec$input_ids$less]]
                    if (is.null(gt) || is.null(lt)) next
                    column_values <- md[[column]]
                    column_values <- as.numeric(column_values)
                    comparison <- rep(FALSE, length(column_values))
                    if (gt <= lt) {
                        comparison <- column_values >= gt & column_values <= lt
                    } else {
                        comparison <- column_values >= gt | column_values <= lt
                    }
                    comparison[is.na(comparison)] <- FALSE
                    keep <- keep & comparison
                }
            }

            keep[is.na(keep)] <- FALSE
            cells_to_keep <- rownames(md)[keep]
            if (!length(cells_to_keep)) {
                return(srt()[, character(0)])
            }
            srt()[, cells_to_keep, drop = FALSE]
        }) |> bindEvent(input$run, ignoreNULL = FALSE)

        subset_result
    })
}
