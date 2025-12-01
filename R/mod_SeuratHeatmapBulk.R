# modUI_SeuratHeatmapBulk <- function(id, allow_subset = FALSE, width_default = 8, height_default = 8, format_default = "pdf", allow_download = TRUE){
#     ns <- NS(id)
    
#     download_panel <- if (isTRUE(allow_download)) {
#         wellPanel(
#             inlineInput("Plot width:", numericInput(ns("width"), NULL, value = width_default, min = 0.1, step = 0.1, width = 70), label_width = "90px"),
#             inlineInput("Plot height:", numericInput(ns("height"),NULL, value = height_default, min = 0.1, step = 0.1, width = 70), label_width = "90px"),
#             inlineInput("File format:", selectInput(ns("format"),NULL, choices = c("png", "pdf", "jpeg", "tiff"), selected = format_default, width = 70), label_width = "90px"),
#             downloadButton(ns("plot_heatmap_download"), "Download")
#         )
#     } else {
#         NULL
#     }
    
#     plot_panel <- wellPanel(
#         selectizeInput(ns("gene"), "Gene name:", choices = NULL, multiple = TRUE),
#         uiOutput(ns("groupby_ui")),
#         uiOutput(ns("splitby_ui")),
#         actionButton(ns("plot"), "Plot")
#     )
#     sidebarl_content <- if (!allow_subset) {
#         tagList(plot_panel, download_panel)
#     } else {
#         accordion(
#             accordion_panel(
#                 title = "Subset data (optional)",
#                 value = "subset",
#                 modUI_SeuratSubset(ns("subset"))
#             ),
#             accordion_panel(
#                 title = "Plot options",
#                 value = "plot",
#                 tagList(plot_panel, download_panel)
#             ),
#             multiple = FALSE,
#             open = "plot"
#         )
#     }

#     layout_sidebar(
#         sidebar = sidebar(
#             sidebar_content,
#             open = "always"
#         ),
#         plotOutput(ns("plot_heatmap"))
#     )
# }


# modServer_SeuratHeatmapBulk <- function(id, srt, dataname, groupby_column=NULL, splitby_column=NULL, subsetby_columns = NULL){
#     moduleServer(id, function(input, output, session){
#         ns <- session$ns
#         groupby_options <- reactiveVal(character())
#         splitby_options <- reactiveVal(character())
#         has_subset <- !is.null(subsetby_columns) && length(subsetby_columns) > 0
#         subset_srt <- NULL
#         if (has_subset) {
#             subset_srt <- modServer_SeuratSubset(
#                 id = "subset",
#                 srt = srt,
#                 subsetby_columns = subsetby_columns
#             )
#         }

#         srt_for_plot <- reactive({
#             obj <- srt()
#             req(obj)
#             if (!has_subset) return(obj)
#             subset_obj <- subset_srt()
#             if (is.null(subset_obj)) return(obj)
#             if (ncol(subset_obj) == 0) return(obj)
#             subset_obj
#         })

#         observe({
#             req(srt())
#             groupbys <- if (is.null(groupby_column)){
#                 md <- srt()@meta.data
#                 cols <- colnames(md)[sapply(md, function(x) is.character(x) || is.factor(x))]
#                 unique(cols)
#             }else{
#                 groupby_column
#             }

#             splitbys <- if(is.null(splitby_column)){
#                 md <- srt()@meta.data
#                 cols <- colnames(md)[sapply(md, function(x) is.character(x) || is.factor(x))]
#                 unique(cols)
#             }else{
#                 splitby_column
#             }
#             updateSelectizeInput(session, "gene", choices = rownames(srt()), server = T)
#             groupby_options(groupbys)
#             splitby_options(splitbys)
#         })

#         output$groupby_ui <- renderUI({
#             choices <- groupby_options()
#             req(length(choices) > 1) # returns NULL (hides UI) if condition fails
#             selectInput(ns("groupby"), "Group by:", choices = choices)
#         })

#         output$splitby_ui <- renderUI({
#             choices <- splitby_options()
#             req(length(choices) > 1) # returns NULL (hides UI) if condition fails
#             selectInput(ns("splitby"), "Bin groups by:", choices = c("Unselect" = "null", choices))
#         })

#         groupby_param <- reactive({
#             choices <- groupby_options()
#             if (length(choices) == 0) return(NULL)
#             if (length(choices) == 1) return(choices[1])
#             req(input$groupby)
#             input$groupby
#         })

#         splitby_param <- reactive({
#             choices <- splitby_options()
#             if (length(choices) == 0) return(NULL)
#             if (length(choices) == 1) return(choices[1])
#             req(input$splitby)
#             if (input$splitby == "null") return(NULL)
#             input$splitby
#         })

#         plot_heatmap <- reactive({
#             plot_obj <- srt_for_plot()
#             req(plot_obj, groupby_param(), input$gene)
#             g <- 
#         })

#         output$plot_heatmap <- renderPlot(plot_heatmap(), res = 96) |> bindEvent(input$plot, srt_for_plot())

#         output$plot_heatmap_download <- downloadHandler(
#             filename = function(){
#                 groupby_str <- groupby_param()
#                 splitby_str <- splitby_param()
                
#                 paste0(
#                     "Heatmap_",dataname,
#                     "_groupby",groupby_str,
#                     ifelse(is.null(splitby_str), "", paste0("_splitby",splitby_str)),
#                     ".",input$format)
#             },
#             content = function(file){
#                 ggsave(filename = file, plot = plot_heatmap(), width = input$width, height = input$height, dpi = 300)
#             }
#         )