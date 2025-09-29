modUI_SeuratVlnPlot <- function(id, width_default = 6, height_default = 6, format_default = "png"){
    ns <- NS(id)
    layout_sidebar(
        sidebar = sidebar(
            wellPanel(
                selectizeInput(ns("gene"), "Gene name:", choices = NULL),
                uiOutput(ns("groupby_ui")),
                uiOutput(ns("splitby_ui")),
                input_switch(ns("plotmean"), "Plot mean", value = T),
                actionButton(ns("plot"), "Plot")
            ),
            wellPanel(
                inlineInput("Plot width:", numericInput(ns("width"), NULL, value = width_default, min = 0.1, step = 0.1, width = 70), label_width = "90px"),
                inlineInput("Plot height:", numericInput(ns("height"),NULL, value = height_default, min = 0.1, step = 0.1, width = 70), label_width = "90px"),
                inlineInput("File format:", selectInput(ns("format"),NULL, choices = c("png", "pdf", "jpeg", "tiff"), selected = format_default, width = 70), label_width = "90px"),
                downloadButton(ns("plot_violin_download"), "Download")
            )
        ),
        plotOutput(ns("plot_violin"))
    )
}

modServer_SeuratVlnPlot <- function(id, srt, dataname, groupby_choices=NULL, splitby_choices=NULL){
    moduleServer(id, function(input, output, session){
        ns <- session$ns
        groupby_options <- reactiveVal(character())
        splitby_options <- reactiveVal(character())

        # update the choices of groupby splitby inputs
        observeEvent(srt(), {
            req(srt())
            groupbys <- if(is.null(groupby_choices)){
                md <- srt()@meta.data
                cols <- colnames(md)[sapply(md, function(x) is.character(x) || is.factor(x))]
                unique(cols)
            }else{
                groupby_choices
            }
            splitbys <- if(is.null(splitby_choices)){
                md <- srt()@meta.data
                cols <- colnames(md)[sapply(md, function(x) is.character(x) || is.factor(x))]
                unique(cols)
            }else{
                splitby_choices
            }
            updateSelectizeInput(session, "gene", choices = rownames(srt()), server = T)
            groupby_options(groupbys)
            splitby_options(splitbys)
        }, ignoreInit = F)

        output$groupby_ui <- renderUI({
            choices <- groupby_options()
            req(length(choices) > 1) # returns NULL (hides UI) if condition fails
            selectInput(ns("groupby"), "Group by:", choices = choices)
        })

        output$splitby_ui <- renderUI({
            choices <- splitby_options()
            req(length(choices) > 1) # returns NULL (hides UI) if condition fails
            selectInput(ns("splitby"), "Bin groups by:", choices = c("Unselect" = "null", choices), selectize = FALSE)
        })

        groupby_param <- reactive({
            choices <- groupby_options()
            if (length(choices) == 0) return(NULL)
            if (length(choices) == 1) return(choices[1])
            req(input$groupby)
            input$groupby
        })

        splitby_param <- reactive({
            choices <- splitby_options()
            if (length(choices) == 0) return(NULL)
            if (length(choices) == 1) return(choices[1])
            req(input$splitby)
            if (input$splitby == "null") return(NULL)
            input$splitby
        })

        plot_violin <- reactive({
            req(srt(), groupby_param(), input$gene)
            g <- VlnPlot.xlabel(
                srt(),
                input$gene,
                group.by = groupby_param(),
                split.by = splitby_param(),
                plot_mean = input$plotmean)
            return(g)
        })

        output$plot_violin <- renderPlot(plot_violin(), res = 96) |> bindEvent(input$plot)

        output$plot_violin_download <- downloadHandler(
            filename = function(){
                groupby_str <- groupby_param()
                splitby_str <- splitby_param()
                
                paste0(
                    "VlnPlot_",dataname,
                    "_gene_",input$gene,
                    "_groupby",groupby_str,
                    ifelse(is.null(splitby_str), "", paste0("_splitby",splitby_str)),
                    ".",input$format)
            },
            content = function(file){
                ggsave(filename = file, plot = plot_violin(), width = input$width, height = input$height, dpi = 300)
            }
        )


        # # subset srt data
        # srt <- reactive()
    })
}
