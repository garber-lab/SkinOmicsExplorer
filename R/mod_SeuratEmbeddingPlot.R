modUI_SeuratEmbeddingPlot <- function(id){
    ns <- NS(id)
    bslib::layout_sidebar(
        sidebar = bslib::sidebar(
            wellPanel(
                selectInput(
                ns("reduction"),
                "Plot using embedding:",
                choices = NULL
            ),
            selectInput(
                ns("groupby"),
                "Color the cells by:",
                choices = NULL
            ),
            bslib::input_switch(ns("label"), "Label in plot", value = T),
            bslib::input_switch(ns("legend"), "Show legend", value = F),
            actionButton(ns("plot"), "Plot")
            ),
            wellPanel(
                inlineInput("Plot width:", numericInput(ns("width"),NULL, value = 6, min = 0.1, step = 0.1, width = 70), label_width = "90px"),
                inlineInput("Plot height:", numericInput(ns("height"),NULL, value = 6, min = 0.1, step = 0.1, width = 70), label_width = "90px"),
                inlineInput("File format:", selectInput(ns("format"),NULL, choices = c("png","pdf","jpeg","tiff"), width = 70), label_width = "90px"),
                downloadButton(ns("plot_embedding_download"), "Download plot")
            )
        ),
        plotOutput(ns("plot_embedding"))
    )
}


modServer_SeuratEmbeddingPlot <- function(id, srt, groupby_choices=NULL, reduction_choices=NULL, dataname){
    moduleServer(id, function(input, output, session){

        output$test <- renderText({
                is.null(groupby_choices)
            })

        observeEvent(srt(), {
            req(srt())
            reductions <- if(!is.null(reduction_choices)) reduction_choices else Seurat::Reductions(srt())
            updateSelectInput(session, "reduction", choices = reductions)
            groupbys <- if(!is.null(groupby_choices)) {
                groupby_choices
            }else{
                md <- srt()@meta.data
                cols <- colnames(md)[sapply(md, function(x) is.character(x) || is.factor(x))]
                unique(cols)
            }
            updateSelectInput(session, "groupby", choices = groupbys)
        }, ignoreInit = F)

        plot_embedding = reactive({
            req(srt(), input$reduction, input$groupby)
			g <- Seurat::DimPlot(
                srt(), 
                reduction = input$reduction, 
                group.by = input$groupby, 
                shuffle = T, alpha = 0.8,
                label = isTRUE(input$label)) + coord_fixed(ratio = 1)
            if (!isTRUE(input$legend)){
                g <- g + NoLegend()
            }
            return(g)
		})
        
        output$plot_embedding <- renderPlot(plot_embedding()) |> bindEvent(input$plot)

        output$plot_embedding_download <- downloadHandler(
            filename = function(){
                paste0("DimPlot_",dataname,"_groupby",input$groupby,"_",input$reduction,"_label",input$label,"_legend",input$legend,".",input$format)
            },
            content = function(file){
                ggplot2::ggsave(filename = file, plot = plot_embedding(), width = input$width, height = input$height, dpi = 300)
            }
        )
    })
}

