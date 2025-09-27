library(shiny)
library(Seurat)
library(SeuratObject)
library(future)

# library(tidyverse)
library(tidyr)
library(ggplot2)
library(plotly)
library(dplyr)

library(cowplot)
# library(gridExtra)
library(scSpatial)
library(AddOns)
# library(sp)
library(ComplexHeatmap)

# library(circlize)

options(scipen = 4) 

ui = fluidPage( 
  
  useShinyjs(),
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  
  navbarPage(
  )
)

server <- function(input, output, session) {
  
  data_path = reactive({
    if (Sys.info()[[7]] == 'root') {
      return("/home/app_data/")
    } else {
      return("data/")
    }
  })
  
  transcript_data = transcriptomicTabServer('transcriptomic_tab', data_path)

}

options(shiny.host = "0.0.0.0", shiny.port = 8789, shiny.reactlog = TRUE)
shinyApp(ui = ui, server = server)