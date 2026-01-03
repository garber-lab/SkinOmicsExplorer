library(shiny)
library(bslib)
library(shinyjs)
library(Seurat)
# library(SeuratObject)
# library(future)

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
setwd("/Users/yuqing/projects/apps/SkinOmicsExplorer/")
for (f in list.files("R", "[.][Rr]$", full.names = TRUE)) source(f)


ui <- fluidPage(
  useShinyjs(),
  includeCSS("www/style.css"),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  page_navbar(
    title = "Skin Omics Explorer",
    nav_panel(
      "Home",
      tabUI_home("tab_home")
    ),
    nav_menu(
      "scRNA-seq",
      nav_panel("Four disease inDrop", tabUI_fourDisease_indrop("tab_fourDisease_indrop")),
      nav_panel("CLE GSE179633", tabUI_CLE_gse179633("tab_CLE_gse179633")),
      nav_panel("DM 10x", tabUI_DM_10x("tab_DM_10x"))
    ),
    nav_menu(
      "Proteomics",
      nav_panel("Four disease NULISA", tabUI_fourDisease_nulisa("tab_fourDisease_nulisa")),
      nav_panel("UV in vitro OLINK", tabUI_UV_olink("tab_UV_olink"))
    ),
    nav_menu(
      "Spatial Transcriptomics",
      nav_panel("Four disease seqFISH", tabUI_fourDisease_seqfish("tab_fourDisease_seqfish")),
      nav_panel("UV on CLE NL skin seqFISH", tabUI_UV_seqfish("tab_UV_seqfish"))
    ),
    nav_menu(
      "bulk RNA-seq",
      nav_panel("UV in vitro bulk RNA-seq", tabUI_UV_bulk("tab_UV_bulk"))
    ),
    nav_spacer(),
    nav_panel("Abbreviations", tabUI_abbreviations("tab_abbreviation")),
    nav_panel("Data Access", tabUI_dataAccess("tab_dataAccess")),
    nav_panel("Contact", tabUI_contact("tab_contact"))
  )
)

server <- function(input, output, session) {
  data_path <- reactive({
    if (Sys.info()[[7]] == "root") {
      return("/home/app_data/")
    } else {
      return("/Users/yuqing/UMass Medical School Dropbox/Yuqing Wang/Ongoing/data_hosting/shinyApp_content/")
    }
  })

  tabServer_home("tab_home", data_path)
  transcript_data <- tabServer_fourDisease_indrop("tab_fourDisease_indrop", data_path)
  tabServer_CLE_gse179633("tab_CLE_gse179633", data_path)
  tabServer_DM_10x("tab_DM_10x", data_path)
  tabServer_fourDisease_nulisa("tab_fourDisease_nulisa", data_path)
  tabServer_UV_olink("tab_UV_olink", data_path)
  tabServer_fourDisease_seqfish("tab_fourDisease_seqfish", data_path)
  tabServer_UV_seqfish("tab_UV_seqfish", data_path)
  tabServer_UV_bulk("tab_UV_bulk", data_path)
  tabServer_abbreviations("tab_abbreviation", data_path)
  tabServer_dataAccess("tab_dataAccess", data_path)
  tabServer_contact("tab_contact", data_path)
}

options(shiny.host = "0.0.0.0", shiny.port = 8888, shiny.reactlog = TRUE)
shinyApp(ui = ui, server = server)
