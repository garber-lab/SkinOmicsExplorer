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
    id = "main_nav",
    nav_panel(
      "Home",
      value = "home",
      tabUI_home("tab_home")
    ),
    nav_menu(
      "scRNA-seq",
      nav_panel("Four disease inDrop", value = "fourDisease_indrop", tabUI_fourDisease_indrop("tab_fourDisease_indrop")),
      nav_panel("CLE GSE179633", value = "CLE_gse179633", tabUI_CLE_gse179633("tab_CLE_gse179633")),
      nav_panel("DM 10x", value = "DM_10x", tabUI_DM_10x("tab_DM_10x"))
    ),
    nav_menu(
      "Proteomics",
      nav_panel("Four disease NULISA", value = "fourDisease_nulisa", tabUI_fourDisease_nulisa("tab_fourDisease_nulisa")),
      nav_panel("UV in vitro OLINK", value = "UV_olink", tabUI_UV_olink("tab_UV_olink"))
    ),
    nav_menu(
      "Spatial Transcriptomics",
      nav_panel("Four disease seqFISH", value = "fourDisease_seqfish", tabUI_fourDisease_seqfish("tab_fourDisease_seqfish")),
      nav_panel("UV on CLE NL skin seqFISH", value = "UV_seqfish", tabUI_UV_seqfish("tab_UV_seqfish"))
    ),
    nav_menu(
      "bulk RNA-seq",
      nav_panel("UV in vitro bulk RNA-seq (moDC)", value = "UV_bulk_moDC", tabUI_UV_bulk_moDC("tab_UV_bulk_moDC")),
      nav_panel("UV in vitro bulk RNA-seq (FB)", value = "UV_bulk_FB", tabUI_UV_bulk_FB("tab_UV_bulk_FB"))
    ),
    nav_spacer(),
    nav_panel("Abbreviations", value = "abbreviation", tabUI_abbreviations("tab_abbreviation")),
    nav_panel("Data Access", value = "dataAccess", tabUI_dataAccess("tab_dataAccess")),
    nav_panel("Contact", value = "contact", tabUI_contact("tab_contact"))
  )
)

server <- function(input, output, session) {
  active_tab <- reactive(input$main_nav)
  data_path <- reactive({
    if (Sys.info()[[7]] == "root") {
      return("/home/app_data/")
    } else {
      return("/Users/yuqing/UMass Medical School Dropbox/Yuqing Wang/Ongoing/data_hosting/shinyApp_content/")
    }
  })

  tabServer_home("tab_home", data_path)
  transcript_data <- tabServer_fourDisease_indrop("tab_fourDisease_indrop", data_path, active_tab)
  tabServer_CLE_gse179633("tab_CLE_gse179633", data_path, active_tab)
  tabServer_DM_10x("tab_DM_10x", data_path, active_tab)
  tabServer_fourDisease_nulisa("tab_fourDisease_nulisa", data_path, active_tab)
  tabServer_UV_olink("tab_UV_olink", data_path, active_tab)
  tabServer_fourDisease_seqfish("tab_fourDisease_seqfish", data_path, active_tab)
  tabServer_UV_seqfish("tab_UV_seqfish", data_path, active_tab)
  tabServer_UV_bulk_moDC("tab_UV_bulk_moDC", data_path, active_tab)
  tabServer_UV_bulk_FB("tab_UV_bulk_FB", data_path, active_tab)
  tabServer_abbreviations("tab_abbreviation", data_path)
  tabServer_dataAccess("tab_dataAccess", data_path)
  tabServer_contact("tab_contact", data_path)
}

options(shiny.host = "0.0.0.0", shiny.port = 8888, shiny.reactlog = TRUE)
shinyApp(ui = ui, server = server)
