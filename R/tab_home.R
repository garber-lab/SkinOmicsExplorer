# Home tab UI helpers -----------------------------------------------------

#' Build a simple card body for a dataset summary
#'
#' @param title Human readable dataset name shown in the card header.
#' @param tagline Short phrase describing how the dataset can be used in the app.
#' @param tech Optional technology/platform callout rendered as muted text.
#'
#' @return A list of UI elements that can be wrapped with `bslib::card()`.
dataset_card_content <- function(title, tagline, specs=NULL, tech) {
  body <- list(tagline)
  if (!is.null(specs)) {
    if (length(specs) > 0 && !is.null(names(specs)) && all(nzchar(names(specs)))) {
      spec_items <- lapply(seq_along(specs), function(i) {
        spec_value <- specs[[i]]
        spec_text <- if (length(spec_value) > 1) paste(spec_value, collapse = ", ") else spec_value
        tags$small(
          class = "d-block lh-sm",
          tagList(tags$strong(names(specs)[i]), ": ", spec_text)
        )
      })
      spec_block <- do.call(
        tags$div,
        c(list(class = "mt-2 lh-sm"), spec_items)
      )
      body <- append(body, list(spec_block))
    } else {
      body <- append(body, list(tags$small(specs)))
    }
  }
  body <- append(body, list(tags$small(class = "text-muted", tech)))

  tagList(
    bslib::card_header(title),
    bslib::card_body(body)
  )
}

# Pre-defined dataset cards that can be dropped into the layout by ID.
tab_home_cards <- list(
  fourDisease_indrop = dataset_card_content(
    title = "Four disease inDrop",
    tagline = "All samples were processed under a uniform protocol, minimizing batch effects and enabling robust cross-disease comparisons.",
    specs = c(
      "Cohort composition" = "5 DM, 4 CLE, 5 psoriasis, 19 vitiligo, 11 healthy controls",
      "Sampling method" = "Suction blister",
      "Skin type" = "Lesional, non-lesional, healthy skin"
    ),
    tech = "scRNA-seq · inDrop"
  ),
  CLE_gse179633 = dataset_card_content(
    title = "CLE GSE179633 10x",
    tagline = "Re-clustering of a deeply sequenced public CLE dataset (GSE179633).",
    specs = c(
      "Cohort composition" = "CLE, DLE, HC", # !!! fill in the patient numbers
      "Sampling method" = "Punch biopsy",
      "Sample process" = "Separate dermis and epidermis",
      "Skin type" = "Lesional, non-lesional, healthy skin"
    ),
    tech = "scRNA-seq · 10x Genomics"
  ),
  DM_10x = dataset_card_content(
    title = "DM 10x",
    tagline = "Myeloid cells of three DM samples with high IFNβ.",
    specs = c(
      "Cohort composition" = "3 DM",
      "Sampling method" = "Punch biopsy",
      "Sample process" = "Separate dermis and epidermis", # !!! confirm
      "Skin type" = "Lesional, non-lesional, healthy skin" # !!! confirm
    ),
    tech = "scRNA-seq · 10x Genomics"
  ),
  fourDisease_nulisa = dataset_card_content(
    title = "Four disease NULISA",
    tagline = "Protein levels in interstitial fluid across four autoimmune skin diseases.",
    specs = c(
      "Cohort composition" = " DM,  CLE,  psoriasis,  vitiligo,  healthy controls", # !!! fill in the number
      "Sampling method" = "Suction blister fluid",
      "Skin type" = "Lesional, non-lesional, healthy skin",
      "Panel" = "Immune" # !!! confirm
    ),
    tech = "Proteomics · NULISA"
  ),
  UV_olink = dataset_card_content(
    title = "UV in vitro OLINK",
    tagline = "Proteins secreted into supernatant by UVB-irradiated keratinocytes (KCs) pretreated with or without IFNβ. Subsequently, dendritic cells (DCs) were incubated with KC supernatants, and proteins in DC supernatants were then measured.",
    specs = c(
      "Cell type" = "Primary keratinocytes, monocyte-derived DCs", # !!! confirm
      "Sampling method" = "Supernatant of cell culture"
    ),
    tech = "Proteomics · Olink"
  ),
  fourDisease_seqfish = dataset_card_content(
    title = "Four disease seqFISH",
    tagline = "Single-cell spatial transcriptomics of lesional skin from DM, CLE, psoriasis and vitiligo.",
    specs = c(
      "Cohort composition" = "2 DM, 2 CLE, 2 psoriasis, 1 vitiligo",
      "Sampling method" = "Punch biopsy",
      "Skin type" = "Lesional skin",
      "Imaging design" = "Two tissue sections per punch biopsy were imaged, except CLE sample 1"
    ),
    tech = "Spatial transcriptomics · seqFISH"
  ),
  UV_seqfish = dataset_card_content(
    title = "UV seqFISH",
    tagline = "UVB irradiation on CLE non-lesional (NL) skin with or without Anifrolumab (type I interferon receptor antagonist) treatment.",
    specs = c(
      "Cohort composition" = "1 CLE",
      "Original skin type" = "Non-lesional skin",
      "Sampled skin condition" = "NL, UVB-irradiated NL, UVB-irradiatied NL after Anifrolumab",
      "Sampling method" = "Punch biopsy",
      "Imaging design" = "Two tissue sections per punch biopsy were imaged" # !!! confirm
    ),
    tech = "Spatial transcriptomics · seqFISH"
  ),
  UV_bulk = dataset_card_content(
    title = "UV bulk RNA-seq",
    tagline = "Bulk RNA-seq of UVB-irradiated keratinocytes (KCs) pretreated with or without IFNβ. And bulk RNA-seq of dendritic cells (DCs) incubated with KC supernatants.",
    specs = c(
      "Cell type" = "Primary keratinocytes, monocyte-derived DCs", # !!! confirm
      "Sampling method" = "Cell pellet"
    ),
    tech = "Transcriptomics · bulk RNA-seq"
  )
)

# Home tab module ---------------------------------------------------------

tabUI_home <- function(id) {
  ns <- NS(id)

  tagList(
    bslib::card(
      bslib::card_header("Introduction"),
      bslib::card_body(tags$p("!!introduction to this Shiny app.")) # !!! fill in
    ),
    bslib::card(
      bslib::card_header("Publication"),
      bslib::card_body(tags$p("!!publication")) # !!! fill in
    ),
    accordion(
      accordion_panel(
        title = "Organize datasets by purpose",
        fluidRow(
          tags$h5("Four disease comparison"),
          layout_column_wrap(
            bslib::card(tab_home_cards[["fourDisease_indrop"]]),
            bslib::card(tab_home_cards[["fourDisease_nulisa"]]),
            bslib::card(tab_home_cards[["fourDisease_seqfish"]]),
            width = 1/2
          ),
          tags$h5(class = "mt-4", "Supplemental single-cell datasets"),
          layout_column_wrap(
            bslib::card(tab_home_cards[["CLE_gse179633"]]),
            bslib::card(tab_home_cards[["DM_10x"]]),
            width = 1/2
          ),
          tags$h5(class = "mt-4", "UV perturbation series"),
          layout_column_wrap(
            bslib::card(tab_home_cards[["UV_olink"]]),
            bslib::card(tab_home_cards[["UV_seqfish"]]),
            bslib::card(tab_home_cards[["UV_bulk"]]),
            width = 1/2
          )
        )
      ),
      accordion_panel(
        title = "Organize datasets by technology",
        fluidRow(
          tags$h5("scRNA-seq"),
          layout_column_wrap(
            bslib::card(tab_home_cards[["fourDisease_indrop"]]),
            bslib::card(tab_home_cards[["CLE_gse179633"]]),
            bslib::card(tab_home_cards[["DM_10x"]]),
            width = 1/2
          ),
          tags$h5(class = "mt-4", "Proteomics"),
          layout_column_wrap(
            bslib::card(tab_home_cards[["fourDisease_nulisa"]]),
            bslib::card(tab_home_cards[["UV_olink"]]),
            width = 1/2
          ),
          tags$h5(class = "mt-4", "Spatial transcriptomics"),
          layout_column_wrap(
            bslib::card(tab_home_cards[["fourDisease_seqfish"]]),
            bslib::card(tab_home_cards[["UV_seqfish"]]),
            width = 1/2
          ),
          tags$h5(class = "mt-4", "Bulk RNA-seq"),
          layout_column_wrap(
            bslib::card(tab_home_cards[["UV_bulk"]]),
            width = 1/2
          )
        )
      ),
      multiple = FALSE,
      open = NULL
    )
  )
}


tabServer_home <- function(id, data_path) {
  moduleServer(id, function(input, output, session) {
    invisible(NULL)
  })
}
