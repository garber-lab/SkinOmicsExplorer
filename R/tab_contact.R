tabUI_contact <- function(id) {
  ns <- NS(id)

  placeholder_link <- function(label) {
    tags$a(
      href = "#",
      target = "_blank",
      rel = "noopener noreferrer",
      label
    )
  }

  contact_table <- tags$table(
    class = "table table-striped table-sm align-middle",
    tags$thead(
      tags$tr(
        tags$th("Responsibility"),
        tags$th("Contact"),
        tags$th("Use this for")
      )
    ),
    tags$tbody(
      tags$tr(
        tags$td(tags$strong("Website and data analysis")),
        tags$td(
          tagList(
            "Yuqing Wang",
            tags$br(),
            tags$a(
              href = "mailto:yuqingwang1995@gmail.com",
              "yuqingwang1995@gmail.com"
            )
          )
        ),
        tags$td("Bug reports, feature requests, and questions about analyses or visualizations.")
      ),
      tags$tr(
        tags$td(tags$strong("Hosting and deployment")),
        tags$td(
          tagList(
            "ViaFoundry",
            tags$br(),
            placeholder_link("Company link (add URL)")
          )
        ),
        tags$td("Availability, hosting questions, and deployment support.")
      ),
      tags$tr(
        tags$td(tags$strong("Computational data analysis")),
        tags$td(
          tagList(
            "Manuel Garber lab, UMass Chan",
            tags$br(),
            placeholder_link("Lab page (add URL)")
          )
        ),
        tags$td("Analysis methods, computational pipelines, and data interpretation questions.")
      ),
      tags$tr(
        tags$td(tags$strong("Patient recruitment and wet lab work")),
        tags$td(
          tagList(
            "Mehdi Rashighi lab, UMass Chan",
            tags$br(),
            placeholder_link("Contact info (add URL)")
          )
        ),
        tags$td("Patient recruitment, sample acquisition, and wet lab experiment questions.")
      )
    )
  )

  tagList(
    bslib::card(
      bslib::card_header("Contact and credits"),
      bslib::card_body(
        tags$p("Contact the appropriate group based on the type of question."),
        tags$p(
          "This paper reflects a collaboration between the Manuel Garber and Mehdi Rashighi labs at UMass Chan.",
          " The Garber lab led computational data analysis, while the Rashighi lab led patient recruitment, sample acquisition,",
          " and wet lab experiments."
        ),
        tags$p(
          tags$strong("Website author:"),
          " Yuqing Wang (",
          tags$a(
            href = "mailto:yuqingwang1995@gmail.com",
            "yuqingwang1995@gmail.com"
          ),
          ")."
        ),
        contact_table
      )
    )
  )
}

tabServer_contact <- function(id, data_path) {
  moduleServer(id, function(input, output, session) {
    invisible(NULL)
  })
}
