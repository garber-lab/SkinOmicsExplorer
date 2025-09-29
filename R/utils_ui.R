### inline output to save vertical space
inlineInput <- function(label, input, label_width = "100px", gap = "5px") {
  div(
    style = "display: flex; align-items: baseline;", # margin-bottom: 5px;
    tags$label(label, style = paste0("margin-right: ", gap, "; width:", label_width)), # margin-bottom: 0
    input
  )
}
# inlineInput("Plot width:",
#   numericInput("width", NULL, 6, min = 0.1, step = 0.1, width = 70)
# )


### group inputs. similar to wellPanel() but with no border
inputGroup <- function(..., margin_bottom = 0) {
  div(
    style = paste0("margin-bottom: ",margin_bottom,"px;"),  # adjust spacing once
    ...
  )
}
