# modules/consistency_test/consistency_test_ui.R

consistency_test_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        actionButton(ns("do_consistency"), "进行一致性检验")
      ),
      mainPanel(
        verbatimTextOutput(ns("consistencyTest"))
      )
    )
  )
}